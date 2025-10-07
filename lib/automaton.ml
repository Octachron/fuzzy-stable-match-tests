open Maps


(* For a position i^e (i character read, e-error ), a valid state contains only
   one position by diagonal i-e *)
type diagonal = { read_minus_error:int; error:int }
type state = diagonal list

let read diag = diag.read_minus_error + diag.error


(* i^d subsumes the cone rooted in this position   *)
let subsumed diag1 diag2 =
  let read1 = read diag1 and read2 = read diag2 in
  if read1 >= read2 then false
  else abs (diag1.error - diag2.error) <= read2 - read1

let rec add prev_diags diag = function
  | [] -> List.rev prev_diags
  | a :: q as l  ->
      if a.read_minus_error = diag.read_minus_error then
          List.rev_append prev_diags
            (if diag.error < a.error then diag ::q else l)
      else if a.read_minus_error < diag.read_minus_error then
        begin if subsumed a diag then
            add prev_diags diag q
          else add (a::prev_diags) diag q
        end
      else
        List.rev_append prev_diags (diag::l)

let add x l = add [] x l

let map f l = List.fold_left (fun m p ->
    List.fold_left (fun m x -> add x m) m (f p)
  ) [] l

let rec profile last map uword p pos s =
  if pos >= String.length s then
    (map, Dynarray.to_array p, Dynarray.to_array uword)
  else
    let decode = String.get_utf_8_uchar s pos in
    let char = Uchar.utf_decode_uchar decode in
    Dynarray.add_last uword char;
    let l = Uchar.utf_decode_length decode in
    match Uchar_map.find_opt char map with
    | None ->
        let map = Uchar_map.add char last map in
        let () = Dynarray.add_last p last in
        profile (last+1) map uword p (pos+l) s
    | Some c ->
        let () = Dynarray.add_last p c in
        profile last map uword p (pos+l) s

let profile s = profile 0 Uchar_map.empty (Dynarray.create ()) (Dynarray.create ()) 0 s

let base_error ~emax ~rmax { read_minus_error = re; error = e } =
  let error = e + 1 in
  if error > emax then []
  else if re + error > rmax then
    [ { error; read_minus_error = re - 1 }]
  else
    [ {error; read_minus_error = re}; { error; read_minus_error = re - 1}]

let transition_other ~emax ~rmax m = map (base_error ~emax ~rmax) m

let rec index nchar pos a =
  if pos >= Array.length a then None
  else if a.(pos) = nchar then
    Some pos
  else index (nchar) (pos + 1) a

let transition_nchar ~emax profile nchar m =
  let rmax = Array.length profile in
  let f d =
    let r = read d in
    if profile.(r) = nchar then begin
      if r >= rmax then []
      else [ { read_minus_error = d.read_minus_error + 1; error = d.error} ]
    end else
      let base = base_error ~emax ~rmax d in
      match index nchar (r+1) profile with
      | None -> base
      | Some j ->
          let diff = j - r in
          let error = d.error + diff in
          if error > emax then base
          else
          { read_minus_error = d.read_minus_error + 1 ; error } :: base
  in
  map f m

  module State_map = Map.Make(struct
      type t = diagonal list
      let compare (x:t) (y:t) = compare x y
    end)

  let accept ~emax ~rmax state =
    let accept_diag = rmax - emax in
    List.exists (fun { read_minus_error; _} ->
        read_minus_error >= accept_diag
      ) state

  type automaton = {
    profile: int array;
    max_error:int;
    transitions: int Int_map.t Dynarray.t;
    states: (diagonal list * bool) Dynarray.t;
    mutable rev_map: int State_map.t
  }


  let zero_diag = { read_minus_error=0; error = 0;}

  let create max_error profile =
    let states = Dynarray.create () in
    let zero_state = [zero_diag] in
    Dynarray.add_last states (zero_state, Array.length profile <= max_error) ;
    let rev_map = State_map.of_list [ [], -1 ; zero_state,  0 ] in
    let transitions = Dynarray.create () in
    Dynarray.add_last transitions Int_map.empty;
    {
      profile;
      max_error;
      states;
      transitions;
      rev_map
    }


  let add_transition automaton state nchar =
    let full_state, _ = Dynarray.get automaton.states state in
    let emax = automaton.max_error in
    let new_full_state =
      if nchar < 0 then
        transition_other ~emax ~rmax:(Array.length automaton.profile) full_state
      else
        transition_nchar ~emax:automaton.max_error automaton.profile nchar full_state
    in
    let new_state =
      match State_map.find_opt new_full_state automaton.rev_map with
      | Some i -> i
      | None ->
        let new_state = Dynarray.length automaton.states in
        Dynarray.add_last automaton.states
          (new_full_state, accept ~emax ~rmax:(Array.length automaton.profile) new_full_state);
        automaton.rev_map <- State_map.add new_full_state new_state automaton.rev_map;
        Dynarray.add_last automaton.transitions Int_map.empty;
        new_state
    in
    let transitions = Dynarray.get automaton.transitions new_state in
    Dynarray.set automaton.transitions new_state (Int_map.add nchar new_state transitions);
    new_state

  type query = {
    word: Uchar.t array;
    char_map: int Uchar_map.t;
    automaton: automaton;
  }

  let transition query state uchar =
    let nchar =
      match Uchar_map.find_opt uchar query.char_map with
      | None -> -1
      | Some x -> x
    in
    let t = Dynarray.get query.automaton.transitions state in
    let state = match Int_map.find_opt nchar t with
      | Some e -> e
      | None -> add_transition query.automaton state nchar
    in
    if state < 0 then None else Some state

  let rec transition_suffix query state pos len s =
    if pos >= len then Some state else
    let decode = String.get_utf_8_uchar s pos in
    let uchar = Uchar.utf_decode_uchar decode in
    match transition query state uchar with
    | None -> None
    | Some state ->
        let pos = pos + Uchar.utf_decode_length decode in
        transition_suffix query state pos len s

  module Profile_map = Map.Make(struct
    type t = int array
    let compare (x:t) (y:t) = compare x y
  end)

  let create_query emax automaton_register word =
    let char_map, p, word = profile word in
    let automaton =
      match Profile_map.find_opt p !automaton_register with
      | None ->
          let automaton = create emax p in
          automaton_register := Profile_map.add p automaton !automaton_register;
          automaton
      | Some a -> a
     in
     { char_map; word;  automaton }


  let accept emax = fun word s ->
    let r = ref (Profile_map.empty) in
    let query = create_query emax r word in
    transition_suffix query 0 0 (String.length s) s
