(** An implementation (in [diff]) of Zoltan Kiraly's "New Algorithm," presented
    in "Linear Time Local Approximation Algorithm for Maximum Stable Marriage":
    https://www.mdpi.com/1999-4893/6/3/471. It computes a 3/2-approximation of
    a maximum stable marriage in linear time (linear in the sum of the lengths
    of the preference lists). *)


  (* This implementation does not use the same semantics as the original paper.
     Below is a conversion from the paper's terms to the implementation's terms:
     - woman: left
     - man: right
     - engaged (woman / man): paired
     - maiden (woman): unpaired
     - active (man): active
     - lad: first phase
     - bachelor: second phase
     - old bachelor: closed
     - uncertain (man): has other choices
     - flighty (woman): has a weak pair *)

  type distance = int

  type dequeue = { front: int list; pre:bool; back:int list }
  let next dq = match dq.front with
    | a :: front -> Some (a, { dq with front })
    | [] -> match List.rev dq.back with
      | [] -> None
      | a :: front -> Some (a, { front; pre=false; back = [] })
  let push_back dq x = { dq with back = x :: dq.back }

  let replace_front x dq = { dq with front = x :: dq.front }



  type left_state =
    | Left_unpaired
    | Left_paired of int * distance

  type right_phase =
    | First
    | Second

  type active_right_state = {
    mutable previous_layers : Model.layer list;
        (** Invariant: this list is not empty in the first phase . *)
    mutable current_layer : dequeue; (** Invariant: this list is not empty. *)
    mutable current_distance : distance;
    mutable paired: bool;
    mutable phase: right_phase;
    mutable next_layers : Model.layer Seq.t;
  }

  type ('a,'b) state =
    { left: 'a array; right: 'b array; mutable reactivated:int list }

  let is_never_paired state j = match state.left.(j) with
    | Left_unpaired -> true
    | _ -> false

  let rec has_alternative_choices state r =
    let cl = r.current_layer in
    if not cl.pre then false else
    match cl.front with
    | a :: b :: q ->
       is_never_paired state b ||
          let current_layer =
            { front = a :: q; pre=true; back = b :: cl.back }
          in
          r.current_layer <- current_layer;
          has_alternative_choices state r
    | [_] | [] -> false

  let rec skip_paired state dq =
    assert dq.pre;
    match next dq with
    | None -> assert false
    | Some (first,others) ->
        if is_never_paired state first then
          first, others
        else skip_paired state (push_back others first)

  let has_weak_pair state j =
    match state.left.(j) with
    | Left_unpaired -> false
    | Left_paired (i, _) ->
        match state.right.(i) with
        | None -> assert false
        | Some r -> has_alternative_choices state r

  let phase state i =
    Option.map (fun x -> x.phase) state.right.(i)

  let prepare_dequeue state { Model.left_candidates=i; pref=d} r =
    let pre, later = List.partition (is_never_paired state) i in
    let dequeue = { front = pre; pre=true; back = later } in
    match state.right.(r) with
    | None -> ()
    | Some r ->
        r.current_distance <- d;
        r.current_layer <- dequeue

  let second_phase state ir r =
    let layers = List.rev r.previous_layers in
    r.previous_layers <- [];
    r.phase <- Second;
    match layers with
    | [] -> assert false
    | layer :: q ->
        prepare_dequeue state layer ir;
        r.next_layers <- List.to_seq q


  let next_layer state ir r = match r.next_layers () with
    | Seq.Nil ->
        begin match r.phase with
        | First -> second_phase state ir r; true
        | Second -> false
        end
    | Seq.Cons(layer, next_layers) ->
        r.previous_layers <- layer :: r.previous_layers;
        r.next_layers <- next_layers;
        prepare_dequeue state layer ir;
        true

  let rec get_left_candidate state ir r =
    assert (r.paired = false);
    if has_alternative_choices state r then
      let f, others = skip_paired state r.current_layer in
      r.current_layer <- others;
      Some f
    else match next r.current_layer with
      | Some (f,others) ->
          r.current_layer <- others;
          Some f
      | None ->
          if next_layer state ir r then get_left_candidate state ir r
          else None

  let rec get_compatible_left_candidate compatibility state ir r =
    match get_left_candidate state ir r with
    | None -> None
    | Some l as c ->
        if compatibility l ir then c
        else
          get_compatible_left_candidate compatibility state ir r

  let reject state i =
    match state.right.(i) with
    | None -> ()
    | Some right ->
    right.paired <- false;
    match next right.current_layer with
    | None -> state.right.(i) <- None
    | Some (f,others) ->
        let dequeue = if others.pre then
            push_back others f
          else others
        in
        right.current_layer <- dequeue;
        state.reactivated <- i :: state.reactivated

  let accepted_proposal state i j d =
    has_weak_pair state j ||
    match state.left.(j) with
    | Left_unpaired -> true
    | Left_paired (i', d') ->
        d < d' ||
        d = d' &&
        match phase state i, phase state i' with
        | Some Second, Some First -> true
        | _ -> false

  let pair state i j d =
    begin match state.right.(i) with
    | None -> ()
    | Some r ->
      r.paired <- true;
      r.current_layer <- replace_front j r.current_layer
    end;
    match state.left.(j) with
    | Left_unpaired -> state.left.(j) <- Left_paired (i, d)
    | Left_paired (i', _) ->
        reject state i';
        state.left.(j) <- Left_paired (i, d)

  let trie_preferences ?max_elements ~cutoff x =
    let name i field = Item.name field, i in
    let left_trie = x |> Array.to_seq |> Seq.mapi name |> Trie.of_seq in
    fun name ->
      Trie_naive.compute_preference_layers
        ~cutoff:(cutoff name)
        ?max_elements
        left_trie
        name

  let d_of_list front = { front; pre=true; back = [] }

  let init_right_state ~preferences right =
    Array.map
      (fun right_field ->
         let name = Item.name right_field in
         let sequence = preferences name in
         match sequence () with
         | Seq.Nil -> None
         | Seq.Cons (layer, tail) ->
             Some {
               paired = false;
               phase = First;
               current_distance = layer.pref;
               current_layer = d_of_list layer.left_candidates;
               previous_layers = [layer];
               next_layers = tail;
             }
      )
      right


  let rec proposals compatibility state i right =
    match get_compatible_left_candidate compatibility state i right with
    | None -> ()
    | Some j ->
        if accepted_proposal state i j right.current_distance then
          pair state i j right.current_distance
        else
          proposals compatibility state i right


  let diff ~preferences ~compatibility left right =
    let n = Array.length left in
    let m = Array.length right in
    let left_state = Array.make n Left_unpaired in
    let right_state = init_right_state ~preferences right in
    let state = { left=left_state; reactivated = []; right=right_state } in
    let rec loop = function
      | [] ->
          begin  match state.reactivated with
          | [] -> ()
          | l -> state.reactivated <- []; loop l
          end
      | i :: l ->
        match state.right.(i) with
          | None -> loop l
          | Some right ->
              proposals compatibility state i right;
              loop l
    in
    loop (List.init m Fun.id);
    let left_final = Seq.zip (Array.to_seq left) (Array.to_seq state.left) in
    let left, pairs = Seq.partition_map (fun (field, status) ->
        match status with
        | Left_unpaired -> Either.Left field
        | Left_paired (i,_) ->
            Either.Right (field, right.(i))
      ) left_final
    in
    {
      Matches.left = List.of_seq left;
      right =
        Array.to_seq right
        |> Seq.filteri (fun i _ ->
          match state.right.(i) with
          | Some r -> not r.paired
          | None -> true)
        |> List.of_seq
      ;
      pairs = List.of_seq pairs;
    }

  let diff ~compatibility ~preferences left right =
    if Array.length right >  Array.length left then
      diff
        ~preferences:(preferences right)
        ~compatibility:(fun a b -> compatibility b a)
        right left
      |> Matches.reverse
    else diff ~preferences:(preferences left) ~compatibility left right
