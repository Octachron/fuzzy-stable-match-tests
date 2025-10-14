module Int_set = Set.Make(Int)

let next_char pos s =
  let d = String.get_utf_8_uchar s pos in
  let len = Uchar.utf_decode_length d in
  let u = Uchar.utf_decode_uchar d in
  pos + len, u

let rec one_deletions b ~len ~pos s () =
  if pos >= len then Seq.Nil
  else
    let pos, u = next_char pos s in
    let n = (Buffer.contents b) ^ (String.sub s pos (len-pos)) in
    Buffer.add_utf_8_uchar b u;
    Seq.Cons(n, one_deletions b ~len ~pos s)


let one_deletions s () =
  one_deletions (Buffer.create 10) ~len:(String.length s) ~pos:0 s ()

let rec many_deletions prefix last ~dels ~len ~pos s () =
  if pos >= len then Seq.Nil
  else if pos + dels = len then
    let current = String.sub s last (len-dels-last) :: prefix in
    Seq.singleton (String.concat "" (List.rev current)) ()
  else if dels = 0 then
    let current = String.sub s last (len-last) :: prefix in
    Seq.singleton (String.concat "" (List.rev current)) ()
  else
    let pos', _ = next_char pos s in
    let prefix' = String.sub s last (pos - last) :: prefix in
    Seq.append
      (many_deletions prefix' pos' ~dels:(dels-1) ~len ~pos:pos' s)
      (many_deletions prefix last ~dels ~len ~pos:pos' s)
      ()

let many_deletions ~dels s () =
  many_deletions [] 0 ~dels ~len:(String.length s) ~pos:0 s ()


let prefix pmax s =
  if String.length s < pmax then s
  else String.sub s 0 pmax

let prefix s = prefix 7 s

let pure words = Hashtbl.of_seq @@ Seq.mapi (fun i w -> prefix w, Int_set.singleton i) words

let add_word_indices dict (dword,indices) =
  let new_set =
    match Hashtbl.find_opt dict dword with
    | None -> indices
    | Some s -> Int_set.union indices s
  in
  Hashtbl.replace dict dword new_set

let make del_dict =
  let new_dict = Hashtbl.create 100 in
  Hashtbl.iter (fun wd index_set ->
      let s = Seq.map (fun dw -> dw, index_set) @@ one_deletions wd in
      Seq.iter (add_word_indices new_dict) s
    ) del_dict;
  new_dict


let query_dword words dist dict word =
  Seq.concat_map (fun dword ->
    match Hashtbl.find_opt dict dword with
    | None -> Seq.empty
    | Some set ->
      Seq.filter (fun i -> String.edit_distance words.(i) word = dist)
        (Int_set.to_seq set)
  ) (many_deletions ~dels:dist @@ prefix word)

type search_data = {
  words: string array;
  del_dictionaries: (string, Int_set.t) Hashtbl.t Dynarray.t
}

let get_del_dict {del_dictionaries;_} dist =
  let len = Dynarray.length del_dictionaries in
  if dist < len then
    Dynarray.get del_dictionaries dist
  else if len = 0 then assert false
  else begin
    for i = len to dist do
      let old = Dynarray.get del_dictionaries (i-1) in
      Dynarray.add_last del_dictionaries (make old);
    done;
    Dynarray.get del_dictionaries dist
  end

let query search_data dist word =
  query_dword search_data.words dist (get_del_dict search_data dist) word


let rec layers search_data ~cutoff dist word () =
  if dist > cutoff then Seq.Nil
  else
    let s = query search_data dist word in
    match List.of_seq s with
    | [] -> layers search_data ~cutoff (dist+1) word ()
    | left_candidates ->
      Seq.Cons({Model.left_candidates; pref=dist}, layers search_data ~cutoff (dist+1) word)

let preferences ?max_elements:_ ~cutoff words =
  let words = Array.map Item.name words in
  let del_dictionaries = Dynarray.make 1 (pure @@ Array.to_seq words) in
  let search_data = { words; del_dictionaries } in
  fun word -> layers search_data ~cutoff:(cutoff word) 1 word
