open Maps
open Trie

let next_char pos s =
  let d = String.get_utf_8_uchar s pos in
  let len = Uchar.utf_decode_length d in
  let u = Uchar.utf_decode_uchar d in
  pos + len, u


let rec query_path result query st pos len path trie =
  if pos >= len then query_trie result query st trie
  else
    let pos, u = next_char pos path in
    match Automaton.transition query st u with
    | None -> result
    | Some st -> query_path result query st pos len path trie

and query_trie result query st trie =
  let result = match trie.leaf with
    | None -> result
    | Some x ->
      let accepted = Automaton.accepted_state query (Some st) in
      if accepted then x :: result
      else result
  in
  query_children result query st trie.strict_suffixes

and query_children result query st children =
  Uchar_map.fold (fun _ {path;subtree} result ->
      query_path result query st 0 (String.length path) path subtree
    ) children result


type search_data = {
  dictionary: int Trie.t;
  automatons: Automaton.t Automaton.Profile_map.t Dynarray.t
}

let get_pmap d pos =
  let len = Dynarray.length d in
  if pos < len then Dynarray.get d pos
  else
    begin
      for _ = len to pos do
        Dynarray.add_last d Automaton.Profile_map.empty
      done;
      Automaton.Profile_map.empty
    end

let create_query emax automaton_register word =
  let n_chars, char_map, p, word = Automaton.profile word in
  let automaton =
    let pmap = get_pmap automaton_register emax in
    match Automaton.Profile_map.find_opt p pmap with
    | None ->
      let automaton = Automaton.create emax n_chars p in
      let pmap = Automaton.Profile_map.add p automaton pmap in
      Dynarray.set automaton_register emax pmap;
      automaton
    | Some a -> a
  in
  { Automaton.char_map; word;  automaton }

let rec query ~cutoff search name distance () =
  if distance > cutoff then Seq.Nil
  else
    let q = create_query distance search.automatons name in
    match query_trie [] q 0 search.dictionary with
    | [] -> query ~cutoff search name (distance+1) ()
    | left_candidates ->
      let layer = { Model.left_candidates; Model.pref = distance } in
      Seq.Cons(layer, (query ~cutoff search name (distance+1)))


let preference_layers ~cutoff search name =
  query ~cutoff:(cutoff name) search name 1

  let preferences ?max_elements:_ ~cutoff d =
    let d = Seq.mapi (fun i item -> Item.name item, i) (Array.to_seq d) in
    let dictionary = Trie.of_seq d in
    let search = { dictionary; automatons = Dynarray.create ()} in
    preference_layers ~cutoff search
