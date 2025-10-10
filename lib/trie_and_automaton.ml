open Maps
open Trie

let next_char pos s =
  let d = String.get_utf_8_uchar s pos in
  let len = Uchar.utf_decode_length d in
  let u = Uchar.utf_decode_uchar d in
  pos + len, u

let rec query_children score word result_map queue st children =
  if Uchar_map.is_empty children then
    query_next score word result_map queue
  else
    let u, first = Uchar_map.choose children in
    let rest = Uchar_map.remove u children in
    let trie = { st.trie with strict_suffixes = rest } in
    let st = { st with trie } in
    let () = QState.add queue st in
    query_path score word result_map queue st first

and query_path score word result_map queue st p =
  let cols = col_edit_distance
      ~left:word ~rlen:(String.length p.path) ~right:p.path ~rpos:0
      ~col_minus_1:(copy st.matrix.col_minus_1)
      ~col:(copy st.matrix.col)
  in
  let best_score = best_future_score cols in
  let st = { matrix=cols; best_score; trie = p.subtree } in
  let result_map = add_leaf result_map st in
  if best_score <= score then
    query_children score word result_map queue st st.trie.strict_suffixes
  else
    let () = QState.add queue st in
    query_next score word result_map queue

let uchar_array word =
  let d = Dynarray.create () in
  let pos = ref 0 in
  let len = String.length word in
  while !pos < len do
    let npos, char = next_char !pos word in
    Dynarray.add_last d char;
    pos := npos
  done;
  Dynarray.to_array d

let rec query rmap cutoff queue name () =
  let next_score =
    match QState.min_elt queue, Int_map.choose_opt rmap with
    | None, None -> None
    | Some {best_score=s; _ }, None | None, Some (s,_) -> Some s
    | Some q, Some (m,_) -> Some (min q.best_score m)
  in
  match next_score with
  | None -> Seq.Nil
  | Some layer ->
    if layer > cutoff then
      Seq.Nil
    else
      let rmap = query_next layer name rmap queue in
      match Int_map.find_opt layer rmap with
      | None | Some [] -> query rmap cutoff queue name ()
      | Some left_candidates ->
        let rmap = Int_map.remove layer rmap in
        let pref_layer = { Model.left_candidates; pref = layer } in
        Seq.Cons(pref_layer, query rmap cutoff queue name)

let init name trie =
  let queue = QState.create () in
  let len = Array.length name + 1 in
  let col_minus_1 =
    { char = Uchar.rep; score = Array.make len Int.max_int } in
  let col = { char = Uchar.rep; score = Array.init len Fun.id } in
  QState.add queue {
    best_score=0;
    trie;
    matrix = { col; col_minus_1 }
  };
  queue


let compute_preference_layers ?max_elements:_ ~cutoff trie name =
  let uchar_name = uchar_array name in
  query Int_map.empty cutoff (init uchar_name trie) uchar_name
