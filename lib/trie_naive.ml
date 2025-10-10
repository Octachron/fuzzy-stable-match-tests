open Trie
type column = { char:Uchar.t; score: int Array.t }

type frontier = { col_minus_1:column; col:column }
let copy c = { c with score = Array.copy c.score }

type 'a state = {
  matrix:frontier;
  best_score:int;
  trie: 'a t
}

module QState = Pqueue.MakeMinPoly(struct
    type 'a t = 'a state
    let compare x y = Int.compare x.best_score y.best_score
  end)


let score st = st.matrix.col.score.(Array.length st.matrix.col.score - 1)
let best_future_score x = Array.fold_left min Int.max_int x.col.score


let add_leaf result_map state =
  match state.trie.leaf with
  | Some index -> Int_map.add_to_list (score state) index result_map
  | None -> result_map

let next_char pos s =
  let d = String.get_utf_8_uchar s pos in
  let len = Uchar.utf_decode_length d in
  let u = Uchar.utf_decode_uchar d in
  pos + len, u

let rec col_edit_distance ~(left:Uchar.t array) ~rlen ~rpos ~(right:String.t)
    ~col_minus_1
    ~col
  =
  if rpos >= rlen then { col_minus_1; col } else
    let rpos, rchar = next_char rpos right in
    let l = Array.length col.score in
    for i = l - 1 downto 0 do
      let addition = col.score.(i) + 1 in
      let subst =
        if i = 0 then addition
        else if rchar = left.(i-1) then min addition col.score.(i-1)
        else Int.min addition (1 + col.score.(i-1)) in
      let transpose =
        if i >=2 && rchar = left.(i-2) && col.char = left.(i-1) then
          Int.min subst (1 + col_minus_1.score.(i-2))
        else
          subst
      in
      col_minus_1.score.(i) <- transpose
    done;
    for i = 1 to l - 1 do
      col_minus_1.score.(i) <-
        Int.min col_minus_1.score.(i) (1+col_minus_1.score.(i-1))
    done;
    let new_col = { score = col_minus_1.score; char = rchar } in
    col_edit_distance ~left ~rlen ~rpos ~right
      ~col:new_col ~col_minus_1:col


let rec query_next score word result_map queue =
  match QState.min_elt queue with
  | None -> result_map
  | Some st ->
    if st.best_score > score then result_map
    else begin
      QState.remove_min queue;
      query_children score word result_map queue st st.trie.strict_suffixes
    end
and query_children score word result_map queue st children =
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
