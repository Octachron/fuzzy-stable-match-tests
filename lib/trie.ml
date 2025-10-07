open Maps

type 'data t = {
    leaf : 'data option;
    strict_suffixes : 'data path Uchar_map.t;
  }
  and 'data path = { path:string; subtree: 'data t }


  let empty = { leaf = None; strict_suffixes = Uchar_map.empty }
  let leaf x = { leaf = Some x; strict_suffixes = Uchar_map.empty}

  type diff =
    | At_pos of {offset:int; l:Uchar.t option;  r:Uchar.t }
    | Same of int

  let rec find_difference ~llen ~lpos l ~rlen ~offset r =
    match lpos = llen, offset = rlen with
    | true, true -> Same lpos
    | true, false ->
        let r = Uchar.utf_decode_uchar (String.get_utf_8_uchar r offset) in
        At_pos { offset; l = None; r }
    | false, true -> Same lpos
    | false, false ->
        let ld = String.get_utf_8_uchar l lpos in
        let rd = String.get_utf_8_uchar r offset in
        if ld <> rd then
          let l = Some (Uchar.utf_decode_uchar ld) in
          At_pos { offset; l; r = Uchar.utf_decode_uchar rd }
        else
          let diff = Uchar.utf_decode_length ld in
          find_difference
            ~llen ~lpos:(lpos+diff) l
            ~rlen ~offset:(offset+diff) r

  let comma ppf () = Format.fprintf ppf ",@ "
  let pp_uchar ppf u =
    let u =
      let b = Buffer.create 2 in Buffer.add_utf_8_uchar b u; Buffer.contents b
    in
    Format.pp_print_string ppf u

  let[@warning "-32"] rec pp_trie pp_leaf ppf m =
    let pp_oleaf ppf = function
      | None -> Format.fprintf ppf "()"
      | Some leaf -> Format.fprintf ppf "(%a)" pp_leaf leaf
    in
    Format.fprintf ppf "@[<v 2>%a [%a]@]" pp_oleaf m.leaf
      (Format.pp_print_seq ~pp_sep:comma @@ pp_binding pp_leaf)
      (Uchar_map.to_seq m.strict_suffixes)
  and[@warning "-32"] pp_binding pp_leaf ppf (u,p) =
    Format.fprintf ppf "(%a)%s %a" pp_uchar u p.path (pp_trie pp_leaf) p.subtree


  let add (s,k) trie =
    let rec aux k len pos s trie =
      if pos = len then { trie with leaf = Some k } else
        let decode = String.get_utf_8_uchar s pos in
        let char = Uchar.utf_decode_uchar decode in
        let diff = Uchar.utf_decode_length decode in
        let new_sub = match Uchar_map.find char trie.strict_suffixes with
          | exception Not_found -> {
              path = String.sub s pos (String.length s - pos);
              subtree = leaf k
            }
          | { path; subtree } ->
              match find_difference ~llen:len ~lpos:(pos+diff) s
                      ~rlen:(String.length path) ~offset:diff path
              with
              | Same lpos -> { path; subtree = aux k len lpos s subtree }
              | At_pos {offset;l;r} ->
                  let common = String.sub path 0 offset in
                  let right =
                    let path =
                      String.sub path offset (String.length path - offset)
                    in
                    Uchar_map.singleton r { path ; subtree }
                  in
                  let subtree = match l with
                    | None -> { leaf = Some k; strict_suffixes = right }
                    | Some l ->
                        let path =
                          let pos = pos + offset in
                          String.sub s pos (String.length s - pos)
                        in
                        let strict_suffixes =
                          Uchar_map.add l {path; subtree=leaf k} right
                        in
                        { leaf = None; strict_suffixes }
                  in
                  { path = common; subtree }
        in
        let strict_suffixes = Uchar_map.add char new_sub trie.strict_suffixes in
        { trie with strict_suffixes }
    in
    aux k (String.length s) 0 s trie

  let of_seq s = Seq.fold_left (fun t x -> add x t) empty s

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
