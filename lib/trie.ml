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
