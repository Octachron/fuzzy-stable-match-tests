open Fuzzy

type bk_variant =
  | Hashmap
  | Dynarray
  | Exact
  | Map

type implem =
  | Trie
  | Trie_A
  | BK_tree of bk_variant
  | Del_dict
  | Del_dict_hm
  | Simple

let methods = [ "trie"; "trie+a"; "bk"; "simple"; "bk+hm"; "bk+da"; "bk+e"; "dd"; "ddh"]
let parse_implem = function
    | "trie" -> Trie
    | "trie+a" -> Trie_A
    | "bk" -> BK_tree Map
    | "dd" -> Del_dict
    | "ddh" -> Del_dict_hm
    | "simple" -> Simple
    | "bk+hm" -> BK_tree Hashmap
    | "bk+da" -> BK_tree Dynarray
    | "bk+e" -> BK_tree Exact
    | _ -> BK_tree Map

let to_list f ?max_elements ~cutoff left = f ?max_elements ~cutoff (Array.to_list left)

let implem = function
  | Trie -> Stable_marriage.trie_preferences ?max_elements:None
  | Trie_A -> Trie_and_automaton.preferences ?max_elements:None
  | Del_dict -> Del_dict.preferences ?max_elements:None
  | Del_dict_hm -> Del_dict_hm.preferences ?max_elements:None
  | BK_tree Map -> to_list Bk.preferences ?max_elements:None
  | BK_tree Hashmap -> to_list Bk_hm.preferences ?max_elements:None
  | BK_tree Dynarray -> to_list Bk_da.preferences ?max_elements:None
  | BK_tree Exact -> Bk_dl.preferences ?max_elements:None
  | Simple -> Simple.preferences ?max_elements:None


let fuzzy_match_names ~implem ~compatibility left right =
  (* The edit distance between an existing name and a suggested rename must be
     at most half the length of the name. *)
  let cutoff name =
    match String.length name with
    | 0 | 1 -> 0
    | 2 | 3 | 4 -> 1
    | 5 | 6 | 7 | 8 -> 2
    | _ -> 3
  in
    let matches =
      let preferences = implem ~cutoff in
      Stable_marriage.diff
        ~preferences ~compatibility
        left
        right
    in
    let pairs = List.map (fun (x,y) -> Item.(item x, item y)) matches.pairs in
    { matches with pairs }


let meth = ref Simple
let parse_meth s = meth := parse_implem s

let size = ref 1_000
let mut = ref 10
let show = ref false
let seed = ref 0

let args = [
  "-m", Arg.Symbol (methods, parse_meth), "implementations to benchmark";
  "-mutations", Arg.Set_int mut, "number of mutations";
  "-size", Arg.Set_int size, "size of simulated module";
  "-show", Arg.Set show, "show pairs";
  "-seed", Arg.Set_int seed, "PRNG seed"
]

let item (name,ty) = { Item.name; item = (name,ty); kind = ty }

let comma ppf () = Format.fprintf ppf ",@ "
let list pr ppf x = Format.pp_print_list ~pp_sep:comma pr ppf x
let name ppf x = Format.pp_print_string ppf (Item.name x)
let names = list name
let pair ppf (x,y) = Format.fprintf ppf "%s<->%s" (fst x) (fst y)
let pairs = list pair

let () =
  Arg.parse args ignore "bench -m trie";
  let implem = implem !meth in
  let size = !size in
  let mut = !mut in
  let () = Random.init !seed in
  let left, right = Gen.process ~mut ~size in
  let left = Array.map item left and right = Array.map item right in
  if !show then
    Format.eprintf "@[<v 2>left:@ @[%a@]@;<0 -2>right:@,@[%a@]@,@]@."
      names (Array.to_list left) names (Array.to_list right);
  let compatibility _x _y = true in
  let matches = fuzzy_match_names ~compatibility ~implem left right in
  Format.printf "%d left elements unpaired, %d paired, %d unpaired right elements@."
    (List.length matches.left)  (List.length matches.pairs) (List.length matches.right);
  if !show then
    Format.eprintf "@[<v 2>left:@ @[%a@]@;<0 -2>pairs:@,@[%a@]@;<0 -2>right:@,@[%a@]@]@."
      names matches.left pairs matches.pairs names matches.right
