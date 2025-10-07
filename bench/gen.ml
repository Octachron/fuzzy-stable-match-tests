type ty =
  | Char
  | Int
  | Float
  | String

type mutation =
  | Duplicate
  | Delete
  | Single
  | Mixture
  | Change_type

type _ Effect.t +=
  | Random_int: int -> int Effect.t
  | Random_name: string Effect.t
  | Str_mutation: mutation Effect.t
  | Letter_mutation: bool Effect.t
  | Letter: char Effect.t
  | Type: ty Effect.t


let name () = Effect.perform Random_name
let ty () = Effect.perform Type

module Name_set = Set.Make(String)

let shuffle a =
  let a = Array.copy a in
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i+1) in
    let tmp = a.(j) in
    a.(j) <- a.(i);
    a.(i) <- tmp
  done;
  a

let init n =
  let x = Array.init n (fun _ -> name (), ty()) in
  x, shuffle x

let mutation () = Effect.perform Str_mutation
let letter () = Effect.perform Letter
let change_letter () = Effect.perform Letter_mutation


let one_letter (s,ty) =
  String.map (fun c ->
      if change_letter () then letter () else c
    ) s, ty

let mixture_name aux l =
  let b = Buffer.create (String.length l) in
  for i = 0 to (max (String.length l) (String.length aux) - 1) do
    if change_letter () && i < String.length aux then
      Buffer.add_char b aux.[i]
    else if i < String.length l then
      Buffer.add_char b l.[i]
  done;
  Buffer.contents b

let mixture (y,_yt) (x,xt) =
  (mixture_name x y, xt)

let change_type (n,_) = (n, ty ())

let random_index array = Effect.perform (Random_int (Dynarray.length array))
let elt array = Dynarray.get array (random_index array)
let update array f =
  let index = random_index array in
  Dynarray.set array index (f @@ Dynarray.get array index)

let nihil = "", Char

let mutate array =
  match mutation () with
  | Duplicate ->
    let elt = elt array in
    Dynarray.add_last array elt
  | Delete -> Dynarray.set array (random_index array) nihil
  | Single -> update array one_letter
  | Mixture -> update array (mixture @@ elt array)
  | Change_type -> update array change_type


let rec repeat n str =
  if n < 1 then () else
    let () = mutate str in
    repeat (n-1) str

let keywords = Name_set.of_list
    [ "if"; "true"; "false"; "for"; "of"; "to"; "in"; "or"; "done";
    "asr"; "lsl"; "lsr"; "mod"; "asl"; "do"; "as"; "let"; "val"; "end"; "begin";"struct"; "sig";
     "object"; "lazy"; "else"; "then" ;"rec"; "with"; "when"; "try"; "match"; "lor"; "land"; "and";
    "fun"; "function"; "new"; "lxor"; "type"; "module"; "open"; "include"; "exception"; "effect"; ""
    ]

let names d =
  Dynarray.fold_left (fun set x ->
      Name_set.add (fst x) set
    ) Name_set.empty d

let deduplicate set a =
  let rec aux d set pos =
    if pos < 0 then Dynarray.to_array d
    else
      let elt = Dynarray.get a pos in
      if Name_set.mem (fst elt) set then
        aux d set (pos-1)
      else
        let () = Dynarray.add_last d elt in
        let set = Name_set.add (fst elt) set in
        aux d set (pos-1)
  in
  aux
    (Dynarray.create ())
    (Name_set.union set keywords)
    (Dynarray.length a -1)

let gen ~mut ~size =
  let str, sg = init size in
  let str, sg = Dynarray.of_array str, Dynarray.of_array sg in
  repeat mut str;
  repeat mut sg;
  let str_set = names str in
  let sg_set = names sg in
  let str = deduplicate sg_set str in
  let sg = deduplicate str_set sg in
  str, sg

let uletter () =
  Char.chr @@ Char.code 'a' + Random.int 26

let exp scale =
  2 + Float.to_int (~-. scale *. log (Random.float 1.))

let name () =
  let len = exp 10. in
  String.init len (fun _ -> uletter ())

let continue = Effect.Deep.continue

let mutation () =
  match Random.int 10 with
  | 0 -> Duplicate
  | 1 -> Delete
  | 2 -> Mixture
  | 3 -> Change_type
  | 4|5|6|7|8|9| _ -> Single

let uty () = match Random.int 4 with
  | 0 -> Int
  | 1 -> Char
  | 2 -> Float
  | 3 | _ -> String


let small_change () = Random.float 1. > 0.9

let handler f = match f () with
  | effect Random_int n, k -> continue k @@ Random.int n
  | effect Random_name, k -> continue k @@ name ()
  | effect Str_mutation, k ->  continue k @@ mutation ()
  | effect Letter_mutation, k -> continue k @@ small_change ()
  | effect Letter, k -> continue k (uletter ())
  | effect Type, k -> continue k (uty ())
  | x -> x

let () = Random.self_init ()
let process ~mut ~size  =
  handler (fun () -> gen ~mut ~size)
