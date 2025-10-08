open Fuzzy.Automaton
open Fuzzy.Maps
let semi ppf () = Format.fprintf ppf ";@ "

let comma ppf () = Format.fprintf ppf ",@ "

 let pp_uchar ppf u =
  let b = Buffer.create 0 in
  Buffer.add_utf_8_uchar b u;
  Format.fprintf ppf "'%s'" (Buffer.contents b)

let pp_dynarray pr ppf x =
  Format.fprintf ppf "⦅%a⦆"
    (Format.pp_print_seq ~pp_sep:semi pr) (Dynarray.to_seq x)


let pp_map key_pr to_seq pr ppf m =
  let binding ppf (k,x) = Format.fprintf ppf "%a⇒%a" key_pr k pr x in
  Format.fprintf ppf "{%a}"
    (Format.pp_print_seq ~pp_sep:comma binding) (to_seq m)

let pp_uchar_map pr ppf m = pp_map pp_uchar Uchar_map.to_seq pr ppf m
let pp_int_map pr ppf m = pp_map Format.pp_print_int Int_map.to_seq pr ppf m
let pp_rev_map pr ppf m = pp_map pp_state State_map.to_seq pr ppf m

let _, p, _u = profile "atlas"

let a = create 1 p;;

#install_printer pp_diag;;
#install_printer pp_uchar;;
#install_printer pp_dynarray;;
#install_printer pp_uchar_map;;
#install_printer pp_int_map;;
#install_printer pp_rev_map;;
