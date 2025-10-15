
let min4 a b c d = min (min a b) (min c d)

let init lim left right =
  let m = Array.make_matrix (1 + Array.length left) (1 + Array.length right) lim in
  for i = 0 to Array.length left do
    m.(i).(0) <- i
  done;
  for j = 0 to Array.length right do
    m.(0).(j) <- j
  done;
  m

let next_char pos s =
  let d = String.get_utf_8_uchar s pos in
  let len = Uchar.utf_decode_length d in
  let u = Uchar.utf_decode_uchar d in
  pos + len, u

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


let fill lim mat left right =
  let last_seen_on_left = Hashtbl.create 5 in
  let last_eq_on_right = ref 0 in
  for i=1 to Array.length left do
    let start = max 1 (i-lim) and stop = min (i+lim) (Array.length right) in
    last_eq_on_right := 0;
    for j = start to stop do
      let l = !last_eq_on_right in
      let local_cost =
        if Uchar.equal left.(i-1) right.(j-1) then
          (last_eq_on_right := j; 0)
        else 1
      in
      let transition = match l, Hashtbl.find_opt last_seen_on_left right.(j-1) with
        | 0, _ | _, None -> lim
        | _, Some k -> mat.(k-1).(l-1) + (i-k-1) + (j-l-1) + local_cost
      in
      mat.(i).(j) <-
        min4
          (mat.(i-1).(j-1) + local_cost)
          (mat.(i-1).(j) + 1)
          (mat.(i).(j-1) + 1)
          transition
    done;
    Hashtbl.replace last_seen_on_left left.(i-1) i;
  done

let distance lim left right =
  let mat = init lim left right in
  fill lim mat left right;
  mat.(Array.length left).(Array.length right)

let edit_distance' lim l r = distance lim l r

let edit_distance lim l r = distance lim (uchar_array l) (uchar_array r)
