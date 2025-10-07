type ('a,'v) t = {
  left : 'a list;
  pairs : ('v * 'v) list;
  right : 'a list;
}


let reverse d =
  {
    right = d.left;
    left = d.right;
    pairs = List.map (fun (right, left) -> (left, right)) d.pairs;
  }
