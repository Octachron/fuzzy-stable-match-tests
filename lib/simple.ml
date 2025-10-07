let preferences ?max_elements:_ ~cutoff left name =
  let cutoff = 1 + cutoff name in
  let a =
    Array.of_seq
    @@ Seq.filter (fun (_,d) -> d < cutoff)
    @@ Seq.mapi (fun i r ->
        i, String.edit_distance ~limit:cutoff name @@ Item.name r)
    @@ Array.to_seq left in
  let () = Array.sort (fun (_,n) (_,n') -> Int.compare n n') a in
  let rec group_by current acc pos () =
    if pos >= Array.length a then
      match acc with
      | [] -> Seq.Nil
      | _ -> Seq.Cons ({ Model.left_candidates=acc; pref=current }, Seq.empty)
    else
      let x, dist = a.(pos) in
      if dist = current then
        group_by current (x::acc) (pos+1) ()
      else if acc = [] then
        group_by dist [x] (pos+1) ()
      else
        Seq.Cons (
          {left_candidates=acc; pref=current}, group_by dist [x] (pos+1)
        )
  in
  group_by 0 [] 0
