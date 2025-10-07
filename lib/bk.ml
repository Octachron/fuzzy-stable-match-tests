open Maps
type 'a t = { root: 'a; name:string; children: 'a t Int_map.t }

  let rec query results stack ~cutoff ~max_dist name t =
    let dist = String.edit_distance name t.name in
    let results =
      if dist <= cutoff then Int_map.add_to_list dist t.root results
      else results
    in
    query_children results stack ~cutoff ~max_dist name dist t.children
  and query_children result stack ~cutoff ~max_dist name dist children =
    let children =
      if max_dist = 0 then Option.to_list (Int_map.find_opt dist children)
      else
        let left = if dist - max_dist < 0
          then None
          else Int_map.find_opt (dist-max_dist) children
        in
        let right = Int_map.find_opt (dist+max_dist) children in
        Option.to_list left @ Option.to_list right
    in
    match children, stack with
    | [], [] -> result
    | [], a :: q -> query result q ~cutoff ~max_dist name a
    | a :: q, _  ->
        let stack = q @ stack in
        query result stack ~cutoff ~max_dist name a

  let rec layer_seq result ~cutoff ~max_dist dist children name () =
    if max_dist > cutoff then Seq.Nil
    else
      let r = query_children result []  ~cutoff ~max_dist name dist children in
      let at_dist =
        Option.value ~default:[] (Int_map.find_opt max_dist r)
      in
      match at_dist with
      | [] -> layer_seq r ~cutoff ~max_dist:(max_dist+1) dist children name ()
      | _ ->
          let next =
            layer_seq r ~cutoff ~max_dist:(max_dist+1) dist children name in
          Seq.Cons( {Model.left_candidates=at_dist; pref=max_dist}, next)

  let layers cutoff (t: int t) name =
    let cutoff = cutoff name in
    let dist = String.edit_distance t.name name in
    let results =
      if dist <= cutoff then Int_map.singleton dist [t.root]
      else Int_map.empty
    in
    layer_seq results ~cutoff ~max_dist:0 dist t.children name

  let rec make = function
    | [] -> invalid_arg "Empty lexicon"
    | (root_name,root) :: q ->
        let dist_map = List.fold_left (fun map (name,_ as x) ->
            let dist = String.edit_distance root_name name in
            Int_map.add_to_list dist x map
          ) Int_map.empty q
        in
        let children = Int_map.map make dist_map in
        { root; name=root_name; children }

  let preferences ?max_elements:_ ~cutoff d =
    let d = List.mapi (fun i item -> Item.name item, i) d in
    match d with
    | [] -> Fun.const Seq.empty
    | _ ->
        layers cutoff (make d)
