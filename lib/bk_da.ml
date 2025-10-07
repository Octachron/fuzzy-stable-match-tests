  type 'a t = { root: 'a; name:string; children: 'a t option Dynarray.t  }

  let (.?()) da n = if n >= Dynarray.length da || n <= 0 then None else
      Dynarray.get da n


  let (.$()<-) da n x =
    if n < Dynarray.length da then Dynarray.set da n (x::Dynarray.get da n)
    else
      let diff = 1 + n - Dynarray.length da in
      Dynarray.append_seq da Seq.(take diff @@ repeat []);
      Dynarray.set da n [x]

  let rec query results stack ~cutoff ~max_dist name t =
    let dist = String.edit_distance name t.name in
    if dist <= cutoff then Hashtbl.add results dist t.root;
    query_children results stack ~cutoff ~max_dist name dist t.children
  and query_children result stack ~cutoff ~max_dist name dist children =
    let children =
      if max_dist = 0 then Option.to_list children.?(dist)
      else
        let left = children.?(dist-max_dist) in
        let right = children.?(dist+max_dist) in
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
      let r = query_children result [] ~cutoff ~max_dist name dist children in
      let at_dist = Hashtbl.find_all r max_dist in
      match at_dist with
      | [] -> layer_seq r ~cutoff ~max_dist:(max_dist+1) dist children name ()
      | _ ->
          let next =
            layer_seq r ~cutoff ~max_dist:(max_dist+1) dist children name in
          Seq.Cons( {Model.left_candidates=at_dist; pref=max_dist}, next)

  let layers cutoff (t: int t) name =
    let cutoff = cutoff name in
    let dist = String.edit_distance t.name name in
    let results = Hashtbl.create 17 in
    if dist <= cutoff then Hashtbl.add results dist t.root;
    layer_seq results ~cutoff ~max_dist:0 dist t.children name

  let rec make = function
    | [] -> invalid_arg "Empty lexicon"
    | (root_name,root) :: q ->
        let da = Dynarray.create () in
        List.iter (fun (name,_ as x) ->
            let dist = String.edit_distance root_name name in
            da.$(dist) <- x
          ) q;
        let children =
          Dynarray.map (function [] -> None | l -> Some(make l)) da in
        { root; name=root_name; children }

  let preferences ?max_elements:_ ~cutoff d =
    let d = List.mapi (fun i item -> Item.name item, i) d in
    match d with
    | [] -> Fun.const Seq.empty
    | _ ->
        layers cutoff (make d)
