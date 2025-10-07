type ('v, 'k) t = {
  name: string;
  item: 'v;
  kind: 'k;
}

let name f = f.name
let item f = f.item
let kind i = i.kind

type ('v,'k) matches =  (('v,'k) t, 'v) Matches.t
