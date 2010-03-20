type t =
    { name:  string;
      stamp: int }

let current_stamp = ref 0

let create name =
  let stamp = !current_stamp + 1 in
    current_stamp := stamp;
    { name = name; stamp = stamp }

let name id =
  id.name

let compare id1 id2 =
  let c = id1.stamp - id2.stamp in
    assert (c <> 0 || String.compare id1.name id2.name = 0);
    c

let equal id1 id2 =
  compare id1 id2 = 0

