type t =
    { name:  string;
      stamp: int }

let current_stamp = ref 0

let create (name:string): t =
  let stamp = !current_stamp + 1 in
    current_stamp := stamp;
    { name = name; stamp = stamp }

let name (id:t): string =
  id.name

let same (id1:t) (id2:t): bool =
  id1 = id2

let equal (id1:t) (id2:t): bool =
  id1.name = id2.name
