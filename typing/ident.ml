open Format

type t =
    { name:  string;
      stamp: int }

let current_stamp = ref 0

let create name =
  let stamp = !current_stamp + 1 in
    current_stamp := stamp;
    { name = name; stamp = stamp }

let create_predefined name =
  { name = name; stamp = -1 }

let create_tmp i =
  create ("t" ^ (string_of_int i))

let name id =
  id.name

let unique_name id =
  if id.stamp >= 0 then
    id.name ^ "__" ^ (string_of_int id.stamp)
  else
    id.name

let compare id1 id2 =
  let c = id1.stamp - id2.stamp in
    if c <> 0 then
      c
    else
      String.compare id1.name id2.name

let equal id1 id2 =
  compare id1 id2 = 0

let print ppf id =
  match id.stamp with
    | -1 -> fprintf ppf "%s" id.name
    | stamp -> fprintf ppf "%s/%i" id.name stamp
