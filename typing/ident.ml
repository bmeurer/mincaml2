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
  let s = (if id.stamp >= 0 then
             id.name ^ "__" ^ (string_of_int id.stamp)
           else
             "mc2_" ^ id.name) in
  let b = Buffer.create (String.length s) in
    for i = 0 to String.length s - 1 do
      match s.[i] with
        | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' as c ->
            Buffer.add_char b c
        | _ as c ->
            Buffer.add_char b '$';
            Buffer.add_string b (Printf.sprintf "%02x" (Char.code c))
    done;
    Buffer.contents b

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
