type t =
    string

let generate (template:t) (blacklist:t list): t =
  let rec generate_aux (i:int): t =
    match template ^ (string_of_int i) with
      | id when not (List.mem id blacklist) -> id
      | _ -> generate_aux (i + 1)
  in if List.mem template blacklist then generate_aux 1 else template

let generaten (n:int) (template:t) (blacklist:t list): t list =
  let rec generaten_aux (n:int) (i:int) (idl:t list): t list =
    match n, template ^ (string_of_int i) with
      | 0, _ -> idl
      | n, id when not (List.mem id blacklist) -> generaten_aux (n - 1) (i + 1) (id :: idl)
      | n, _ -> generaten_aux n (i + 1) idl
  in if n < 0 then raise (Invalid_argument("Id.generaten")) else generaten_aux n 1 []

let to_string (id:t): string =
  id
