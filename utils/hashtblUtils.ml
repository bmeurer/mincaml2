let create n l =
  let hashtbl = Hashtbl.create n in
    List.iter (fun (a, b) -> Hashtbl.add hashtbl a b) l;
    hashtbl
