let count (a:'a) (l:'a list): int =
  let rec count_aux l n =
    match l with
      | [] -> n
      | x :: l when x = a -> count_aux l (n + 1)
      | x :: l -> count_aux l n
  in count_aux l 0
  
let normalizeq (xl:'a list): 'a list =
  let rec normalizeq_aux xl tl =
    match xl with
      | [] -> List.rev tl
      | x :: xl -> normalizeq_aux xl (if List.memq x tl then tl else x :: tl)
  in normalizeq_aux xl []

let remove (x:'a) (xl:'a list): 'a list =
  let rec remove_aux xl tl =
    match xl with
      | [] -> List.rev tl
      | x' :: xl -> if x <> x' then remove_aux xl (x' :: tl) else remove_aux xl tl
  in remove_aux xl []

let to_string (separator:string) (to_string:'a -> string) (xl:'a list): string =
  List.fold_left (fun s x -> let sx = to_string x in if s = "" then sx else s ^ separator ^ sx) "" xl
