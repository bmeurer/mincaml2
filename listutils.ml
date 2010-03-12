let normalizeq (xl:'a list): 'a list =
  let rec normalizeq_aux xl tl =
    match xl with
      | [] -> List.rev tl
      | x :: xl -> normalizeq_aux xl (if List.memq x tl then tl else x :: tl)
  in normalizeq_aux xl []

