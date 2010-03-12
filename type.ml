type t =
  | Unit
  | Bool
  | Int
  | Char
  | Float
  | String
  | Arrow of t * t
  | Tuple of t list
  | Variable of variable
  | Poly of variable * t
and variable =
    t option ref

let to_string (tau:t): string =
  let mapping = ref [] in
  let to_string_var (alpha:variable) : string =
    let n = (try
               List.assq alpha !mapping
             with
               | Not_found -> let n = List.length !mapping in mapping := (alpha, n) :: !mapping; n) in
      "'" ^ (String.make 1 (char_of_int (97 + n)))
  in let rec to_string_aux (tau:t): string =
    match tau with
      | Unit ->
          "unit"
      | Bool ->
          "bool"
      | Int ->
          "int"
      | Char ->
          "char"
      | Float ->
          "float"
      | String ->
          "string"
      | Arrow(tau, tau') ->
          "(" ^ (to_string_aux tau) ^ " -> " ^ (to_string_aux tau') ^ ")"
      | Tuple(taul) ->
          "(" ^ (List.fold_left
                   (fun s tau -> 
                      (to_string_aux tau) ^ (if s = "" then "" else " * " ^ s))
                   ""
                   taul) ^ ")"
      | Variable({ contents = Some(tau) }) ->
          to_string_aux tau
      | Variable(alpha) ->
          to_string_var alpha
      | Poly(alpha, tau) ->
          "Forall " ^ (to_string_var alpha) ^ ". " ^ (to_string_aux tau)
  in to_string_aux tau

let newvar (_:unit): t =
  Variable(ref None)

let rec fv (tau:t): variable list =
  match tau with
    | Arrow(tau, tau') -> List.rev_append (fv tau) (fv tau')
    | Tuple(taul) -> List.fold_left (fun vl tau -> List.rev_append (fv tau) vl) [] taul
    | Variable({ contents = Some(tau) }) -> fv tau
    | Variable({ contents = None } as alpha) -> [alpha]
    | Poly(alpha, tau) -> List.filter ((!=) alpha) (fv tau)
    | _ -> []

(* replace all occurences of alpha in tau with a fresh variable *)
let fresh (tau:t) (alpha:variable): t =
  let oalpha = alpha in
  let nalpha = ref None in
  let rec fresh_aux (tau:t): t =
    match tau with
      | Arrow(tau, tau') -> Arrow(fresh_aux tau, fresh_aux tau')
      | Tuple(taul) -> Tuple(List.map fresh_aux taul)
      | Variable(alpha) when alpha == oalpha -> Variable(nalpha)
      | Variable({ contents = Some(tau) }) -> fresh_aux tau
      | Poly(alpha, tau) when alpha != oalpha -> Poly(alpha, fresh_aux tau)
      | tau -> tau
  in fresh_aux tau

let rec normalize (tau:t): t =
  match tau with
    | Arrow(tau, tau') -> Arrow(normalize tau, normalize tau')
    | Tuple(taul) -> Tuple(List.map normalize taul)
    | Variable({ contents = Some(tau) }) -> normalize tau
    | Poly(alpha, tau) -> Poly(alpha, normalize tau)
    | tau -> tau
