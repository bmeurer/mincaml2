type t =
  | Arrow of t list * t
  | Tuple of t list
  | Constr of t list * string
  | Variable of variable
  | Poly of variable * t
and variable =
    t option ref

let tunit = Constr([], "unit")
let tbool = Constr([], "bool")
let tchar = Constr([], "char")
let tint = Constr([], "int")
let tfloat = Constr([], "float")
let tstring = Constr([], "string")
let tref tau = Constr([tau], "ref")

type environment =
    (string * t) list

let to_string (tau:t): string =
  let list_to_string sep to_string xl: string =
    List.fold_left (fun s x -> let sx = to_string x in if s = "" then sx else s ^ sep ^ sx) "" xl
  in let mapping = ref [] in
  let to_string_var (alpha:variable) : string =
    let n = (try
               List.assq alpha !mapping
             with
               | Not_found -> let n = List.length !mapping in mapping := (alpha, n) :: !mapping; n) in
      "'" ^ (String.make 1 (char_of_int (97 + n)))
  in let rec to_string_aux (tau:t): string =
    match tau with
      | Arrow(taul, tau) ->
          "(" ^ (list_to_string ", " to_string_aux taul) ^ " -> " ^ (to_string_aux tau) ^ ")"
      | Tuple(taul) ->
          "(" ^ (list_to_string " * " to_string_aux taul) ^ ")"
      | Constr([], name) ->
          name
      | Constr([tau], name) ->
          (to_string_aux tau) ^ " " ^ name
      | Constr(taul, name) ->
          "(" ^ (list_to_string ", " to_string_aux taul) ^ " " ^ name ^ ")"
      | Variable({ contents = Some(tau) }) ->
          to_string_aux tau
      | Variable(alpha) ->
          to_string_var alpha
      | Poly(alpha, tau) ->
          "Forall " ^ (to_string_var alpha) ^ ". " ^ (to_string_aux tau)
  in to_string_aux tau

let newvar (():unit): t =
  Variable(ref None)

let rec fv (tau:t): variable list =
  let fvl (taul:t list): variable list =
    List.fold_left (fun vl tau -> List.rev_append (fv tau) vl) [] taul
  in match tau with
    | Arrow(taul, tau) -> List.rev_append (fvl taul) (fv tau)
    | Tuple(taul)
    | Constr(taul, _) -> fvl taul
    | Variable({ contents = Some(tau) }) -> fv tau
    | Variable({ contents = None } as alpha) -> [alpha]
    | Poly(alpha, tau) -> List.filter ((!=) alpha) (fv tau)

(* replace all occurences of alpha in tau with a fresh variable *)
let fresh (tau:t) (alpha:variable): t =
  let oalpha = alpha in
  let nalpha = ref None in
  let rec fresh_aux (tau:t): t =
    match tau with
      | Arrow(taul, tau) -> Arrow(List.map fresh_aux taul, fresh_aux tau)
      | Tuple(taul) -> Tuple(List.map fresh_aux taul)
      | Constr(taul, name) -> Constr(List.map fresh_aux taul, name)
      | Variable(alpha) when alpha == oalpha -> Variable(nalpha)
      | Variable({ contents = Some(tau) }) -> fresh_aux tau
      | Variable(_) as tau -> tau
      | Poly(alpha, tau) when alpha != oalpha -> Poly(alpha, fresh_aux tau)
      | Poly(_) as tau -> tau
  in fresh_aux tau

let rec normalize (tau:t): t =
  match tau with
    | Arrow(taul, tau) -> Arrow(List.map normalize taul, normalize tau)
    | Tuple(taul) -> Tuple(List.map normalize taul)
    | Constr(taul, name) -> Constr(List.map normalize taul, name)
    | Variable({ contents = Some(tau) }) -> normalize tau
    | Variable(_) as tau -> tau
    | Poly(alpha, tau) -> Poly(alpha, normalize tau)

let generalize (gamma:environment) (tau:t): t =
  let bvl = List.fold_left (fun bvl (_, tau) -> List.rev_append (fv tau) bvl) [] gamma in
  let fvl = List.filter (fun alpha -> not (List.memq alpha bvl)) (Listutils.normalizeq (fv tau)) in
    List.fold_left (fun tau alpha -> Poly(alpha, tau)) tau fvl

let rec instantiate (tau:t): t =
  match tau with
    | Poly(alpha, tau) -> fresh (instantiate tau) alpha
    | tau -> tau

let gamma0:environment =
  let alpha = newvar () in
  let beta = newvar () in
  let alpha_alpha_to_bool = Arrow([alpha; alpha], tbool) in
  let int_int_to_int = Arrow([tint; tint], tint) in
  let float_float_to_float = Arrow([tfloat; tfloat], tfloat) in
  let alpha_times_beta = Tuple([alpha; beta]) in
  let alpha_ref = tref alpha in
  let gamma0 =
    ["()",    tunit;
     "true",  tbool;
     "false", tbool;
     "=",     alpha_alpha_to_bool;
     "<>",    alpha_alpha_to_bool;
     "<",     alpha_alpha_to_bool;
     ">",     alpha_alpha_to_bool;
     "<=",    alpha_alpha_to_bool;
     ">=",    alpha_alpha_to_bool;
     "==",    alpha_alpha_to_bool;
     "!=",    alpha_alpha_to_bool;
     "~-",    Arrow([tint], tint);
     "+",     int_int_to_int;
     "-",     int_int_to_int;
     "*",     int_int_to_int;
     "land",  int_int_to_int;
     "lor",   int_int_to_int;
     "lxor",  int_int_to_int;
     "lnot",  int_int_to_int;
     "lsl",   int_int_to_int;
     "lsr",   int_int_to_int;
     "asr",   int_int_to_int;
     "~-.",   Arrow([tfloat], tfloat);
     "+.",    float_float_to_float;
     "-.",    float_float_to_float;
     "*.",    float_float_to_float;
     "^",     Arrow([tstring; tstring], tstring);
     "fst",   Arrow([alpha_times_beta], alpha);
     "snd",   Arrow([alpha_times_beta], beta);
     "ref",   Arrow([alpha], alpha_ref);
     "!",     Arrow([alpha_ref], alpha);
     ":=",    Arrow([alpha_ref; alpha], tunit)]
  in List.map (fun (id, tau) -> id, generalize [] tau) gamma0
