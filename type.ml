type t =
  | Arrow of t * t
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
  let mapping = ref [] in
  let to_string_var (alpha:variable) : string =
    let n = (try
               List.assq alpha !mapping
             with
               | Not_found -> let n = List.length !mapping in mapping := (alpha, n) :: !mapping; n) in
      "'" ^ (String.make 1 (char_of_int (97 + n)))
  in let rec to_string_aux (tau:t): string =
    match tau with
      | Arrow(tau, tau') ->
          "(" ^ (to_string_aux tau) ^ " -> " ^ (to_string_aux tau') ^ ")"
      | Tuple(taul) ->
          "(" ^ (List.fold_left
                   (fun s tau -> 
                      (to_string_aux tau) ^ (if s = "" then "" else " * " ^ s))
                   ""
                   taul) ^ ")"
      | Constr([], name) ->
          name
      | Constr([tau], name) ->
          (to_string_aux tau) ^ " " ^ name
      | Constr(taul, name) ->
          "(" ^ (List.fold_left
                   (fun s tau ->
                      (to_string_aux tau) ^ (if s = "" then "" else ", " ^ s))
                   ""
                   taul) ^ ") " ^ name
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
    | Tuple(taul)
    | Constr(taul, _) -> List.fold_left (fun vl tau -> List.rev_append (fv tau) vl) [] taul
    | Variable({ contents = Some(tau) }) -> fv tau
    | Variable({ contents = None } as alpha) -> [alpha]
    | Poly(alpha, tau) -> List.filter ((!=) alpha) (fv tau)

(* replace all occurences of alpha in tau with a fresh variable *)
let fresh (tau:t) (alpha:variable): t =
  let oalpha = alpha in
  let nalpha = ref None in
  let rec fresh_aux (tau:t): t =
    match tau with
      | Arrow(tau, tau') -> Arrow(fresh_aux tau, fresh_aux tau')
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
    | Arrow(tau, tau') -> Arrow(normalize tau, normalize tau')
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
  let alpha_to_alpha_to_bool = Arrow(alpha, Arrow(alpha, tbool)) in
  let int_to_int = Arrow(tint, tint) in
  let int_to_int_to_int = Arrow(tint, int_to_int) in
  let float_to_float = Arrow(tfloat, tfloat) in
  let float_to_float_to_float = Arrow(tfloat, float_to_float) in
  let alpha_times_beta = Tuple([alpha; beta]) in
  let alpha_ref = tref alpha in
  let gamma0 =
    ["()",   tunit;
     "=",    alpha_to_alpha_to_bool;
     "<>",   alpha_to_alpha_to_bool;
     "<",    alpha_to_alpha_to_bool;
     ">",    alpha_to_alpha_to_bool;
     "<=",   alpha_to_alpha_to_bool;
     ">=",   alpha_to_alpha_to_bool;
     "==",   alpha_to_alpha_to_bool;
     "!=",   alpha_to_alpha_to_bool;
     "~-",   int_to_int;
     "+",    int_to_int_to_int;
     "-",    int_to_int_to_int;
     "*",    int_to_int_to_int;
     "land", int_to_int_to_int;
     "lor",  int_to_int_to_int;
     "lxor", int_to_int_to_int;
     "lnot", int_to_int_to_int;
     "lsl",  int_to_int_to_int;
     "lsr",  int_to_int_to_int;
     "asr",  int_to_int_to_int;
     "~-.",  float_to_float;
     "+.",   float_to_float_to_float;
     "-.",   float_to_float_to_float;
     "*.",   float_to_float_to_float;
     "^",    Arrow(tstring, Arrow(tstring, tstring));
     "fst",  Arrow(alpha_times_beta, alpha);
     "snd",  Arrow(alpha_times_beta, beta);
     "ref",  Arrow(alpha, alpha_ref);
     "!",    Arrow(alpha_ref, alpha);
     ":=",   Arrow(alpha_ref, Arrow(alpha, tunit))]
  in List.map (fun (id, tau) -> id, generalize [] tau) gamma0
