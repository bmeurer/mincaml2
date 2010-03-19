open Astcommon
open Parsedast
open Typedast
open Types


type error =
  | Type_arity_mismatch of string * int * int * Location.t
  | Unbound_type_constructor of string * Location.t
  | Unbound_type_variable of string * Location.t
exception Error of error


(*******************)
(*** Unification ***)
(*******************)

exception Unify_error of type_expr * type_expr

let rec occur (alpha:type_variable) (tau:type_expr): bool =
  match tau with
    | Tvar(alpha') when alpha == alpha' -> true
    | Tvar({ contents = Some(tau') }) -> occur alpha tau'
    | Tarrow(tau, tau') -> occur alpha tau || occur alpha tau'
    | Ttuple(taul) 
    | Tconstruct(_, taul) -> List.exists (occur alpha) taul
    | Tpoly(alpha', tau) when alpha <> alpha' -> occur alpha tau
    | _ -> false

let rec unify (tau1:type_expr) (tau2:type_expr): unit =
  try
    match tau1, tau2 with
      | Tarrow(tau1, tau1'), Tarrow(tau2, tau2') ->
          unify tau1 tau2;
          unify tau1' tau2'
      | Ttuple(tau1l), Ttuple(tau2l) ->
          List.iter2 unify tau1l tau2l
      | Tconstruct(id1, tau1l), Tconstruct(id2, tau2l) when Ident.same id1 id2 ->
          List.iter2 unify tau1l tau2l
      | Tpoly(_), _
      | _, Tpoly(_) ->
          invalid_arg "Typing.unify"
      | Tvar(alpha1), Tvar(alpha2) when alpha1 == alpha2 ->
          ()
      | Tvar({ contents = Some(tau1) }), tau2
      | tau1, Tvar({ contents = Some(tau2) }) ->
          unify tau1 tau2
      | Tvar({ contents = None } as alpha), tau 
      | tau, Tvar({ contents = None } as alpha) when not (occur alpha tau) ->
          alpha := Some(tau)
      | tau1, tau2 ->
          raise (Unify_error(tau1, tau2))
  with
    | Invalid_argument("List.iter2") ->
        raise (Unify_error(tau1, tau2))


(*** TODO ***)

(* replace all unbound occurrences of alpha in tau with a fresh variable *)
let fresh (alpha:type_variable) (tau:type_expr): type_expr =
  let oalpha = alpha in
  let nalpha = ref None in
  let rec fresh_aux (tau:type_expr): type_expr =
    match tau with
      | Tvar(alpha) when alpha == oalpha -> Tvar(nalpha)
      | Tvar({ contents = Some(tau) }) -> fresh_aux tau
      | Tarrow(tau, tau') -> Tarrow(fresh_aux tau, fresh_aux tau')
      | Ttuple(taul) -> Ttuple(List.map fresh_aux taul)
      | Tconstruct(id, taul) -> Tconstruct(id, List.map fresh_aux taul)
      | Tpoly(alpha, tau) when alpha != oalpha -> Tpoly(alpha, fresh_aux tau)
      | tau -> tau
  in fresh_aux tau

let rec instantiate (tau:type_expr): type_expr =
  match tau with
    | Tpoly(alpha, tau) -> fresh alpha (instantiate tau)
    | tau -> tau

type policy =
  | Open   (* add new type variables as they appear *)
  | Closed (* fail on unknown type variable *)

let rec translate_type (policy:policy) alphas (gamma:Typeenv.t) (ptau:typ): type_expr =
  match ptau.ptyp_desc with
    | Ptyp_any ->
        if policy = Open then
          Tvar(new_type_variable ())
        else
          raise (Error(Unbound_type_variable("_", ptau.ptyp_loc)))
    | Ptyp_var(name) ->
        Tvar(try
               List.assoc name !alphas
             with
               | Not_found when policy = Open ->
                   let alpha = new_type_variable () in
                     alphas := (name, alpha) :: !alphas;
                     alpha
               | Not_found ->
                   raise (Error(Unbound_type_variable(name, ptau.ptyp_loc))))
    | Ptyp_arrow(ptau1, ptau2) ->
        Tarrow(translate_type policy alphas gamma ptau1, translate_type policy alphas gamma ptau2)
    | Ptyp_tuple(ptaul) ->
        Ttuple(List.map (translate_type policy alphas gamma) ptaul)
    | Ptyp_construct(name, ptaul) ->
        let taul = List.map (translate_type policy alphas gamma) ptaul in
        let taul_length = List.length taul in
        let (id, decl) = (try
                            Typeenv.lookup_type name gamma
                          with
                            | Not_found ->
                                raise (Error(Unbound_type_constructor(name, ptau.ptyp_loc)))) in
          if taul_length <> decl.type_arity then
            raise (Error(Type_arity_mismatch(name, decl.type_arity, taul_length, ptau.ptyp_loc)));
          Tconstruct(id, taul)


(**********************)
(*** Type inference ***)
(**********************)

let type_constant (c:constant): type_expr =
  instantiate (match c with
                 | Const_int(_) -> Predefined.type_int
                 | Const_char(_) -> Predefined.type_char
                 | Const_float(_) -> Predefined.type_float
                 | Const_int32(_) -> Predefined.type_int32
                 | Const_int64(_) -> Predefined.type_int64
                 | Const_string(_) -> Predefined.type_string
                 | Const_nativeint(_) -> Predefined.type_nativeint)

let rec type_exp (gamma:Typeenv.t) (pe:Parsedast.expression): Typedast.expression =
  match pe.pexp_desc with
    | Pexp_constant(c) ->
        { exp_desc = Texp_constant(c);
          exp_loc = pe.pexp_loc;
          exp_tau = type_constant c;
          exp_gamma = gamma }
    (* TODO *)
    | Pexp_tuple(pel) ->
        let el = List.map (type_exp gamma) pel in
          { exp_desc = Texp_tuple(el);
            exp_loc = pe.pexp_loc;
            exp_tau = Ttuple(List.map (fun e -> e.exp_tau) el);
            exp_gamma = gamma }
    (* TODO *)
    | Pexp_ifthenelse(pe0, pe1, pe2) ->
        let e0 = solve_exp gamma pe0 (instantiate Predefined.type_bool) in
          begin match pe2 with
            | None ->
                let e1 = solve_exp gamma pe1 (instantiate Predefined.type_unit) in
                  { exp_desc = Texp_ifthenelse(e0, e1, None);
                    exp_loc = pe.pexp_loc;
                    exp_tau = e1.exp_tau;
                    exp_gamma = gamma }
            | Some(pe2) ->
                let e1 = type_exp gamma pe1 in
                let e2 = solve_exp gamma pe2 e1.exp_tau in
                  { exp_desc = Texp_ifthenelse(e0, e1, Some(e2));
                    exp_loc = pe.pexp_loc;
                    exp_tau = e1.exp_tau;
                    exp_gamma = gamma }
          end
    | Pexp_sequence(pe1, pe2) ->
        let e1 = solve_exp gamma pe1 (instantiate Predefined.type_unit) in
        let e2 = type_exp gamma pe2 in
          { exp_desc = Texp_sequence(e1, e2);
            exp_loc = pe.pexp_loc;
            exp_tau = e2.exp_tau;
            exp_gamma = gamma }
    (* TODO *)
    | Pexp_when(pe1, pe2) ->
        let e1 = solve_exp gamma pe1 (instantiate Predefined.type_bool) in
        let e2 = type_exp gamma pe2 in
          { exp_desc = Texp_when(e1, e2);
            exp_loc = pe.pexp_loc;
            exp_tau = e2.exp_tau;
            exp_gamma = gamma }

and solve_exp (gamma:Typeenv.t) (pe:Parsedast.expression) (tau:type_expr): Typedast.expression =
  try
    let e = type_exp gamma pe in
      unify e.exp_tau tau;
      e
  with
      (* TODO *)
    | Unify_error(_) as exn -> raise exn
