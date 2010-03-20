open Astcommon
open Parsedast
open Typedast
open Types


type error =
  | Duplicate_pattern_variable of string * Location.t
  | Expression_type_mismatch of (type_expr * type_expr) list * Location.t
  | Pattern_type_mismatch of (type_expr * type_expr) list * Location.t
  | Type_arity_mismatch of string * int * int * Location.t
  | Unbound_type_constructor of string * Location.t
  | Unbound_type_variable of string * Location.t

exception Error of error


(*******************)
(*** Unification ***)
(*******************)

exception Unify_error of (type_expr * type_expr) list

let rec occur alpha = function
  | Tvar(alpha') when alpha == alpha' -> true
  | Tvar({ contents = Some(tau') }) -> occur alpha tau'
  | Tarrow(tau, tau') -> occur alpha tau || occur alpha tau'
  | Ttuple(taul) 
  | Tconstruct(_, taul) -> List.exists (occur alpha) taul
  | Tpoly(alpha', tau) when alpha <> alpha' -> occur alpha tau
  | _ -> false

let rec unify tau1 tau2 =
  try
    match tau1, tau2 with
      | Tarrow(tau1, tau1'), Tarrow(tau2, tau2') ->
          unify tau1 tau2;
          unify tau1' tau2'
      | Ttuple(tau1l), Ttuple(tau2l) ->
          List.iter2 unify tau1l tau2l
      | Tconstruct(id1, tau1l), Tconstruct(id2, tau2l) when Ident.equal id1 id2 ->
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
      | _ ->
          raise (Unify_error([]))
  with
    | Invalid_argument("List.iter2") ->
        raise (Unify_error([tau1, tau2]))
    | Unify_error(taupl) ->
        raise (Unify_error((tau1, tau2) :: taupl))


(*** TODO ***)

(* replace all unbound occurrences of alpha in tau with a fresh variable *)
let fresh alpha tau =
  let oalpha = alpha in
  let nalpha = ref None in
  let rec fresh_aux = function
    | Tvar(alpha) when alpha == oalpha -> Tvar(nalpha)
    | Tvar({ contents = Some(tau) }) -> fresh_aux tau
    | Tarrow(tau, tau') -> Tarrow(fresh_aux tau, fresh_aux tau')
    | Ttuple(taul) -> Ttuple(List.map fresh_aux taul)
    | Tconstruct(id, taul) -> Tconstruct(id, List.map fresh_aux taul)
    | Tpoly(alpha, tau) when alpha != oalpha -> Tpoly(alpha, fresh_aux tau)
    | tau -> tau
  in fresh_aux tau

let rec instantiate = function
  | Tpoly(alpha, tau) -> fresh alpha (instantiate tau)
  | tau -> tau

type policy =
  | Open   (* add new type variables as they appear *)
  | Closed (* fail on unknown type variable *)

let rec translate_type policy sigma gamma ptau =
  match ptau.ptyp_desc with
    | Ptyp_any ->
        if policy = Open then
          Tvar(new_type_variable ())
        else
          raise (Error(Unbound_type_variable("_", ptau.ptyp_loc)))
    | Ptyp_var(name) ->
        Tvar(try
               List.assoc name !sigma
             with
               | Not_found when policy = Open ->
                   let alpha = new_type_variable () in
                     sigma := (name, alpha) :: !sigma;
                     alpha
               | Not_found ->
                   raise (Error(Unbound_type_variable(name, ptau.ptyp_loc))))
    | Ptyp_arrow(ptau1, ptau2) ->
        Tarrow(translate_type policy sigma gamma ptau1, translate_type policy sigma gamma ptau2)
    | Ptyp_tuple(ptaul) ->
        Ttuple(List.map (translate_type policy sigma gamma) ptaul)
    | Ptyp_construct(name, ptaul) ->
        let taul = List.map (translate_type policy sigma gamma) ptaul in
        let taul_length = List.length taul in
        let (id, decl) = (try
                            Typeenv.find_type name gamma
                          with
                            | Not_found ->
                                raise (Error(Unbound_type_constructor(name, ptau.ptyp_loc)))) in
          if taul_length <> decl.type_arity then
            raise (Error(Type_arity_mismatch(name, decl.type_arity, taul_length, ptau.ptyp_loc)));
          Tconstruct(id, taul)


(****************************)
(*** Pattern environments ***)
(****************************)

module PatternEnv =
struct
  type t = (Ident.t * type_expr * Location.t) list

  let empty = []

  let add name tau loc rho =
    let rec add_aux = function
      | [] ->
          let id = Ident.create name in
            id, [id, tau, loc]
      | ((id', tau', loc') as entry') :: rho' as rho ->
          let c = String.compare name (Ident.name id') in
            if c < 0 then
              let id = Ident.create name in
                id, (id, tau, loc) :: rho
            else if c > 0 then
              let id, rho' = add_aux rho' in
                id, entry' :: rho'
            else
              raise (Error(Duplicate_pattern_variable(name, loc)))
    in add_aux rho
end


(**********************)
(*** Type inference ***)
(**********************)

let type_constant = function
  | Const_int(_) -> Predefined.type_int
  | Const_char(_) -> Predefined.type_char
  | Const_float(_) -> Predefined.type_float
  | Const_int32(_) -> Predefined.type_int32
  | Const_int64(_) -> Predefined.type_int64
  | Const_string(_) -> Predefined.type_string
  | Const_nativeint(_) -> Predefined.type_nativeint

let rec type_pat sigma gamma ppat rho =
  try
    match ppat.ppat_desc with
      | Ppat_any ->
          { pat_desc = Tpat_any;
            pat_loc = ppat.ppat_loc;
            pat_tau = Tvar(new_type_variable ());
            pat_gamma = gamma }, rho
      | Ppat_var(name) ->
          let tau = Tvar(new_type_variable ()) in
          let id, rho = PatternEnv.add name tau ppat.ppat_loc rho in
            { pat_desc = Tpat_var(id);
              pat_loc = ppat.ppat_loc;
              pat_tau = tau;
              pat_gamma = gamma }, rho
      | Ppat_alias(ppat1, name) ->
          let pat1, rho = type_pat sigma gamma ppat1 rho in
          let id, rho = PatternEnv.add name pat1.pat_tau ppat.ppat_loc rho in
            { pat_desc = Tpat_alias(pat1, id);
              pat_loc = ppat.ppat_loc;
              pat_tau = pat1.pat_tau;
              pat_gamma = gamma }, rho
      | Ppat_constant(c) ->
          { pat_desc = Tpat_constant(c);
            pat_loc = ppat.ppat_loc;
            pat_tau = type_constant c;
            pat_gamma = gamma }, rho
      | Ppat_tuple(ppatl) ->
          let patl, rho = (List.fold_left
                             (fun (patl, rho) ppat ->
                                let pat, rho = type_pat sigma gamma ppat rho in
                                  pat :: patl, rho)
                             ([], rho)
                             ppatl) in
          let patl = List.rev patl in
            { pat_desc = Tpat_tuple(patl);
              pat_loc = ppat.ppat_loc;
              pat_tau = Ttuple(List.map (fun pat -> pat.pat_tau) patl);
              pat_gamma = gamma }, rho
      (* TODO *)
      | Ppat_constraint(ppat, ptau) ->
          let tau = translate_type Open sigma gamma ptau in
          let pat, rho = type_pat sigma gamma ppat rho in
            unify pat.pat_tau tau;
            pat, rho
      (* TODO *)
      | _ -> assert false
  with
    | Unify_error(taupl) ->
        raise (Error(Pattern_type_mismatch(taupl, ppat.ppat_loc)))

let rec type_exp sigma gamma pe =
  match pe.pexp_desc with
    | Pexp_constant(c) ->
        { exp_desc = Texp_constant(c);
          exp_loc = pe.pexp_loc;
          exp_tau = type_constant c;
          exp_gamma = gamma }
    (* TODO *)
    | Pexp_tuple(pel) ->
        let el = List.map (type_exp sigma gamma) pel in
          { exp_desc = Texp_tuple(el);
            exp_loc = pe.pexp_loc;
            exp_tau = Ttuple(List.map (fun e -> e.exp_tau) el);
            exp_gamma = gamma }
    (* TODO *)
    | Pexp_ifthenelse(pe0, pe1, pe2) ->
        let e0 = solve_exp sigma gamma pe0 Predefined.type_bool in
          begin match pe2 with
            | None ->
                let e1 = solve_exp sigma gamma pe1 Predefined.type_unit in
                  { exp_desc = Texp_ifthenelse(e0, e1, None);
                    exp_loc = pe.pexp_loc;
                    exp_tau = e1.exp_tau;
                    exp_gamma = gamma }
            | Some(pe2) ->
                let e1 = type_exp sigma gamma pe1 in
                let e2 = solve_exp sigma gamma pe2 e1.exp_tau in
                  { exp_desc = Texp_ifthenelse(e0, e1, Some(e2));
                    exp_loc = pe.pexp_loc;
                    exp_tau = e1.exp_tau;
                    exp_gamma = gamma }
          end
    | Pexp_sequence(pe1, pe2) ->
        let e1 = solve_exp sigma gamma pe1 Predefined.type_unit in
        let e2 = type_exp sigma gamma pe2 in
          { exp_desc = Texp_sequence(e1, e2);
            exp_loc = pe.pexp_loc;
            exp_tau = e2.exp_tau;
            exp_gamma = gamma }
    | Pexp_constraint(pe, ptau) ->
        let tau = translate_type Open sigma gamma ptau in
          solve_exp sigma gamma pe tau
    | Pexp_when(pe1, pe2) ->
        let e1 = solve_exp sigma gamma pe1 Predefined.type_bool in
        let e2 = type_exp sigma gamma pe2 in
          { exp_desc = Texp_when(e1, e2);
            exp_loc = pe.pexp_loc;
            exp_tau = e2.exp_tau;
            exp_gamma = gamma }
    (* TODO *)
    | _ ->
        assert false

and solve_exp sigma gamma pe tau =
  try
    let e = type_exp sigma gamma pe in
      unify e.exp_tau tau;
      e
  with
    | Unify_error(taupl) ->
        raise (Error(Expression_type_mismatch(taupl, pe.pexp_loc)))
