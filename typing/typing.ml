open Astcommon
open Parsedast
open Typedast
open Types


type error =
  | Duplicate_pattern_variable of string * Location.t
  | Expression_type_mismatch of (typ * typ) list * Location.t
  | Pattern_variable_missing of Ident.t * Location.t
  | Pattern_type_mismatch of (typ * typ) list * Location.t
  | Type_arity_mismatch of string * int * int * Location.t
  | Unbound_type_constructor of string * Location.t
  | Unbound_type_variable of string * Location.t

exception Error of error


(*********************)
(*** Miscellaneous ***)
(*********************)

(* StandardML criterion for let polymorphism *)
let rec is_value exp =
  match exp.exp_desc with
    | Texp_constant(_)
    | Texp_ident(_)
    | Texp_function(_) -> true
    | Texp_tuple(expl) -> List.for_all is_value expl
    | Texp_when(_, exp) -> is_value exp
    | _ -> false


(*******************)
(*** Unification ***)
(*******************)

exception Unify_error of (typ * typ) list

(* Check if tau0 appears in tau, lowering variable levels to level0 *)
let occur level0 tau0 tau =
  let rec occur_aux tau =
    match repr tau with
      | { typ_desc = Tvar(_) } as tau -> tau.typ_level <- min tau.typ_level level0; tau == tau0
      | { typ_desc = Tarrow(tau1, tau2) } -> occur_aux tau1 || occur_aux tau2
      | { typ_desc = Ttuple(taul) }
      | { typ_desc = Tconstruct(_, taul) } -> List.exists occur_aux taul
  in occur_aux tau

(* Expand tau if it is an abbreviation. Returns tau otherwise. *)
let expand gamma tau =
  match tau.typ_desc with
    | Tconstruct(ident, taul) ->
        begin try
          Types.expand (Typeenv.lookup_type ident gamma) taul
        with
          | Invalid_argument("Types.expand") -> tau
        end
    | _ ->
        tau

(* Unification *)
let rec unify gamma tau1 tau2 =
  if tau1 == tau2 then () else
    let tau1 = repr tau1 and tau2 = repr tau2 in
      if tau1 == tau2 then () else
        try
          match tau1.typ_desc, tau2.typ_desc with
            | Tvar(alpha1), Tvar(alpha2) ->
                if tau1.typ_level < tau2.typ_level then
                  begin
                    tau2.typ_level <- tau1.typ_level;
                    alpha2 := Some(tau1)
                  end
                else
                  begin
                    tau1.typ_level <- tau2.typ_level;
                    alpha1 := Some(tau2)
                  end
            | Tvar(alpha1), _ when not (occur tau1.typ_level tau1 tau2) ->
                alpha1 := Some(tau2)
            | _, Tvar(alpha2) when not (occur tau2.typ_level tau2 tau1) ->
                alpha2 := Some(tau1)
            | Tarrow(tau1, tau1'), Tarrow(tau2, tau2') ->
                unify gamma tau1 tau2;
                unify gamma tau1' tau2'
            | Ttuple(tau1l), Ttuple(tau2l) ->
                List.iter2 (unify gamma) tau1l tau2l
            | Tconstruct(ident1, tau1l), Tconstruct(ident2, tau2l) when Ident.equal ident1 ident2 ->
                List.iter2 (unify gamma) tau1l tau2l
            | _ ->
                (* expand abbreviations and try again if either type was actually expanded *)
                let tau1' = expand gamma tau1 in
                let tau2' = expand gamma tau2 in
                  if tau1 != tau1' || tau2 != tau2' then
                    unify gamma tau1' tau2'
                  else
                    raise (Unify_error([]))
        with
          | Invalid_argument("List.iter2") ->
              raise (Unify_error([tau1, tau2]))
          | Unify_error(taupl) ->
              raise (Unify_error((tau1, tau2) :: taupl))


(********************************)
(*** Translating parsed types ***)
(********************************)

let translated_type_vars = ref ([] : (string * typ) list)

let clear_translated_type_vars () =
  translated_type_vars := []

type policy =
  | Strict     (* fail on unknown type variable *)
  | Extensible (* add new type variables as they appear *)

let rec translate_type policy gamma ptau =
  match ptau.ptyp_desc with
    | Ptyp_var(name) ->
        begin try
          instantiate (List.assoc name !translated_type_vars)
        with
          | Not_found when policy = Extensible ->
              (* Named type variables are restricted to the global level *)
              let tau = new_global_var () in
                translated_type_vars := (name, tau) :: !translated_type_vars;
                tau
          | Not_found ->
              raise (Error(Unbound_type_variable(name, ptau.ptyp_loc)))
        end
    | Ptyp_arrow(ptau1, ptau2) ->
        new_typ (Tarrow(translate_type policy gamma ptau1, translate_type policy gamma ptau2))
    | Ptyp_tuple(ptaul) ->
        new_typ (Ttuple(List.map (translate_type policy gamma) ptaul))
    | Ptyp_construct(name, ptaul) ->
        (* TODO *)
        assert false


(****************************)
(*** Pattern environments ***)
(****************************)

module PatternEnv =
struct
  type t = (Ident.t * typ * Location.t) list

  let empty = []

  (* Add a new named pattern variable with the given type to the
     environment. Raise Duplicate_pattern_variable if the name is
     already known. Returns the qualified identifier and the new
     environment. *)
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

  (* Check if the environments rho1 and rho2 match on the names
     of their declared identifiers and generate a list which maps
     the identifiers of rho2 to their counterparts in rho1 *)
  let unify gamma rho1 rho2 =
    let rec unify_aux rho1 rho2 accu =
      match rho1, rho2 with
        | [], [] ->
            accu
        | (id1, tau1, loc1) :: rho1, (id2, tau2, loc2) :: rho2 ->
            let c = String.compare (Ident.name id1) (Ident.name id2) in
              if c = 0 then
                try
                  unify gamma tau2 tau1;
                  unify_aux rho1 rho2 ((id2, id1) :: accu)
                with
                  | Unify_error(taupl) -> raise (Error(Pattern_type_mismatch(taupl, loc2)))
              else
                let id, loc = if c < 0 then id1, loc1 else id2, loc2 in
                  raise (Error(Pattern_variable_missing(id, loc)))
        | (id, _, loc) :: _, _
        | _, (id, _, loc) :: _ ->
            raise (Error(Pattern_variable_missing(id, loc)))
    in unify_aux rho1 rho2 []

  (* Merge the pattern environment rho into the type environment
     gamma and return the extended type environment. *)
  let rec merge gamma = function
    | [] -> gamma
    | (id, tau, _) :: rho -> merge (Typeenv.add_value id { val_kind = Val_regular; val_tau = tau } gamma) rho
end


(**********************)
(*** Type inference ***)
(**********************)

let type_constant c =
  instantiate (match c with
                 | Const_int(_) -> Typeenv.type_int
                 | Const_char(_) -> Typeenv.type_char
                 | Const_float(_) -> Typeenv.type_float
                 | Const_int32(_) -> Typeenv.type_int32
                 | Const_int64(_) -> Typeenv.type_int64
                 | Const_string(_) -> Typeenv.type_string
                 | Const_nativeint(_) -> Typeenv.type_nativeint)

let rec type_pat gamma ppat rho =
  match ppat.ppat_desc with
    | Ppat_any ->
        { pat_desc = Tpat_any;
          pat_loc = ppat.ppat_loc;
          pat_tau = new_var ();
          pat_gamma = gamma }, rho
    | Ppat_var(name) ->
        let tau = new_var () in
        let id, rho = PatternEnv.add name tau ppat.ppat_loc rho in
          { pat_desc = Tpat_var(id);
            pat_loc = ppat.ppat_loc;
            pat_tau = tau;
            pat_gamma = gamma }, rho
    | Ppat_alias(ppat1, name) ->
        let pat1, rho = type_pat gamma ppat1 rho in
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
        let patl, rho = type_pat_list gamma ppatl rho in
          { pat_desc = Tpat_tuple(patl);
            pat_loc = ppat.ppat_loc;
            pat_tau = new_typ (Ttuple(List.map (fun pat -> pat.pat_tau) patl));
            pat_gamma = gamma }, rho
    | Ppat_construct(name, ppat') ->
        (* TODO *)
        assert false
    | Ppat_or(ppat1, ppat2) ->
        let pat1, rho1 = type_pat gamma ppat1 rho in
        let pat2, rho2 = solve_pat gamma ppat2 pat1.pat_tau rho in
        let map2to1 = PatternEnv.unify gamma rho1 rho2 in
          { pat_desc = Tpat_or(pat1, pattern_map_idents (fun id -> List.assq id map2to1) pat2);
            pat_loc = ppat.ppat_loc;
            pat_tau = pat1.pat_tau;
            pat_gamma = gamma }, rho1
    | Ppat_constraint(ppat, ptau) ->
        solve_pat gamma ppat (translate_type Extensible gamma ptau) rho

and type_pat_list gamma ppatl rho =
  let patl, rho = (List.fold_left
                     (fun (patl, rho) ppat ->
                        let pat, rho = type_pat gamma ppat rho in
                          pat :: patl, rho)
                     ([], rho)
                     ppatl) in
    List.rev patl, rho

and solve_pat gamma ppat tau rho =
  try
    let pat, rho = type_pat gamma ppat rho in
      unify gamma pat.pat_tau tau;
      pat, rho
  with
    | Unify_error(taupl) ->
        raise (Error(Pattern_type_mismatch(taupl, ppat.ppat_loc)))

and type_exp gamma pexp =
  match pexp.pexp_desc with
    | Pexp_constant(c) ->
        { exp_desc = Texp_constant(c);
          exp_loc = pexp.pexp_loc;
          exp_tau = type_constant c;
          exp_gamma = gamma }
    | Pexp_ident(name) ->
        let id, value = Typeenv.find_value name gamma in
          { exp_desc = Texp_ident(id, value);
            exp_loc = pexp.pexp_loc;
            exp_tau = instantiate value.val_tau;
            exp_gamma = gamma }
    | Pexp_let(rec_flag, pcases, pexp') ->
        let gamma', cases = type_let gamma rec_flag pcases in
        let exp = type_exp gamma' pexp' in
          { exp_desc = Texp_let(rec_flag, cases, exp);
            exp_loc = pexp.pexp_loc;
            exp_tau = exp.exp_tau;
            exp_gamma = gamma }
    | Pexp_function(pcases) ->
        let tau = new_var () in
        let tau' = new_var () in
        let cases, partial = solve_cases gamma pcases tau tau' in
          { exp_desc = Texp_function(cases, partial);
            exp_loc = pexp.pexp_loc;
            exp_tau = new_typ (Tarrow(tau, tau'));
            exp_gamma = gamma }
    | Pexp_apply(pexp', pexpl) ->
        let tau = new_var () in
        let expl = List.map (type_exp gamma) pexpl in
        let tau' = List.fold_right (fun exp tau -> new_typ (Tarrow(exp.exp_tau, tau))) expl tau in
        let exp = solve_exp gamma pexp' tau' in
          { exp_desc = Texp_apply(exp, expl);
            exp_loc = pexp.pexp_loc;
            exp_tau = tau;
            exp_gamma = gamma }
    | Pexp_match(pexp', pcases) ->
        let tau = new_var () in
        let exp = type_exp gamma pexp' in
        let cases, partial = solve_cases gamma pcases exp.exp_tau tau in
          { exp_desc = Texp_match(exp, cases, partial);
            exp_loc = pexp.pexp_loc;
            exp_tau = tau;
            exp_gamma = gamma }
    | Pexp_try(pexp', pcases) ->
        let exp = type_exp gamma pexp' in
        let cases, _ = solve_cases gamma pcases (instantiate Typeenv.type_exn) exp.exp_tau in
          { exp_desc = Texp_try(exp, cases);
            exp_loc = pexp.pexp_loc;
            exp_tau = exp.exp_tau;
            exp_gamma = gamma }
    | Pexp_tuple(pexpl) ->
        let expl = List.map (type_exp gamma) pexpl in
          { exp_desc = Texp_tuple(expl);
            exp_loc = pexp.pexp_loc;
            exp_tau = new_typ (Ttuple(List.map (fun exp -> exp.exp_tau) expl));
            exp_gamma = gamma }
    | Pexp_construct(name, pexp') ->
        assert false
    | Pexp_ifthenelse(pexp0, pexp1, pexp2) ->
        let exp0 = solve_exp gamma pexp0 (instantiate Typeenv.type_bool) in
          begin match pexp2 with
            | None ->
                let exp1 = solve_exp gamma pexp1 (instantiate Typeenv.type_unit) in
                  { exp_desc = Texp_ifthenelse(exp0, exp1, None);
                    exp_loc = pexp.pexp_loc;
                    exp_tau = exp1.exp_tau;
                    exp_gamma = gamma }
            | Some(pexp2) ->
                let exp1 = type_exp gamma pexp1 in
                let exp2 = solve_exp gamma pexp2 exp1.exp_tau in
                  { exp_desc = Texp_ifthenelse(exp0, exp1, Some(exp2));
                    exp_loc = pexp.pexp_loc;
                    exp_tau = exp1.exp_tau;
                    exp_gamma = gamma }
          end
    | Pexp_sequence(pexp1, pexp2) ->
        let exp1 = solve_exp gamma pexp1 (instantiate Typeenv.type_unit) in
        let exp2 = type_exp gamma pexp2 in
          { exp_desc = Texp_sequence(exp1, exp2);
            exp_loc = pexp.pexp_loc;
            exp_tau = exp2.exp_tau;
            exp_gamma = gamma }
    | Pexp_constraint(pexp, ptau) ->
        solve_exp gamma pexp (translate_type Extensible gamma ptau)
    | Pexp_when(pexp1, pexp2) ->
        let exp1 = solve_exp gamma pexp1 (instantiate Typeenv.type_bool) in
        let exp2 = type_exp gamma pexp2 in
          { exp_desc = Texp_when(exp1, exp2);
            exp_loc = pexp.pexp_loc;
            exp_tau = exp2.exp_tau;
            exp_gamma = gamma }

and solve_exp gamma pexp tau =
  try
    let exp = type_exp gamma pexp in
      unify gamma exp.exp_tau tau;
      exp
  with
    | Unify_error(taupl) ->
        raise (Error(Expression_type_mismatch(taupl, pexp.pexp_loc)))

and solve_cases gamma pcases tau tau' =
  let cases = (List.map
                 (fun (ppat, pexp) ->
                    let pat, rho = solve_pat gamma ppat tau PatternEnv.empty in
                    let gamma' = PatternEnv.merge gamma rho in
                    let exp = solve_exp gamma' pexp tau' in
                      pat, exp)
                 pcases) in
    (* TODO - check if pattern is full *)
    cases, Partial

and type_let gamma rec_flag pcases =
  let gamma', cases =
    (try
       increase_typ_level ();
       let patl, rho = type_pat_list gamma (List.map fst pcases) PatternEnv.empty in
       let gamma' = PatternEnv.merge gamma rho in
       let gamma1 = (match rec_flag with
                       | Recursive -> gamma'
                       | NonRecursive -> gamma) in
       let cases = (List.map2
                      (fun pat (_, pexp) -> pat, solve_exp gamma1 pexp pat.pat_tau)
                      patl
                      pcases) in
         decrease_typ_level ();
         gamma', cases
     with
       | exn ->
           decrease_typ_level ();
           raise exn) in
    List.iter (fun (pat, exp) -> if not (is_value exp) then nongeneralize pat.pat_tau) cases;
    List.iter (fun (pat, exp) -> generalize pat.pat_tau) cases;
    gamma', cases

and type_structure_item gamma pstr =
  clear_translated_type_vars ();
  match pstr.pstr_desc with
    | Pstr_exp(pexp) ->
        let exp = type_exp gamma pexp in
          { str_desc = Tstr_exp(exp);
            str_loc = pstr.pstr_loc;
            str_gamma = gamma }, gamma
    | Pstr_let(rec_flag, pcases) ->
        let gamma', cases = type_let gamma rec_flag pcases in
          { str_desc = Tstr_let(rec_flag, cases);
            str_loc = pstr.pstr_loc;
            str_gamma = gamma }, gamma'
    | Pstr_typ(_) ->
        (* TODO *)
        assert false
    | Pstr_exn(_) ->
        (* TODO *)
        assert false

and type_structure gamma pstrl =
  match pstrl with
    | [] ->
        []
    | pstr :: pstrl ->
        let str, gamma = type_structure_item gamma pstr in
          str :: type_structure gamma pstrl
