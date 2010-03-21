open Astcommon
open Parsedast
open Typedast
open Types


type error =
  | Constructor_arity_mismatch of Ident.t * int * int * Location.t
  | Cyclic_abbreviation of Ident.t * Location.t
  | Duplicate_constructor of string * Location.t
  | Duplicate_pattern_variable of string * Location.t
  | Duplicate_type_constructor of string * Location.t
  | Duplicate_type_param of string * Location.t
  | Expression_type_mismatch of (typ * typ) list * Location.t
  | Pattern_variable_missing of Ident.t * Location.t
  | Pattern_type_mismatch of (typ * typ) list * Location.t
  | Type_arity_mismatch of Ident.t * int * int * Location.t
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
                (* Expand abbreviations and try again if either type was actually expanded.
                   We will not recurse forever, as type abbreviations are guaranteed to be
                   non-cyclic (see the checks in translate_type_decls). *)
                let tau1_expanded = expand gamma tau1 in
                let tau2_expanded = expand gamma tau2 in
                  if tau1 != tau1_expanded || tau2 != tau2_expanded then
                    unify gamma tau1_expanded tau2_expanded
                  else
                    raise (Unify_error([]))
        with
          | Invalid_argument("List.iter2") ->
              raise (Unify_error([tau1, tau2]))
          | Unify_error(taupl) ->
              raise (Unify_error((tau1, tau2) :: taupl))


(******************************************************)
(*** Translating parsed types and type declarations ***)
(******************************************************)

let translated_type_vars = ref ([] : (string * typ) list)

(* Clear the list of translated type variables. *)
let clear_translated_type_vars () =
  translated_type_vars := []

(* Bind type variables to all param_names. Make sure each type parameter name
   is unique. *)
let bind_translated_type_vars loc param_names =
  clear_translated_type_vars ();
  let rec bind_translated_type_vars_aux = function
    | [] ->
        []
    | name :: names ->
        if List.mem name names then
          raise (Error(Duplicate_type_param(name, loc)));
        let tau = new_global_var () in
          translated_type_vars := (name, tau) :: !translated_type_vars;
          tau :: bind_translated_type_vars_aux names
  in bind_translated_type_vars_aux param_names

type policy =
  | Strict     (* fail on unknown type variable *)
  | Extensible (* add new type variables as they appear *)

(* TODO *)
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
        begin try
          let id, decl = Typeenv.find_type name gamma in
          let taul = List.map (translate_type policy gamma) ptaul in
          let taul_length = List.length taul in
            if taul_length <> decl.type_arity then
              raise (Error(Type_arity_mismatch(id, decl.type_arity, taul_length, ptau.ptyp_loc)))
            else
              new_typ (Tconstruct(id, taul))
        with
          | Not_found ->
              raise (Error(Unbound_type_constructor(name, ptau.ptyp_loc)))
        end

(* Translate a list of (name, parsed decl) pairs into a list of (ident, typed decl)
   triples with the typed decls set to Type_abstract. Ensures that type constructor
   names are unique. The list is returned in reverse order (tail-recursive) *)
let pre_translate_type_decls pnameddecls =
  let rec pre_translate_type_decls_aux pnameddecls accu =
    match pnameddecls with
      | [] ->
          accu
      | (name, pdecl) :: pnameddecls ->
          if List.mem_assoc name pnameddecls then
            raise (Error(Duplicate_type_constructor(name, pdecl.ptype_loc)));
          let ident = Ident.create name in
          let decl = { type_params = [];
                       type_arity = List.length pdecl.ptype_params;
                       type_desc = Type_abstract;
                       type_loc = pdecl.ptype_loc } in
            pre_translate_type_decls_aux pnameddecls ((ident, decl) :: accu)
  in pre_translate_type_decls_aux pnameddecls []

(* Translate a list of parsed type variants into a list of type variants. Ensure
   that each constructor name is unique within the list of constructor names. *)
let rec translate_type_variants pre_gamma = function
  | [] ->
      []
  | (name, ptaul, loc) :: pvariants ->
      let variants = translate_type_variants pre_gamma pvariants in
        if List.mem_assoc name variants then
          raise (Error(Duplicate_constructor(name, loc)));
        (name, List.map (translate_type Strict pre_gamma) ptaul) :: variants

(* Translate a single type declaration using the pre-environment pre_gamma. The returnd
   type declaration is generalized using the type parameters in pdecl. *)
let translate_type_decl pre_gamma pdecl =
  let level = enter_typ_level () in
    try
      let type_params = bind_translated_type_vars pdecl.ptype_loc pdecl.ptype_params in
      let type_desc = (match pdecl.ptype_desc with
                         | Ptype_abstract ->
                             leave_typ_level level;
                             Type_abstract
                         | Ptype_abbrev(ptau) ->
                             let tau = translate_type Strict pre_gamma ptau in
                               leave_typ_level level;
                               generalize tau;
                               Type_abbrev(tau)
                         | Ptype_variant(pvariants) ->
                             let variants = translate_type_variants pre_gamma pvariants in
                               leave_typ_level level;
                               List.iter (fun (_, taul) -> List.iter generalize taul) variants;
                               Type_variant(variants)) in
        List.iter generalize type_params;
        { type_params = type_params;
          type_arity = List.length type_params;
          type_desc = type_desc;
          type_loc = pdecl.ptype_loc }
    with
      | exn ->
          leave_typ_level level;
          raise exn

(* Check for cyclic abbreviations in the list of (qualified identifier, type declaration) pairs.
   All references to unknown qualified identifiers are assumed to be non-cyclic. Cycles within
   variants are permitted (actually the only way to introduce recursive types), but cycles through
   abbreviations must be rejected, otherwise unify may not terminate. *)
let check_abbrev_decls iddecls =
  let rec check_abbrev_decl id decl =
    let rec check_abbrev_typ tau =
      match tau.typ_desc with
        | Tvar(_) ->
            ()
        | Tarrow(tau1, tau2) ->
            check_abbrev_typ tau1; check_abbrev_typ tau2
        | Ttuple(taul) ->
            List.iter check_abbrev_typ taul
        | Tconstruct(id', taul) ->
            try
              let _, decl' = List.find (fun (id, _) -> Ident.equal id id') iddecls in
                if decl' == decl then
                  raise (Error(Cyclic_abbreviation(id, decl.type_loc)))
                else
                  check_abbrev_decl id decl'
            with
              | Not_found -> () (* reference to unknown type decl, assumed to be non-cyclic *)
    in match decl.type_desc with
      | Type_abbrev(tau) -> check_abbrev_typ tau
      | _ -> ()
  in List.iter (fun (id, decl) -> check_abbrev_decl id decl) iddecls
  
(* Translate a list of (name, parsed decl) pairs into a list of (ident, type decl) pairs.
   This does all the nasty work, including the cyclic abbreviation checks necessary to
   ensure that unify will terminate. *)
let translate_type_decls gamma pnameddecls =
  let pre_iddecls = pre_translate_type_decls pnameddecls in
  let pre_gamma = Typeenv.add_types pre_iddecls gamma in
  (* Really translate the decls now, using rev_map2 because pre_translate_type_decls returns
     the list in reverse order. *)
  let iddecls = (List.rev_map2
                   (fun (ident, _) (_, pdecl) -> ident, translate_type_decl pre_gamma pdecl)
                   pre_iddecls
                   (List.rev pnameddecls)) in
    (* Check for cyclic abbreviations *)
    check_abbrev_decls iddecls;
    iddecls


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

(* Determine an instance of the constants type *)
let type_constant c =
  instantiate (match c with
                 | Const_int(_) -> Typeenv.type_int
                 | Const_char(_) -> Typeenv.type_char
                 | Const_float(_) -> Typeenv.type_float
                 | Const_int32(_) -> Typeenv.type_int32
                 | Const_int64(_) -> Typeenv.type_int64
                 | Const_string(_) -> Typeenv.type_string
                 | Const_nativeint(_) -> Typeenv.type_nativeint)

(* TODO *)
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
        begin try
          let id, cstr = Typeenv.find_cstr name gamma in
          let ppatl = (match ppat' with
                         | None ->
                             []
                         | Some({ ppat_desc = Ppat_tuple(ppatl) }) when cstr.cstr_arity > 1 ->
                             ppatl
                         | Some({ ppat_desc = Ppat_any } as ppat) when cstr.cstr_arity <> 1 ->
                             Array.to_list (Array.make cstr.cstr_arity ppat)
                         | Some(ppat) ->
                             [ppat]) in
          let ppatl_length = List.length ppatl in
            if ppatl_length <> cstr.cstr_arity then
              raise (Error(Constructor_arity_mismatch(id, cstr.cstr_arity, ppatl_length, ppat.ppat_loc)));
            let taul, tau = instantiate_cstr cstr in
            let patl, rho = solve_pat_list gamma ppatl taul rho in
              { pat_desc = Tpat_construct(id, patl);
                pat_loc = ppat.ppat_loc;
                pat_tau = tau;
                pat_gamma = gamma }, rho
        with
          | Not_found -> raise (Error(Unbound_type_constructor(name, ppat.ppat_loc)))
        end
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

and solve_pat_list gamma ppatl taul rho =
  try
    let patl, rho = (List.fold_left2
                       (fun (patl, rho) ppat tau ->
                          let pat, rho = solve_pat gamma ppat tau rho in
                            pat :: patl, rho)
                       ([], rho)
                       ppatl
                       taul) in
      List.rev patl, rho
  with
    | Invalid_argument("List.fold_left2") -> invalid_arg "Typing.solve_pat_list"

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
        begin try
          let id, cstr = Typeenv.find_cstr name gamma in
          let pexpl = (match pexp' with
                         | None -> []
                         | Some({ pexp_desc = Pexp_tuple(pexpl) }) when cstr.cstr_arity > 1 -> pexpl
                         | Some(pexp) -> [pexp]) in
          let pexpl_length = List.length pexpl in
            if pexpl_length <> cstr.cstr_arity then
              raise (Error(Constructor_arity_mismatch(id, cstr.cstr_arity, pexpl_length, pexp.pexp_loc)));
            let taul, tau = instantiate_cstr cstr in
            let expl = solve_exp_list gamma pexpl taul in
              { exp_desc = Texp_construct(id, expl);
                exp_loc = pexp.pexp_loc;
                exp_tau = tau;
                exp_gamma = gamma }
        with 
          | Not_found -> raise (Error(Unbound_type_constructor(name, pexp.pexp_loc)))
        end
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

and solve_exp_list gamma pexpl taul =
  try
    List.map2 (solve_exp gamma) pexpl taul
  with
    | Invalid_argument("List.map2") -> invalid_arg "Typing.solve_exp_list"

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
  let level = enter_typ_level () in
    try
      let patl, rho = type_pat_list gamma (List.map fst pcases) PatternEnv.empty in
      let gamma' = PatternEnv.merge gamma rho in
      let gamma1 = (match rec_flag with
                      | Recursive -> gamma'
                      | NonRecursive -> gamma) in
      let cases = (List.map2
                     (fun pat (_, pexp) -> pat, solve_exp gamma1 pexp pat.pat_tau)
                     patl
                     pcases) in
        leave_typ_level level;
        List.iter (fun (pat, exp) -> if not (is_value exp) then nongeneralize pat.pat_tau) cases;
        List.iter (fun (pat, exp) -> generalize pat.pat_tau) cases;
        gamma', cases
    with
      | exn ->
          leave_typ_level level;
          raise exn

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
    | Pstr_typ(pnameddecls) ->
        let iddecls = translate_type_decls gamma pnameddecls in
          { str_desc = Tstr_typ(iddecls);
            str_loc = pstr.pstr_loc;
            str_gamma = gamma }, Typeenv.add_types iddecls gamma
    | Pstr_exn(_) ->
        (* TODO *)
        assert false

and type_structure gamma pstrl =
  let rec type_structure_aux gamma pstrl accu =
    match pstrl with
      | [] ->
          []
      | pstr :: pstrl ->
          let str, gamma = type_structure_item gamma pstr in
            type_structure_aux gamma pstrl (str :: accu)
  in List.rev (type_structure_aux gamma pstrl [])
