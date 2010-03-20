type typ =
    { typ_desc: typ_desc;
      mutable typ_level: int }

and typ_desc =
  | Tvar of typ option ref
  | Tarrow of typ * typ
  | Ttuple of typ list
  | Tconstruct of Ident.t * typ list

type type_declaration =
    { type_params: typ list;
      type_arity:  int;
      type_desc:   type_declaration_desc;
      type_typ:    typ option }

and type_declaration_desc =
  | Type_abstr
  | Type_variant of (string * typ list) list

and constructor_description =
    { cstr_type:  typ;
      cstr_args:  typ list;
      cstr_arity: int }

and value_description =
    { val_kind: value_kind;
      val_tau:  typ }

and value_kind =
  | Val_regular


(*****************************)
(*** Type level management ***)
(*****************************)

let generic_level = -1
let nongeneric_level = 0
let global_level = nongeneric_level + 1 (* for named type variables *)
let current_level = ref nongeneric_level

let new_typ_with_level desc level = { typ_desc = desc; typ_level = level }
let new_typ desc = new_typ_with_level desc !current_level
let new_generic_typ desc = new_typ_with_level desc generic_level
let new_global_typ desc = new_typ_with_level desc global_level
let new_var () = new_typ (Tvar(ref None))
let new_global_var () = new_global_typ (Tvar(ref None))


(************)
(*** Misc ***)
(************)

(* Determine type representation and do path compression *) 
let rec repr tau =
  match tau.typ_desc with
    | Tvar({ contents = Some(tau) } as alpha) -> let tau = repr tau in alpha := Some(tau); tau
    | _ -> tau


(************************************)
(*** Generalization/Instantiation ***)
(************************************)

(* Generalize the type tau *)
let rec generalize tau =
  let tau = repr tau in
    match tau.typ_desc with
      | Tvar(_) ->
          if tau.typ_level > !current_level then
            tau.typ_level <- generic_level
      | Tarrow(tau1, tau2) ->
          generalize tau1;
          generalize tau2;
          tau.typ_level <- min tau1.typ_level tau2.typ_level
      | Ttuple(taul)
      | Tconstruct(_, taul) ->
          List.iter generalize taul;
          tau.typ_level <- List.fold_left (fun lvl tau -> min lvl tau.typ_level) nongeneric_level taul

(* Lower the level of all generalizable variables in tau to make them non-generalizable *)
let rec nongeneralize tau =
  let tau = repr tau in
    match tau.typ_desc with
      | Tvar(_) ->
          let lvl = !current_level in
            if tau.typ_level > lvl then
              tau.typ_level <- lvl
      | Tarrow(tau1, tau2) ->
          nongeneralize tau1;
          nongeneralize tau2
      | Ttuple(taul)
      | Tconstruct(_, taul) ->
          List.iter nongeneralize taul

(* All generic variable have their reference field set to "None", so we can
   reuse that field to store a reference to the new type variable which is
   the instance of the generic one. cleanup will restore the reference field
   to "None" once the duplicate is ready. *)
let rec duplicate tau =
  let duplicate_generic tau =
    match tau.typ_desc with
      | Tvar({ contents = None } as alpha) -> 
          let tau = new_var () in
            alpha := Some(tau);
            tau
      | Tvar({ contents = Some(tau) }) ->
          tau
      | Tarrow(tau1, tau2) ->
          new_typ_with_level (Tarrow(duplicate tau1, duplicate tau2)) nongeneric_level
      | Ttuple(taul) ->
          new_typ_with_level (Ttuple(List.map duplicate taul)) nongeneric_level
      | Tconstruct(cstr, taul) ->
          new_typ_with_level (Tconstruct(cstr, List.map duplicate taul)) nongeneric_level
  and duplicate_nongeneric tau =
    match tau.typ_desc with
      | Tvar({ contents = None }) -> tau
      | Tvar({ contents = Some(tau) }) -> duplicate tau
      | _ -> tau
  in 
    if tau.typ_level = generic_level then
      duplicate_generic tau
    else
      duplicate_nongeneric tau

(* Restore reference field of generic variables after duplicating. *)
let rec cleanup tau =
  let cleanup_generic tau =
    match tau.typ_desc with
      | Tvar({ contents = None }) -> ()
      | Tvar({ contents = Some(tau) } as alpha) -> alpha := None
      | Tarrow(tau1, tau2) -> cleanup tau1; cleanup tau2
      | Ttuple(taul)
      | Tconstruct(_, taul) -> List.iter cleanup taul
  and cleanup_nongeneric tau =
    match tau.typ_desc with
      | Tvar({ contents = Some(tau) }) -> cleanup tau
      | _ -> ()
  in
    if tau.typ_level = generic_level then
      cleanup_generic tau
    else
      cleanup_nongeneric tau

(* Instantiate a generic type *)
let instantiate tau =
  let tau = duplicate tau in
    cleanup tau;
    tau


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

let rec unify tau1 tau2 =
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
                unify tau1 tau2;
                unify tau1' tau2'
            | Ttuple(tau1l), Ttuple(tau2l) ->
                List.iter2 unify tau1l tau2l
            (* TODO - Tconstr *)
            | _ ->
                raise (Unify_error([]))
        with
          | Invalid_argument("List.iter2") ->
              raise (Unify_error([tau1, tau2]))
          | Unify_error(taupl) ->
              raise (Unify_error((tau1, tau2) :: taupl))
