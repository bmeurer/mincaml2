type typ =
    { typ_desc: typ_desc;
      mutable typ_level: int }

and typ_desc =
  | Tvar of typ option ref
  | Tarrow of typ * typ
  | Ttuple of typ list
  | Tconstruct of Ident.t * typ list

and type_declaration =
    { type_params:       typ list;
      type_arity:        int;
      mutable type_desc: type_declaration_desc } (* mutable to support the open exn type *)

and type_declaration_desc =
  | Type_abstract
  | Type_abbrev of typ
  | Type_variant of (Ident.t * typ list) list

and exn_declaration =
    typ list

and constructor_description =
    { cstr_type:  typ;
      cstr_args:  typ list;
      cstr_arity: int;
      cstr_tag:   int }

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
let new_generic_var () = new_generic_typ (Tvar(ref None))
let new_global_var () = new_global_typ (Tvar(ref None))

type typ_level = int

let enter_typ_level () =
  let level = !current_level in
    current_level := level + 1;
    level

let leave_typ_level level =
  current_level := level


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
  let tau' = duplicate tau in
    cleanup tau;
    tau'

(* Instantiate a constructor *)
let instantiate_cstr cstr =
  let cstr_type = duplicate cstr.cstr_type in
  let cstr_args = List.map duplicate cstr.cstr_args in
    cleanup cstr.cstr_type;
    List.iter cleanup cstr.cstr_args;
    (cstr_args, cstr_type)


(*********************)
(*** Abbreviations ***)
(*********************)

(* Bind a type variable to a type *)
let bind tau1 tau2 =
  match tau1.typ_desc with
    | Tvar({ contents = None } as alpha1) -> alpha1 := Some(tau2)
    | _ -> invalid_arg "Types.bind"

(* Expand abbreviations, raise Invalid_argument("Types.expand") if decl is not an abbreviation. *)
let expand decl taul =
  match decl.type_desc with
    | Type_abbrev(abbrev) ->
        let type_params = List.map duplicate decl.type_params in
        let type_abbrev = duplicate abbrev in
          List.iter cleanup decl.type_params;
          cleanup abbrev;
          List.iter2 bind type_params taul;
          type_abbrev
    | _ ->
        invalid_arg "Types.expand"



