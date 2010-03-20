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


val new_typ: typ_desc -> typ
val new_generic_typ: typ_desc -> typ
val new_var: unit -> typ
val new_global_var: unit -> typ

val generalize: typ -> unit
val instantiate: typ -> typ

exception Unify_error of (typ * typ) list
val unify: typ -> typ -> unit
