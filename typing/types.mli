type typ =
    { typ_desc: typ_desc;
      mutable typ_level: int }

and typ_desc =
  | Tvar of typ option ref
  | Tarrow of typ * typ
  | Ttuple of typ list
  | Tconstruct of Ident.t * typ list

type type_declaration =
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
  | Val_primitive of Primitive.description


val new_typ: typ_desc -> typ
val new_generic_typ: typ_desc -> typ
val new_var: unit -> typ
val new_global_var: unit -> typ
val new_generic_var: unit -> typ

type typ_level

val enter_typ_level: unit -> typ_level
val leave_typ_level: typ_level -> unit

val repr: typ -> typ
val arity: typ -> int

val nongeneralize: typ -> unit
val generalize: typ -> unit
val instantiate: typ -> typ
val instantiate_cstr: constructor_description -> typ list * typ

val expand: type_declaration -> typ list -> typ

