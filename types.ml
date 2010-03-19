type type_expr =
  | Tvar of type_variable
  | Tarrow of type_expr * type_expr
  | Ttuple of type_expr list
  | Tconstruct of Ident.t * type_expr list
  | Tpoly of type_variable * type_expr

and type_variable =
    type_expr option ref

type type_declaration =
    { type_params: type_variable list;
      type_arity:  int;
      type_desc:   type_declaration_desc;
      type_typ:    type_expr option }

and type_declaration_desc =
  | Type_abstr
  | Type_variant of (string * type_expr list) list

type constructor_description =
    { cstr_type:  type_expr;
      cstr_args:  type_expr list;
      cstr_arity: int }

let new_type_variable (_:unit): type_variable =
  ref None
