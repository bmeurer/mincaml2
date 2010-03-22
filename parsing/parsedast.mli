open Astcommon

type typ =
    { ptyp_desc: typ_desc;
      ptyp_loc:  Location.t }

and typ_desc =
  | Ptyp_var of string
  | Ptyp_arrow of typ * typ
  | Ptyp_tuple of typ list
  | Ptyp_construct of string * typ list

type pattern =
    { ppat_desc: pattern_desc;
      ppat_loc:  Location.t }

and pattern_desc =
  | Ppat_any
  | Ppat_var of string
  | Ppat_alias of pattern * string
  | Ppat_constant of constant
  | Ppat_tuple of pattern list
  | Ppat_construct of string * pattern option
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * typ

type expression =
    { pexp_desc: expression_desc;
      pexp_loc:  Location.t }

and expression_desc =
  | Pexp_constant of constant
  | Pexp_ident of string
  | Pexp_let of rec_flag * (pattern * expression) list * expression
  | Pexp_function of (pattern * expression) list
  | Pexp_apply of expression * expression list
  | Pexp_match of expression * (pattern * expression) list
  | Pexp_try of expression * (pattern * expression) list
  | Pexp_tuple of expression list
  | Pexp_construct of string * expression option
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_constraint of expression * typ
  | Pexp_when of expression * expression

and type_declaration =
    { ptype_params: string list;
      ptype_desc:   type_declaration_desc;
      ptype_loc:    Location.t }

and type_declaration_desc =
  | Ptype_abstract
  | Ptype_abbrev of typ
  | Ptype_variant of (string * typ list * Location.t) list

and exn_declaration =
    typ list

and structure_item =
    { pstr_desc: structure_item_desc;
      pstr_loc:  Location.t }

and structure_item_desc =
  | Pstr_exp of expression
  | Pstr_let of rec_flag * (pattern * expression) list
  | Pstr_typ of (string * type_declaration) list
  | Pstr_exn of string * exn_declaration
  | Pstr_external of string * typ * string list

and structure =
    structure_item list
