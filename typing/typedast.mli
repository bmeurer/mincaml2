open Astcommon
open Types

type pattern =
    { pat_desc:  pattern_desc;
      pat_loc:   Location.t;
      pat_tau:   typ;
      pat_gamma: Typeenv.t }

and pattern_desc =
  | Tpat_any
  | Tpat_ident of Ident.t
  | Tpat_alias of pattern * Ident.t
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  | Tpat_construct of constructor_description * pattern list
  | Tpat_or of pattern * pattern

and expression =
    { exp_desc:  expression_desc;
      exp_loc:   Location.t;
      exp_tau:   typ;
      exp_gamma: Typeenv.t }

and expression_desc =
  | Texp_constant of constant
  | Texp_ident of Ident.t * value_description
  | Texp_let of rec_flag * (pattern * expression) list * expression
  | Texp_function of (pattern * expression) list
  | Texp_apply of expression * expression list
  | Texp_match of expression * (pattern * expression) list
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  | Texp_construct of constructor_description * expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_when of expression * expression

and structure_item =
  | Tstr_exp of expression
  | Tstr_let of rec_flag * (pattern * expression) list
  | Tstr_typ of (Ident.t * type_declaration) list
  | Tstr_exn of Ident.t * exn_declaration
  | Tstr_external of Ident.t * value_description

and structure =
    structure_item list


val pattern_fv: pattern -> IdentSet.t
val pattern_map_idents: (Ident.t -> Ident.t) -> pattern -> pattern

