open Astcommon
open Types

type pattern =
    { pat_desc:  pattern_desc;
      pat_loc:   Location.t;
      pat_tau:   type_expr;
      pat_gamma: Typeenv.t }

and pattern_desc =
  | Tpat_any
  | Tpat_var of Ident.t
  | Tpat_alias of pattern * Ident.t
  | Tpat_constant of constant
  | Tpat_tuple of pattern list
  (* TODO *)
  | Tpat_or of pattern * pattern

and expression =
    { exp_desc:  expression_desc;
      exp_loc:   Location.t;
      exp_tau:   type_expr;
      exp_gamma: Typeenv.t }

and partial =
  | Partial
  | Total

and expression_desc =
  | Texp_constant of constant
  | Texp_ident of Ident.t
  | Texp_let of rec_flag * (pattern * expression) list * expression
  | Texp_function of (pattern * expression) list * partial
  | Texp_apply of expression * expression list
  (* TODO *)
  | Texp_try of expression * (pattern * expression) list
  | Texp_tuple of expression list
  (* TODO *)
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_when of expression * expression
