open Astcommon
open Lambda
open Typedast

type error =
  | Illegal_letrec_expression
  | Illegal_letrec_pattern

exception Error of error * Location.t

val report_error: Format.formatter -> error -> unit

val translate_exp: expression -> lambda
val translate_structure: structure -> lambda
