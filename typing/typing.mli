open Types

type error =
  | Constructor_arity_mismatch of string * int * int
  | Cyclic_abbreviation of string
  | Duplicate_constructor of string
  | Duplicate_pattern_variable of string
  | Duplicate_type_constructor of string
  | Duplicate_type_parameter of string
  | Expression_type_mismatch of (typ * typ) list
  | External_identifier_not_a_function of string
  | Invalid_primitive_declaration of string
  | Pattern_variable_missing of string
  | Pattern_type_mismatch of (typ * typ) list
  | Too_many_constructors
  | Type_arity_mismatch of string * int * int
  | Unbound_constructor of string
  | Unbound_type_constructor of string
  | Unbound_type_variable of string
  | Unbound_value of string

exception Error of error * Location.t

val report_error: Format.formatter -> error -> unit

val expand_head: Typeenv.t -> typ -> typ

val type_exp: Typeenv.t -> Parsedast.expression -> Typedast.expression
val type_structure: Typeenv.t -> Parsedast.structure -> Typedast.structure
