open Types

type error =
  | Constructor_arity_mismatch of Ident.t * int * int
  | Cyclic_abbreviation of Ident.t
  | Duplicate_constructor of string
  | Duplicate_pattern_variable of string
  | Duplicate_type_constructor of string
  | Duplicate_type_param of string
  | Expression_type_mismatch of (typ * typ) list
  | Pattern_variable_missing of Ident.t
  | Pattern_type_mismatch of (typ * typ) list
  | Type_arity_mismatch of Ident.t * int * int
  | Unbound_type_constructor of string
  | Unbound_type_variable of string
  | Unbound_value of string

exception Error of error * Location.t


val type_structure: Typeenv.t -> Parsedast.structure -> Typedast.structure
