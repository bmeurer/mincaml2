open Types

(* Qualified identifiers for predefined types *)
val ident_int: Ident.t
val ident_char: Ident.t
val ident_float: Ident.t
val ident_int32: Ident.t
val ident_int64: Ident.t
val ident_string: Ident.t
val ident_nativeint: Ident.t
val ident_exn: Ident.t
val ident_unit: Ident.t
val ident_bool: Ident.t
val ident_list: Ident.t
val ident_option: Ident.t

(* Qualified identifiers for predefined exceptions *)
val ident_match_failure: Ident.t
val ident_out_of_memory: Ident.t
val ident_invalid_argument: Ident.t
val ident_failure: Ident.t
val ident_not_found: Ident.t
val ident_sys_error: Ident.t
val ident_end_of_file: Ident.t
val ident_division_by_zero: Ident.t
val ident_stack_overflow: Ident.t
val ident_sys_blocked_io: Ident.t
val ident_assert_failure: Ident.t

(* Predefined types *)
val type_int: typ
val type_char: typ
val type_float: typ
val type_int32: typ
val type_int64: typ
val type_string: typ
val type_nativeint: typ
val type_exn: typ
val type_unit: typ
val type_bool: typ
val type_list: typ -> typ
val type_option: typ -> typ
val type_arrow: typ -> typ -> typ

(* Type of a type environment *)
type t

(* Lookup by qualified identifiers *)
val lookup_type: Ident.t -> t -> type_declaration
val lookup_value: Ident.t -> t -> value_description

(* Lookup by unqualified names *)
val find_cstr: string -> t -> constructor_description
val find_type: string -> t -> Ident.t * type_declaration
val find_value: string -> t -> Ident.t * value_description

(* Insertion by qualified identifier *)
val add_exn: Ident.t -> exn_declaration -> t -> t
val add_type: Ident.t -> type_declaration -> t -> t
val add_types: (Ident.t * type_declaration) list -> t -> t
val add_value: Ident.t -> value_description -> t -> t

(* The initial environment *)
val initial: t
