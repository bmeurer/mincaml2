open Types

(* Predefined types *)
val type_int: type_expr
val type_char: type_expr
val type_float: type_expr
val type_int32: type_expr
val type_int64: type_expr
val type_string: type_expr
val type_nativeint: type_expr
val type_exn: type_expr
val type_unit: type_expr
val type_bool: type_expr
val type_list: type_expr -> type_expr
val type_option: type_expr -> type_expr

(* Type of a type environment *)
type t

(* Lookup by qualified identifiers *)
val lookup_type: Ident.t -> t -> type_declaration
val lookup_value: Ident.t -> t -> value_description

(* Lookup by unqualified names *)
val find_type: string -> t -> Ident.t * type_declaration
val find_value: string -> t -> Ident.t * value_description

(* Insertion by qualified identifier *)
val add_type: Ident.t -> type_declaration -> t -> t
val add_value: Ident.t -> value_description -> t -> t
