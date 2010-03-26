open Types

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
