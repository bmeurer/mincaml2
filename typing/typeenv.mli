open Types

type t

val empty: t

(* Lookup by qualified identifiers *)
val lookup_type: Ident.t -> t -> type_declaration
val lookup_value: Ident.t -> t -> value_description

(* Lookup by unqualified names *)
val find_type: string -> t -> Ident.t * type_declaration
val find_value: string -> t -> Ident.t * value_description

(* Insertion by qualified identifier *)
val add_type: Ident.t -> type_declaration -> t -> t
val add_value: Ident.t -> value_description -> t -> t
