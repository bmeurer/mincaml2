open Types

type t

val empty: t

(* Lookup by qualified identifiers *)
val lookup_type: Ident.t -> t -> type_declaration

(* Lookup by unqualified names *)
val find_type: string -> t -> Ident.t * type_declaration

(* Insertion by qualified identifier *)
val add_type: Ident.t -> type_declaration -> t -> t
