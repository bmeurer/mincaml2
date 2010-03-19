open Types

type t

val empty: t

val lookup_type: string -> t -> Ident.t * type_declaration
