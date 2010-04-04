type t

val create: string -> t
val create_tmp: int -> t
val name: t -> string
val unique_name: t -> string
val compare: t -> t -> int
val equal: t -> t -> bool

val create_unique: string -> t

val create_predefined_exn: string -> int -> t
val is_predefined_exn: t -> bool
val predefined_exn_tag: t -> int

val create_persistent: string -> t
val is_persistent: t -> bool
val make_persistent: t -> unit
val persistent_name: t -> string

val print: Format.formatter -> t -> unit
