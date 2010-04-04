type t

val create: string -> t
val create_persistent: string -> t
val create_predefined: string -> t
val create_tmp: int -> t
val name: t -> string
val unique_name: t -> string
val compare: t -> t -> int
val equal: t -> t -> bool

val is_persistent: t -> bool
val make_persistent: t -> unit
val persistent_name: t -> string

val print: Format.formatter -> t -> unit
