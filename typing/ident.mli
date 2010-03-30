type t

val create: string -> t
val create_tmp: int -> t
val name: t -> string
val compare: t -> t -> int
val equal: t -> t -> bool

val print: Format.formatter -> t -> unit
