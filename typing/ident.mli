type t

val create: string -> t
val name: t -> string
val compare: t -> t -> int
val equal: t -> t -> bool

