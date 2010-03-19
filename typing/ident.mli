type t

val create: string -> t
val name: t -> string
val same: t -> t -> bool
val equal: t -> t -> bool
