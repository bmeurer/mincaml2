open Types

val print_typ: Format.formatter -> typ -> unit
val report_unification_error: Format.formatter -> (typ * typ) list -> string -> string -> unit
