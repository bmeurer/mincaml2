open Lambda
open Typedast

val translate_match: lambda -> lambda -> (pattern list * lambda) list -> lambda
val translate_match_check_failure: Location.t -> lambda -> (pattern list * lambda) list -> lambda
val translate_tupled_match: lambda -> lambda list -> (pattern list * lambda) list -> lambda
val translate_tupled_match_check_failure: Location.t -> lambda list -> (pattern list * lambda) list -> lambda
