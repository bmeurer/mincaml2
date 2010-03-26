(** [ListUtils.swap n l] swaps the [0]-th and the [n]-th element of the list [l].
    Raise [Failure "ListUtils.swap"] if the list [l] is too short.
    Raise [Invalid_argument "ListUtils.swap"] if [n] is negative. *)
val swap: int -> 'a list -> 'a list
