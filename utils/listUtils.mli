(** TODO *)
val init: int -> (int -> 'a) -> 'a list

(** TODO *)
val rev_init: int -> (int -> 'a) -> 'a list

(** [ListUtils.make n x] creates a new list which contains the value [x] [n] times.
    Raise [Invalid_argument "ListUtils.make"] if [n] is negative. *)
val make: int -> 'a -> 'a list

(** [ListUtils.swap n l] swaps the [0]-th and the [n]-th element of the list [l].
    Raise [Failure "ListUtils.swap"] if the list [l] is too short.
    Raise [Invalid_argument "ListUtils.swap"] if [n] is negative. *)
val swap: int -> 'a list -> 'a list

(** TODO *)
val split: int -> 'a list -> 'a list * 'a list
