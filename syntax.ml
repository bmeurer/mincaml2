type t =
  | Unit
  | Bool of bool
  | Int of int
  | Char of char
  | Float of float
  | String of string
  | Ident of string
  | If of t * t * t
  | Tuple of t list
  | App of t * t
  | Abstr of string * t
  | Let of string * t * t
  | LetTuple of string list * t * t
  | LetRec of string * t * t
