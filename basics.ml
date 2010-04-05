external raise: exn -> 'a = "%raise";;

external ( = ): 'a -> 'a -> bool = "%equal";;
external ( <> ): 'a -> 'a -> bool = "%notequal";;
external ( < ): 'a -> 'a -> bool = "%lessthan";;
external ( > ): 'a -> 'a -> bool = "%greaterthan";;
external ( <= ): 'a -> 'a -> bool = "%lessequal";;
external ( >= ): 'a -> 'a -> bool = "%greaterequal";;
external compare: 'a -> 'a -> int = "%compare";;

external ( == ): 'a -> 'a -> bool = "%eq";;
external ( != ): 'a -> 'a -> bool = "%noteq";;

external not: bool -> bool = "%boolnot";;
external ( && ): bool -> bool -> bool = "%sequand";;
external ( || ): bool -> bool -> bool = "%sequor";;

external ( ~- ): int -> int = "%negint";;
external ( + ): int -> int -> int = "%addint";;
external ( - ): int -> int -> int = "%subint";;
external ( * ): int -> int -> int = "%mulint";;
external ( / ): int -> int -> int = "%divint";;
external ( mod ): int -> int -> int = "%modint";;

external ( land ): int -> int -> int = "%andint";;
external ( lor ): int -> int -> int = "%orint";;
external ( lxor ): int -> int -> int = "%xorint";;
external ( lsl ): int -> int -> int = "%lslint";;
external ( lsr ): int -> int -> int = "%lsrint";;
external ( asr ): int -> int -> int = "%asrint";;

external int_of_float: float -> int = "%intoffloat";;
external float_of_int: int -> float = "%floatofint";;

external ( ~-. ): float -> float = "%negfloat";;
external ( +. ): float -> float -> float = "%addfloat";;
external ( -. ): float -> float -> float = "%subfloat";;
external ( *. ): float -> float -> float = "%mulfloat";;
external ( /. ): float -> float -> float = "%divfloat";;

external ignore: 'a -> unit = "%ignore";;

external fst: 'a * 'b -> 'a = "%field0";;
external snd: 'a * 'b -> 'b = "%field1";;


(***********************)
(*** List operations ***)
(***********************)

let rec (@) l1 l2 =
  match l1 with
    | [] -> l2
    | x :: l1 -> x :: (l1 @ l2)
;;


(***********************************)
(*** String conversion functions ***)
(***********************************)

let string_of_bool = function
  | true -> "true"
  | false -> "false"

let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> raise (Invalid_argument("bool_of_string"))


(**********************)
(*** I/O operations ***)
(**********************)

type in_channel
type out_channel

external open_descriptor_out: int -> out_channel = "mc2_open_descriptor_out";;
external open_descriptor_in: int -> in_channel = "mc2_open_descriptor_in";;

let stdin = open_descriptor_in 0
and stdout = open_descriptor_out 1
and stderr = open_descriptor_out 2;;

external flush: out_channel -> unit = "mc2_flush";;

external output_string: out_channel -> string -> unit = "mc2_output_string";;

let print_string s = output_string stdout s;;
let print_endline s = print_string s; print_string "\n"; flush stdout;;

