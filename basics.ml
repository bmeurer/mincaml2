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

external open_descriptor_out: int -> int -> out_channel = "mc2_io_fdopen";;
external open_descriptor_in: int -> int -> in_channel = "mc2_io_fdopen";;

let stdin = open_descriptor_in 0 1
and stdout = open_descriptor_out 1 0
and stderr = open_descriptor_out 2 0;;

external flush: out_channel -> unit = "mc2_io_fflush";;

external output_char: out_channel -> char -> unit = "mc2_io_fputc";;
external output_string: out_channel -> string -> unit = "mc2_io_fputs";;

let print_string s = output_string stdout s
and print_char c = output_char stdout c
and print_endline s = let channel = stdout in output_string channel s; output_char channel '\n'; flush channel
and print_newline () = output_char stdout '\n'
;;

let prerr_char c = output_char stderr c
and prerr_string s = output_string stderr s
and prerr_endline s = let channel = stderr in output_string channel s; output_char channel '\n'; flush channel
and prerr_newline () = output_char stderr '\n'
;;
