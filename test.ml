external raise: exn -> 'a = "%raise"

external ( = ): 'a -> 'a -> bool = "%equal"
external ( <> ): 'a -> 'a -> bool = "%notequal"
external ( < ): 'a -> 'a -> bool = "%lessthan"
external ( > ): 'a -> 'a -> bool = "%greaterthan"
external ( <= ): 'a -> 'a -> bool = "%lessequal"
external ( >= ): 'a -> 'a -> bool = "%greaterequal"
external compare: 'a -> 'a -> int = "%compare"

external ( == ): 'a -> 'a -> bool = "%eq"
external ( != ): 'a -> 'a -> bool = "%noteq"

external not: bool -> bool = "%boolnot"
external ( && ): bool -> bool -> bool = "%sequand"
external ( || ): bool -> bool -> bool = "%sequor"
external ( ~- ): int -> int = "%negint"

external ( + ): int -> int -> int = "%addint"
external ( - ): int -> int -> int = "%subint"
external ( * ): int -> int -> int = "%mulint"
external ( / ): int -> int -> int = "%divint"
external ( mod ): int -> int -> int = "%modint"

external fst: 'a * 'b -> 'a = "%getfield0"
external snd: 'a * 'b -> 'b = "%getfield1"

let rec fact x = if x = 0 then 1 else fact (x - 1);;

let rec map f l =
  match l with
    | [] -> []
    | x :: l -> (f x) :: (map f l)
;;

let map_fact = map fact;;

(*
let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | x -> (+) (fib ( x-1)) (fib ((+) x (-2)));;

let swap (x, y) = y, x;;

let length l =
  let rec length_aux accu = function
    | [] -> accu
    | _ :: l -> length_aux (accu + 1) l
  in length_aux 0 l
;;
*)
