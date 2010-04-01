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

(*
let rec fact x = if x = 0 then 1 else fact (x - 1) in
  fact 3;;
*)

(*
let rec even x =
  if x == 0 then true
  else odd (x - 1)
and odd x =
  if x == 0 then false
  else even (x - 1)
;;
*)

let value x =
  if x = Some(1) then 1 else 2

(*
let map_fact = map fact;;
*)

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

(*
let rec map f l =
  match l with
    | [] -> []
    | x :: l -> f x :: map f l
;;
*)

(*
let h y l =
  let k = (); function
    | 0 -> 1
    | 1 -> 1
    | x -> (x - 1) * (x - 2) in
  let rec f l =
    map g l
  and g x =
    x + k y
  in g, f
;;
*)

(*
let rec foldl f a l =
  match l with
    | [] -> a
    | x :: l -> foldl f (f a x 0 0 0 0 0 0 0) l
;;
*)

