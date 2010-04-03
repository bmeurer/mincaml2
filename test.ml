(*
let fact x =
  let rec fact_aux x accu =
    match x with
      | 0 -> accu
      | x -> fact_aux (x - 1) (x * accu)
  in fact_aux x 1
;;

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

even, odd;;
*)

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

let rec foldl f a l =
  match l with
    | [] -> a
    | x :: l -> foldl f (f a x) l
;;

foldl (+) 1 [1;2;3];;
