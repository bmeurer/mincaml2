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

let add x y = x + y;;

let one = 1;;

let map_plus1 l =
  let plus1 = fun x -> add x one in
    map plus1 l
;;

let rec printl = function
  | [] -> print_string "\n"
  | [s] -> print_string s; print_string "\n"
  | s :: l -> print_string s; print_string "; "; printl l
;;

printl ["Hallo"; "Welt"];;
*)

let f exn =
match exn with
  | Match_failure(_) -> 0
  | Invalid_argument(_) -> 1
  | _ -> 2;;

f (Not_found);;
