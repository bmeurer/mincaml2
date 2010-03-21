let rec fact x = if x = 0 then 1 else fact (x - 1);;

let rec fib = function
  | 0 -> 1
  | 1 -> 1
  | x -> (+) (fib ( x-1)) (fib ((+) x (-2)));;

let swap (x, y) = y, x;;

let fst (x, _) = x;;
let snd (_, x) = x;;
