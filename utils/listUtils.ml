let make n x =
  let rec make_aux accu = function
    | 0 -> accu
    | n -> make_aux (x :: accu) (n - 1)
  in
    if n < 0 then
      invalid_arg "ListUtils.make"
    else
      make_aux [] n
  
let swap n l =
  let rec swap_aux n l accu =
    match n, l with
      | 0, x :: l -> x :: (List.rev_append accu l)
      | n, x :: l -> swap_aux (n - 1) l (x :: accu)
      | _ -> failwith "ListUtils.swap"
  in
    if n < 0 then
      invalid_arg "ListUtils.swap"
    else
      swap_aux n l []
