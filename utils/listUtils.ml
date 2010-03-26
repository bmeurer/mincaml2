let swap n l =
  let rec swap_aux n l accu =
    match n, l with
      | 0, x :: l -> x :: (List.rev_append accu l)
      | n, x :: l -> swap_aux (n - 1) l (x :: accu)
      | _ -> invalid_arg "ListUtils.swap"
  in
    if n < 0 then
      invalid_arg "ListUtils.swap"
    else
      swap_aux n l []
