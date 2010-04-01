let init n f =
  let rec init_aux n =
    if n = 0 then
      []
    else
      let n = n - 1 in
        f n :: init_aux n
  in
    if n < 0 then
      invalid_arg "ListUtils.init"
    else
      init_aux n

let rev_init n f =
  let rec rev_init_aux n accu =
    if n = 0 then
      accu
    else
      let n = n - 1 in
        rev_init_aux n (f n :: accu)
  in
    if n < 0 then
      invalid_arg "ListUtils.rev_init"
    else
      rev_init_aux n []

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

let split n l =
  let rec split_aux n l =
    match n, l with
      | 0, l ->
          [], l
      | n, x :: l -> 
          let l1, l2 = split_aux (n - 1) l in
            x :: l1, l2
      | _ ->
          failwith "ListUtils.split"
  in
    if n < 0 then
      invalid_arg "ListUtils.split"
    else
      split_aux n l
