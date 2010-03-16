type constant =
  | Const_int of int
  | Const_char of char
  | Const_string of string
  | Const_float of string

type rec_flag =
  | Recursive
  | NonRecursive
