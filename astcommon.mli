type constant =
  | Const_int of int
  | Const_char of char
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
  | Const_string of string

type rec_flag =
  | Recursive
  | NonRecursive
