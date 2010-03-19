type constant =
  | Const_int of int
  | Const_char of char
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_string of string
  | Const_nativeint of nativeint

type rec_flag =
  | Recursive
  | NonRecursive
