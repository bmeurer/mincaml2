type error =
  | Illegal_character of char * Location.t
  | Illegal_escape of char * Location.t
  | Literal_overflow of string * Location.t
  | Unclosed of Location.t * string * Location.t * string
  | Unterminated_comment of Location.t
  | Unterminated_string of Location.t
  | Unterminated_string_in_comment of Location.t * Location.t

exception Error of error