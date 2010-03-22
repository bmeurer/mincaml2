open Format

type error =
  | Illegal_character of char * Location.t
  | Illegal_escape of char * Location.t
  | Literal_overflow of string * Location.t
  | Unclosed of Location.t * string * Location.t * string
  | Unknown of Location.t
  | Unterminated_comment of Location.t
  | Unterminated_string of Location.t
  | Unterminated_string_in_comment of Location.t

exception Error of error

let report_error ppf = function
  | Illegal_character(c, loc) ->
      fprintf ppf "%aIllegal character (%s)"
        Location.print_error loc (Char.escaped c)
  | Illegal_escape(c, loc) ->
      fprintf ppf "%aIllegal backslash escape in string or character (%s)"
        Location.print_error loc (Char.escaped c)
  | Literal_overflow(typ, loc) ->
      fprintf ppf "%aInteger literal exceeds the range of representable integers of type %s"
        Location.print_error loc typ
  | Unclosed(opening_loc, opening, closing_loc, closing) ->
      fprintf ppf "%aSyntax error: '%s' expected@."
        Location.print_error closing_loc closing;
      fprintf ppf "%aThis '%s' might be unmatched"
        Location.print_error opening_loc opening
  | Unknown(loc) ->
      fprintf ppf "%aSyntax error"
        Location.print_error loc
  | Unterminated_comment(loc) ->
      fprintf ppf "%aUnterminated comment"
        Location.print_error loc
  | Unterminated_string(loc) ->
      fprintf ppf "%aUnterminated string literal"
        Location.print_error loc
  | Unterminated_string_in_comment(loc) ->
      fprintf ppf "%aUnterminated string literal in comment"
        Location.print_error loc

