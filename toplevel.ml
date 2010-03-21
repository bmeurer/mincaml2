let parse lexbuf =
  Parser.structure Lexer.token lexbuf;;

let pstr = parse (Lexing.from_channel (open_in "test.ml"));;

let tstr = Typing.type_structure Typeenv.initial pstr;;
