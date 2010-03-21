let parse lexbuf =
  Parser.structure Lexer.token lexbuf;;

let pstr = parse (Lexing.from_channel (open_in "test.ml"));;

let tstr =
  begin try
    Typing.type_structure Typeenv.initial pstr
  with
    | Typing.Error(error, loc) ->
        Location.print_error Format.err_formatter loc;
        Typing.report_error Format.err_formatter error;
        Format.fprintf Format.err_formatter "@.";
        exit 2
  end
;;
