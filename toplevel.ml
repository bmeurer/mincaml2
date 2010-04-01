let pstr =
  begin try
    Parse.file "test.ml" Parse.structure
  with
    | Syntaxerr.Error(error) ->
        Syntaxerr.report_error Format.err_formatter error;
        Format.fprintf Format.err_formatter "@.";
        exit 2
  end;;

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

let lambda =
  begin try
    Translexp.translate_structure tstr
  with
    | Translexp.Error(error, loc) ->
        Location.print_error Format.err_formatter loc;
        Translexp.report_error Format.err_formatter error;
        Format.fprintf Format.err_formatter "@.";
        exit 2
  end
;;

Printlambda.print_lambda Format.std_formatter lambda;
Format.fprintf Format.std_formatter "@.@."

let lambda0 =
  Closure.close_lambda lambda
;;

Printlambda.print_lambda Format.std_formatter lambda0;
Format.fprintf Format.std_formatter "@.@."

