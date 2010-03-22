let structure lexbuf =
  try
    let ast = Parser.structure Lexer.token lexbuf in
      Parsing.clear_parser ();
      ast
  with 
    | Parsing.Parse_error ->
        raise (Syntaxerr.Error(Syntaxerr.Unknown(Location.curr lexbuf)))
      
let file infile parse =
  let inchannel = open_in_bin infile in
  let ast = (try
               let lexbuf = Lexing.from_channel inchannel in
                 Location.init lexbuf infile;
                 parse lexbuf
             with
               | exn ->
                   close_in inchannel;
                   raise exn) in
    close_in inchannel;
    ast
