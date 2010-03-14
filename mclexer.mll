{
  open Mcparser

  let keywords = Hashtbl.create 23
  let _ = List.iter (fun (keyword, token) -> Hashtbl.add keywords keyword token)
    [
      "else", ELSE;
      "if", IF;
      "in", IN;
      "lambda", LAMBDA;
      "let", LET;
      "rec", REC;
      "then", THEN;

      "land", INFIXOP3("land");
      "lor", INFIXOP3("lor");
      "lxor", INFIXOP3("lxor");
      "lsl", INFIXOP4("lsl");
      "lsr", INFIXOP4("lsr");
      "asr", INFIXOP4("asr")
    ]

  let stringbuf = ref ""
}

let blank = [' ' '\009' '\010' '\012' '\013']
let lowercase = ['a'-'z' '\224'-'\246' '\248'-'\255' '_']
let identchar = ( lowercase | ['A'-'Z' '\192'-'\214' '\216'-'\246' '\'' '0'-'9'] )
let decimal_literal = ['0'-'9'] ['0'-'9' '_']*
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal = '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal = '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal = '-'? (decimal_literal | hex_literal | oct_literal | bin_literal)
let float_literal = '-'? (['0'-'9'] ['0'-'9']* ('.' ['0'-'9']* )? (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9']*)?)

rule token = parse
  | blank
      { token lexbuf }
  | lowercase identchar* | "()"
      { let id = Lexing.lexeme lexbuf in
          try
            Hashtbl.find keywords id
          with
            | Not_found -> IDENT(id) }
  | int_literal
      { INT(int_of_string(Lexing.lexeme lexbuf)) }
  | float_literal
      { FLOAT(float_of_string(Lexing.lexeme lexbuf)) }
  | "\""
      { stringbuf := ""; string lexbuf; STRING(!stringbuf) }
  | "(*"
      { comment lexbuf; token lexbuf }
  | "&&"
      { AMPERAMPER }
  | "||"
      { BARBAR }
  | ":="
      { COLONEQUAL }
  | ','
      { COMMA }
  | '.'
      { DOT }
  | "="
      { EQUAL }
  | "<>" | "<" | ">" | "<=" | ">=" | "==" | "!="
      { INFIXOP0(Lexing.lexeme lexbuf) }
  | "^"
      { INFIXOP1(Lexing.lexeme lexbuf) }
  | "+" | "-" | "+." | "-."
      { INFIXOP2(Lexing.lexeme lexbuf) }
  | "*" | "*."
      { INFIXOP3(Lexing.lexeme lexbuf) }
  | '('
      { LPAREN }
  | "~-" | "~-." | "!"
      { PREFIXOP(Lexing.lexeme lexbuf) }
  | ')'
      { RPAREN }
  | ';'
      { SEMI }
  | eof
      { EOF }
  | _
      { failwith ("Unknown token '" ^ (Lexing.lexeme lexbuf) ^ "'") }
and comment = parse
  | "(*"
      { failwith "Nested comments not supported" }
  | "*)"
      { () }
  | eof
      { failwith "Unterminated comment" }
  | _
      { comment lexbuf }
and string = parse
  | '"'
      { () }
  | eof
      { failwith "Unterminated string" }
  | _ 
      { stringbuf := !stringbuf ^ (Lexing.lexeme lexbuf); string lexbuf }
