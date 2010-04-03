{
  open Lexing
  open Parser
  open Syntaxerr

  (* Keyword hash table *)
  let keywords = Hashtbl.create 37
  let _ = List.iter (fun (keyword, token) -> Hashtbl.add keywords keyword token)
    [
      "and", AND;
      "as", AS;
      "begin", BEGIN;
      "else", ELSE;
      "end", END;
      "exception", EXCEPTION;
      "external", EXTERNAL;
      "false", FALSE;
      "fun", FUN;
      "function", FUNCTION;
      "if", IF;
      "in", IN;
      "let", LET;
      "match", MATCH;
      "of", OF;
      "rec", REC;
      "then", THEN;
      "true", TRUE;
      "try", TRY;
      "type", TYPE;
      "when", WHEN;
      "with", WITH;

      "mod", INFIXOP3("mod");
      "land", INFIXOP3("land");
      "lor", INFIXOP3("lor");
      "lxor", INFIXOP3("lxor");
      "lsl", INFIXOP4("lsl");
      "lsr", INFIXOP4("lsr");
      "asr", INFIXOP4("asr")
    ]

  (* String literal handling *)
  let string_buffer = Buffer.create 512
  let string_start_loc = ref Location.none

  (* Comment handling *)
  let comment_start_loc = ref []
  let in_comment (_:unit): bool = !comment_start_loc <> []

  (* Translating character sequences *)
  let char_for_backslash (c:char): char =
    match c with
      | 'b' -> '\008'
      | 'n' -> '\010'
      | 'r' -> '\013'
      | 't' -> '\009'
      | c   -> c
  and char_for_dec (lexbuf:Lexing.lexbuf) (i:int): char =
    let c = (100 * (Char.code (Lexing.lexeme_char lexbuf i) - 48)
             + 10 * (Char.code (Lexing.lexeme_char lexbuf (i + 1)) - 48)
             + (Char.code (Lexing.lexeme_char lexbuf (i + 2)) - 48)) in
      try
        Char.chr c
      with
        | Invalid_argument(_) when in_comment () -> 'x'
        | Invalid_argument(_) -> raise (Error(Illegal_escape(Lexing.lexeme_char lexbuf i, Location.curr lexbuf)))
  and char_for_hex (lexbuf:Lexing.lexbuf) (i:int): char =
    let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
    let val1 = if d1 >= 97 then d1 - 87 else if d1 >= 65 then d1 - 55 else d1 - 48 in
    let d2 = Char.code (Lexing.lexeme_char lexbuf (i + 1)) in
    let val2 = if d2 >= 97 then d2 - 87 else if d2 >= 65 then d2 - 55 else d2 - 48 in
      Char.chr (val1 * 16 + val2)

  (* Remove underscores from float literals *)
  let cleanup_float_literal (float_literal:string): string =
    let buffer = Buffer.create (String.length float_literal) in
      String.iter (fun c -> if c <> '_' then Buffer.add_char buffer c) float_literal;
      Buffer.contents buffer

  (* Update lexer location with file name and line number *)
  let update_pos (lexbuf:Lexing.lexbuf) (file:string option) (line:int) (absolute:bool) (chars:int):unit =
    let pos = lexbuf.lex_curr_p in
    let fname = (match file with Some(fname) -> fname | None -> pos.pos_fname) in
    let lnum = (if absolute then line else pos.pos_lnum + line) in
    let bol = pos.pos_cnum - chars in
      lexbuf.lex_curr_p <- { pos with pos_fname = fname; pos_lnum = lnum; pos_bol = bol }
}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\224'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = ( lowercase | uppercase | ['\216'-'\246' '\'' '0'-'9'] )
let decimal_literal = ['0'-'9'] ['0'-'9' '_']*
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal = '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal = '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal = (decimal_literal | hex_literal | oct_literal | bin_literal)
let float_literal = (['0'-'9'] ['0'-'9' '_']*
                       ('.' ['0'-'9' '_']* )?
                       (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?)

rule token = parse
  | newline
      { update_pos lexbuf None 1 false 0;
        token lexbuf }
  | blank+
      { token lexbuf }
  | (lowercase identchar*) as lexeme
      { try
          Hashtbl.find keywords lexeme
        with
          | Not_found -> LOWERCASEIDENT(lexeme) }
  | (uppercase identchar*) as id
      { UPPERCASEIDENT(id) }
  | int_literal as lexeme
      { try 
          INT(int_of_string(lexeme))
        with
          | Failure(_) -> raise (Error(Literal_overflow("int", Location.curr lexbuf))) }
  | float_literal as lexeme
      { FLOAT(cleanup_float_literal lexeme) }
  | (int_literal as lexeme) "l"
      { try
          INT32(Int32.of_string lexeme)
        with
          | Failure(_) -> raise (Error(Literal_overflow("int32", Location.curr lexbuf))) }
  | (int_literal as lexeme) "L"
      { try
          INT64(Int64.of_string lexeme)
        with
          | Failure(_) -> raise (Error(Literal_overflow("int64", Location.curr lexbuf))) }
  | (int_literal as lexeme) "n"
      { try
          NATIVEINT(Nativeint.of_string lexeme)
        with
          | Failure(_) -> raise (Error(Literal_overflow("nativeint", Location.curr lexbuf))) }
  | "\""
      { Buffer.clear string_buffer;
        let string_start = lexbuf.lex_start_p in
          string_start_loc := Location.curr lexbuf;
          string lexbuf;
          lexbuf.lex_start_p <- string_start;
          STRING(Buffer.contents string_buffer) }
  | "'" newline "'"
      { update_pos lexbuf None 1 false 1;
        CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'" [^ '\\' '\'' '\010' '\013'] "'"
      { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
      { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHAR(char_for_dec lexbuf 2) }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { CHAR(char_for_hex lexbuf 3) }
  | "'\\" (_ as escape)
      { raise (Error(Illegal_escape(escape, Location.curr lexbuf))) }
  | "(*"
      { comment_start_loc := [Location.curr lexbuf];
        comment lexbuf;
        token lexbuf }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
      { update_pos lexbuf name (int_of_string num) true 0;
        token lexbuf }
  | "&&"
      { AMPERAMPER }
  | '|'
      { BAR }
  | "||"
      { BARBAR }
  | ':'
      { COLON }
  | "::"
      { COLONCOLON }
  | ":="
      { COLONEQUAL }
  | ','
      { COMMA }
  | "="
      { EQUAL }
  | "<>" | "<" | ">" | "<=" | ">=" | "==" | "!="
      { INFIXOP0(Lexing.lexeme lexbuf) }
  | '^' | '@'
      { INFIXOP1(Lexing.lexeme lexbuf) }
  | "+" | "+."
      { INFIXOP2(Lexing.lexeme lexbuf) }
  | "*." | "/" | "/."
      { INFIXOP3(Lexing.lexeme lexbuf) }
  | '['
      { LBRACKET }
  | '('
      { LPAREN }
  | '-'
      { MINUS }
  | "-."
      { MINUSDOT }
  | "->"
      { MINUSGREATER }
  | '+'
      { PLUS }
  | "~-" | "~-." | "!"
      { PREFIXOP(Lexing.lexeme lexbuf) }
  | "'"
      { QUOTE }
  | ']'
      { RBRACKET }
  | ')'
      { RPAREN }
  | ';'
      { SEMI }
  | ";;"
      { SEMISEMI }
  | '*'
      { STAR }
  | "_"
      { UNDERSCORE }
  | eof
      { EOF }
  | _
      { raise (Error(Illegal_character(Lexing.lexeme_char lexbuf 0, Location.curr lexbuf))) }
and comment = parse
  | "(*"
      { comment_start_loc := (Location.curr lexbuf) :: !comment_start_loc;
        comment lexbuf }
  | "*)"
      {
        comment_start_loc := List.tl (!comment_start_loc);
        if in_comment () then comment lexbuf }
  | "\""
      { Buffer.clear string_buffer;
        string_start_loc := Location.curr lexbuf;
        begin try
           string lexbuf
         with
           | Error(Unterminated_string(loc)) ->
               raise (Error(Unterminated_string_in_comment(loc)))
        end;
        comment lexbuf }
  | "''"
      { comment lexbuf }
  | "'" newline "'"
      { update_pos lexbuf None 1 false 1;
        comment lexbuf }
  | "'" [^ '\\' '\'' '\010' '\013' ] "'"
      { comment lexbuf }
  | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
      { comment lexbuf }
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { comment lexbuf }
  | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
      { comment lexbuf }
  | eof
      { raise (Error(Unterminated_comment(List.hd (!comment_start_loc)))) }
  | newline
      { update_pos lexbuf None 1 false 0;
        comment lexbuf }
  | _
      { comment lexbuf }
and string = parse
  | '"'
      { () }
  | '\\' newline ([' ' '\t'] * as space)
      { update_pos lexbuf None 1 false (String.length space);
        string lexbuf }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
      { Buffer.add_char string_buffer (char_for_backslash (Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { Buffer.add_char string_buffer (char_for_dec lexbuf 1);
        string lexbuf }
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
      { Buffer.add_char string_buffer (char_for_hex lexbuf 2);
        string lexbuf }
  | '\\' (_ as escape)
      { if in_comment ()
        then string lexbuf
        else raise (Error(Illegal_escape(escape, Location.curr lexbuf))) }
  | newline
      { update_pos lexbuf None 1 false 0;
        Buffer.add_string string_buffer (Lexing.lexeme lexbuf);
        string lexbuf }
  | eof
      { raise (Error(Unterminated_string(!string_start_loc))) }
  | _
      { Buffer.add_char string_buffer (Lexing.lexeme_char lexbuf 0);
        string lexbuf }
