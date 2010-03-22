val structure: Lexing.lexbuf -> Parsedast.structure

val file: string -> (Lexing.lexbuf -> 'a) -> 'a
