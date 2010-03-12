#load "listutils.cmo";;
#load "type.cmo";;
#load "syntax.cmo";;
#load "typing.cmo";;
#load "mcparser.cmo";;
#load "mclexer.cmo";;

let tokenl = ref []
let token lexbuf =
  let t = Mclexer.token lexbuf in
    tokenl := t :: !tokenl; t
let parse_string (s:string) =
  Mcparser.expression token (Lexing.from_string s)

let e = parse_string "let add = lambda x.lambda y.if x then 1 else y in add";;
print_endline (Syntax.to_string e);;

let tau = Typing.infer Type.gamma0 e;;
print_endline (Type.to_string tau);;
