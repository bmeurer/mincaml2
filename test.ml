#load "listutils.cmo";;
#load "type.cmo";;
#load "id.cmo";;
#load "builtin.cmo";;
#load "syntax.cmo";;
#load "typing.cmo";;
#load "purity.cmo";;
#load "optimize0.cmo";;
#load "mcparser.cmo";;
#load "mclexer.cmo";;
#load "codegen.cmo";;

let tokenl = ref []
let token lexbuf =
  let t = Mclexer.token lexbuf in
    tokenl := t :: !tokenl; t
let parse_string (s:string) =
  Mcparser.expression token (Lexing.from_string s)

(*let e = parse_string "let counter = ref 1 in let add x y = incr counter; if x then 1 else y in add true 1";;*)
(*let e = parse_string "let rec fact x t = if x = 0 then 1 else fact (x - 1) (x * t) in fact 4 1";;*)
let e = parse_string
  "let fact x =
     let rec g t x = if x = 0 then !t else (t := !t * x; g t (x - 1)) in
     let t = ref 1 in
        g t x
   in fact 4";;
print_endline (Syntax.to_string e);;

print_endline (Type.to_string (Typing.infer Builtin.gamma0 e));;

let e' = Optimize0.pass e;;
print_endline (Syntax.to_string e');;
(*Typing.infer Builtin.gamma0 e';;*)

let cg = Codegen.create "fact" in
  Codegen.dump e' cg
