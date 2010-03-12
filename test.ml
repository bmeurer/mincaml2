#load "listutils.cmo";;
#load "type.cmo";;
#load "syntax.cmo";;
#load "typing.cmo";;

open Syntax;;

let e = LetRec("id",
               Abstr("x", Ident("x")),
               Tuple([App(Ident("id"), Unit);
                      App(Ident("id"), Float(1.))]));;
Typing.typeof [] e;;
