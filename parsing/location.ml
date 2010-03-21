open Format
open Lexing

type t =
    { loc_start: Lexing.position;
      loc_end:   Lexing.position;
      loc_ghost: bool }

let none =
  { loc_start = Lexing.dummy_pos;
    loc_end   = Lexing.dummy_pos;
    loc_ghost = true }

let symbol_gloc (_:unit): t =
  { loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
    loc_ghost = true }

let symbol_rloc (_:unit): t =
  { loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
    loc_ghost = false }

let rhs_loc (n:int): t =
  { loc_start = Parsing.rhs_start_pos n;
    loc_end = Parsing.rhs_end_pos n;
    loc_ghost = false }

let curr (lexbuf:Lexing.lexbuf): t =
  { loc_start = lexbuf.lex_start_p;
    loc_end = lexbuf.lex_curr_p;
    loc_ghost = false }

let print ppf loc =
  let line, startchar = loc.loc_start.pos_lnum, loc.loc_start.pos_cnum in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    fprintf ppf "Line %i, characters %i-%i:@." line startchar endchar

let print_error ppf loc =
  print ppf loc;
  fprintf ppf "Error: "
