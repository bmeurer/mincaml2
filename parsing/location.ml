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

let symbol_gloc () =
  { loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
    loc_ghost = true }

let symbol_rloc () =
  { loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
    loc_ghost = false }

let rhs_loc n =
  { loc_start = Parsing.rhs_start_pos n;
    loc_end = Parsing.rhs_end_pos n;
    loc_ghost = false }

let curr lexbuf =
  { loc_start = lexbuf.lex_start_p;
    loc_end = lexbuf.lex_curr_p;
    loc_ghost = false }

let init lexbuf fname =
  let curr_p = { pos_fname = fname;
                 pos_lnum = 1;
                 pos_bol = 0;
                 pos_cnum = 0 } in
    lexbuf.lex_curr_p <- curr_p

let print ppf loc =
  let file, line = loc.loc_start.pos_fname, loc.loc_start.pos_lnum in
  let startchar = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    fprintf ppf "File \"%s\", line %i, characters %i-%i:@." file line startchar endchar

let print_error ppf loc =
  print ppf loc;
  fprintf ppf "Error: "
