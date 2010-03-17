%{
  open Astcommon
  open Location
  open Parsedast
  open Syntaxerr

  let mkexp d = {pexp_desc = d; pexp_loc = symbol_rloc ()}
  let mkpat d = {ppat_desc = d; ppat_loc = symbol_rloc ()}
  let mkstr d = {pstr_desc = d; pstr_loc = symbol_rloc ()}
  let mktyp d = {ptyp_desc = d; ptyp_loc = symbol_rloc ()}

  let mkoperator name pos = {pexp_desc = Pexp_ident(name); pexp_loc = rhs_loc pos}
  let mkinfix e1 name e2 = mkexp (Pexp_apply(mkoperator name 2, [e1; e2]))
  let mkuminus name e =
    let negate_float f =
      if String.length f > 0 && f.[0] = '-' then String.sub f 1 (String.length f - 1)
      else "-" ^ f
    in match name, e.pexp_desc with
      | "-", Pexp_constant(Const_int(n)) -> mkexp (Pexp_constant(Const_int(-n)))
      | _, Pexp_constant(Const_float(f)) -> mkexp (Pexp_constant(Const_float(negate_float f)))
      | "-", Pexp_constant(Const_int32(n)) -> mkexp (Pexp_constant(Const_int32(Int32.neg n)))
      | "-", Pexp_constant(Const_int64(n)) -> mkexp (Pexp_constant(Const_int64(Int64.neg n)))
      | "-", Pexp_constant(Const_nativeint(n)) -> mkexp (Pexp_constant(Const_nativeint(Nativeint.neg n)))
      | _, _ -> mkexp (Pexp_apply(mkoperator ("~" ^ name) 1, [e]))

  let ghexp d = {pexp_desc = d; pexp_loc = symbol_gloc ()}
  let ghstrexp e = {pstr_desc = Pstr_exp(e); pstr_loc = {e.pexp_loc with loc_ghost = true}}

  let reloc_exp e = {e with pexp_loc = symbol_rloc ()}
  let reloc_pat p = {p with ppat_loc = symbol_rloc ()}

  let unclosed opening_name opening_num closing_name closing_num =
    raise (Error(Unclosed(rhs_loc opening_num, opening_name, rhs_loc closing_num, closing_name)))
%}

/* Tokens */

%token AMPERAMPER
%token AND
%token AS
%token BAR
%token BARBAR
%token BEGIN
%token <char> CHAR
%token COLON
%token COLONEQUAL
%token COMMA
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token FALSE
%token <string> FLOAT
%token FUN
%token FUNCTION
%token IF
%token IN
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token LET
%token <string> LOWERCASEIDENT
%token LPAREN
%token MATCH
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token <nativeint> NATIVEINT
%token OF
%token PLUS
%token <string> PREFIXOP
%token QUOTE
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token STAR
%token <string> STRING
%token THEN
%token TRUE
%token TRY
%token TYPE
%token UNDERSCORE
%token <string> UPPERCASEIDENT
%token WHEN
%token WITH

/* Precedences and associativities */

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc LET
%nonassoc FUNCTION WITH
%nonassoc AND
%nonassoc THEN
%nonassoc ELSE
%right    COLONEQUAL
%nonassoc AS
%left     BAR
%nonassoc below_COMMA
%left     COMMA
%right    MINUSGREATER
%right    BARBAR
%right    AMPERAMPER
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL
%right    INFIXOP1
%left     INFIXOP2 PLUS MINUS MINUSDOT
%left     INFIXOP3 STAR
%right    INFIXOP4
%nonassoc prec_unary_minus
%nonassoc prec_constant_constructor
%nonassoc prec_constructor_apply
%nonassoc BEGIN CHAR FALSE FLOAT INT INT32 INT64 LOWERCASEIDENT LPAREN NATIVEINT PREFIXOP STRING TRUE UPPERCASEIDENT

/* Entry points */

%start structure
%type <Parsedast.structure> structure

%%


/* Structure */

structure:
| structure_tail EOF
    { $1 }
| seq_expr structure_tail EOF
    { ghstrexp $1 :: $2 }
;

structure_tail:
| /* empty */
    { [] }
| SEMISEMI
    { [] }
| SEMISEMI seq_expr structure_tail
    { ghstrexp $2 :: $3 }
| SEMISEMI structure_item structure_tail
    { $2 :: $3 }
| structure_item structure_tail
    { $1 :: $2 }
;

structure_item:
| LET rec_flag let_bindings
    { match $3 with
        | [{ppat_desc = Ppat_any}, e] -> mkstr (Pstr_exp(e))
        | bindings -> mkstr (Pstr_let($2, List.rev bindings)) }
| TYPE type_declarations
    { mkstr (Pstr_typ(List.rev $2)) }
| EXCEPTION UPPERCASEIDENT constructor_arguments
    { mkstr (Pstr_exn($2, $3)) }
;


/* Expressions */

seq_expr:
| expr %prec below_SEMI
    { $1 }
| expr SEMI
    { reloc_exp $1 }
| expr SEMI seq_expr
    { mkexp (Pexp_sequence($1, $3)) }
;

expr:
| simple_expr
    { $1 }
| simple_expr simple_expr_list
    { mkexp (Pexp_apply($1, List.rev $2)) }
| LET rec_flag let_bindings IN seq_expr
    { mkexp (Pexp_let($2, List.rev $3, $5)) }
| FUNCTION opt_bar match_cases
    { mkexp (Pexp_function(List.rev $3)) }
| FUN simple_pattern fun_def
    { mkexp (Pexp_function([$2, $3])) }
| MATCH seq_expr WITH opt_bar match_cases
    { mkexp (Pexp_match($2, List.rev $5)) }
| TRY seq_expr WITH opt_bar match_cases
    { mkexp (Pexp_try($2, List.rev $5)) }
| expr_comma_list %prec below_COMMA
    { mkexp (Pexp_tuple(List.rev $1)) }
| constructor_ident simple_expr
    { mkexp (Pexp_construct($1, Some $2)) }
| IF seq_expr THEN expr ELSE expr
    { mkexp (Pexp_ifthenelse($2, $4, Some $6)) }
| IF seq_expr THEN expr
    { mkexp (Pexp_ifthenelse($2, $4, None)) }
| expr AMPERAMPER expr
    { mkinfix $1 "&&" $3 }
| expr BARBAR expr
    { mkinfix $1 "||" $3 }
| expr COLONEQUAL expr
    { mkinfix $1 ":=" $3 }
| expr EQUAL expr
    { mkinfix $1 "=" $3 }
| expr PLUS expr
    { mkinfix $1 "+" $3 }
| expr MINUS expr
    { mkinfix $1 "-" $3 }
| expr MINUSDOT expr
    { mkinfix $1 "-." $3 }
| expr STAR expr
    { mkinfix $1 "*" $3 }
| expr INFIXOP0 expr
    { mkinfix $1 $2 $3 }
| expr INFIXOP1 expr
    { mkinfix $1 $2 $3 }
| expr INFIXOP2 expr
    { mkinfix $1 $2 $3 }
| expr INFIXOP3 expr
    { mkinfix $1 $2 $3 }
| expr INFIXOP4 expr
    { mkinfix $1 $2 $3 }
| subtractive expr %prec prec_unary_minus
    { mkuminus $1 $2 }
;

expr_comma_list:
| expr_comma_list COMMA expr
    { $3 :: $1 }
| expr COMMA expr
    { [$3; $1] }
;

simple_expr:
| value_ident
    { mkexp (Pexp_ident($1)) }
| constant
    { mkexp (Pexp_constant($1)) }
| constructor_ident %prec prec_constant_constructor
    { mkexp (Pexp_construct($1, None)) }
| LPAREN seq_expr RPAREN
    { reloc_exp $2 }
| LPAREN seq_expr error
    { unclosed "(" 1 ")" 3 }
| BEGIN seq_expr END
    { reloc_exp $2 }
| BEGIN seq_expr error
    { unclosed "begin" 1 "end" 3 }
| LPAREN seq_expr COLON typ RPAREN
    { mkexp (Pexp_constraint($2, $4)) }
| LPAREN seq_expr COLON typ error
    { unclosed "(" 1 ")" 5 }
| PREFIXOP simple_expr
    { mkexp (Pexp_apply(mkoperator $1 1, [$2])) }
;

simple_expr_list:
| simple_expr
    { [$1] }
| simple_expr_list simple_expr
    { $2 :: $1 }
;

let_bindings:
| let_binding
    { [$1] }
| let_bindings AND let_binding
    { $3 :: $1 }
;

let_binding:
| value_ident fun_binding
    { {ppat_desc = Ppat_var($1); ppat_loc = rhs_loc 1}, $2 }
| pattern EQUAL seq_expr
    { $1, $3 }
;

fun_binding:
| EQUAL seq_expr
    { $2 }
| simple_pattern fun_binding
    { ghexp (Pexp_function([$1, $2])) }
| COLON typ EQUAL seq_expr
    { ghexp (Pexp_constraint($4, $2)) }
;

match_cases:
| pattern match_action
    { [$1, $2] }
| match_cases BAR pattern match_action
    { ($3, $4) :: $1 }
;

fun_def:
| match_action
    { $1 }
| simple_pattern fun_def
    { ghexp (Pexp_function([$1, $2])) }
;

match_action:
| MINUSGREATER seq_expr
    { $2 }
| WHEN seq_expr MINUSGREATER seq_expr
    { mkexp (Pexp_when($2, $4)) }
;


/* Patterns */

pattern:
| simple_pattern
    { $1 }
| pattern AS value_ident
    { mkpat (Ppat_alias($1, $3)) }
| pattern_comma_list %prec below_COMMA
    { mkpat (Ppat_tuple(List.rev $1)) }
| constructor_ident pattern %prec prec_constructor_apply
    { mkpat (Ppat_construct($1, Some($2))) }
| pattern BAR pattern
    { mkpat (Ppat_or($1, $3)) }
;

pattern_comma_list:
| pattern COMMA pattern
    { [$3; $1] }
| pattern_comma_list COMMA pattern
    { $3 :: $1 }
;

simple_pattern:
| value_ident %prec below_EQUAL
    { mkpat (Ppat_var($1)) }
| UNDERSCORE
    { mkpat (Ppat_any) }
| signed_constant
    { mkpat (Ppat_constant($1)) }
| constructor_ident
    { mkpat (Ppat_construct($1, None)) }
| LPAREN pattern RPAREN
    { reloc_pat $2 }
| LPAREN pattern error
    { unclosed "(" 1 ")" 3 }
| LPAREN pattern COLON typ RPAREN
    { mkpat (Ppat_constraint($2, $4)) }
| LPAREN pattern COLON typ error
    { unclosed "(" 1 ")" 5 }
;


/* Type declarations */

type_declarations:
| type_declaration
    { [$1] }
| type_declarations AND type_declaration
    { $3 :: $1 }
;

type_declaration:
| type_parameters LOWERCASEIDENT type_kind
    { let (desc, typ) = $3 in
        $2, {ptype_params = $1; ptype_desc = desc; ptype_typ = typ; ptype_loc = symbol_rloc ()} }
;

type_kind:
| /* empty */
    { Ptype_abstract, None }
| EQUAL typ
    { Ptype_abstract, Some($2) }
| EQUAL constructor_declarations
    { Ptype_variant(List.rev $2), None }
;

type_parameters:
| /* empty */
    { [] }
| type_parameter
    { [$1] }
| LPAREN type_parameter_comma_list RPAREN
    { List.rev $2 }
;

type_parameter:
| QUOTE ident
    { $2 }
;

type_parameter_comma_list:
| type_parameter
    { [$1] }
| type_parameter_comma_list COMMA type_parameter
    { $3 :: $1 }
;

constructor_declarations:
| constructor_declaration
    { [$1] }
| constructor_declarations BAR constructor_declaration
    { $3 :: $1 }
;

constructor_declaration:
| UPPERCASEIDENT constructor_arguments
    { $1, $2, symbol_rloc () }
;

constructor_arguments:
| /* empty */
    { [] }
| OF typ_star_list
    { List.rev $2 }
;


/* Types */

typ:
| simple_typ_or_tuple
    { $1 }
| typ MINUSGREATER typ
    { mktyp (Ptyp_arrow($1, $3)) }
;

typ_comma_list:
| typ
    { [$1] }
| typ_comma_list COMMA typ
    { $3 :: $1 }
;

typ_star_list:
| simple_typ
    { [$1] }
| typ_star_list STAR simple_typ
    { $3 :: $1 }
;

simple_typ:
| QUOTE ident
    { mktyp (Ptyp_var($2)) }
| UNDERSCORE
    { mktyp (Ptyp_any) }
| LOWERCASEIDENT
    { mktyp (Ptyp_construct($1, [])) }
| simple_typ LOWERCASEIDENT
    { mktyp (Ptyp_construct($2, [$1])) }
| LPAREN typ_comma_list RPAREN LOWERCASEIDENT
    { mktyp (Ptyp_construct($4, List.rev $2)) }
;

simple_typ_or_tuple:
| simple_typ
    { $1 }
| simple_typ STAR typ_star_list
    { mktyp (Ptyp_tuple($1 :: List.rev $3)) }
;


/* Constants */

constant:
| INT       { Const_int($1) }
| CHAR      { Const_char($1) }
| FLOAT     { Const_float($1) }
| INT32     { Const_int32($1) }
| INT64     { Const_int64($1) }
| STRING    { Const_string($1) }
| NATIVEINT { Const_nativeint($1) }
;

signed_constant:
| constant        { $1 }
| MINUS INT       { Const_int(- $2) }
| MINUS FLOAT     { Const_float("-" ^ $2) }
| MINUS INT32     { Const_int32(Int32.neg $2) }
| MINUS INT64     { Const_int64(Int64.neg $2) }
| MINUS NATIVEINT { Const_nativeint(Nativeint.neg $2) }
| PLUS INT        { Const_int($2) }
| PLUS FLOAT      { Const_float($2) }
| PLUS INT32      { Const_int32($2) }
| PLUS INT64      { Const_int64($2) }
| PLUS NATIVEINT  { Const_nativeint($2) }
;


/* Identifiers and operators */

ident:
| LOWERCASEIDENT { $1 }
| UPPERCASEIDENT { $1 }
;

constructor_ident:
| UPPERCASEIDENT    { $1 }
| LPAREN RPAREN     { "()" }
| FALSE             { "false" }
| TRUE              { "true" }
;

value_ident:
| LOWERCASEIDENT         { $1 }
| LPAREN operator RPAREN { $2}
;

operator:
| AMPERAMPER { "&&" }
| BARBAR     { "||" }
| COLONEQUAL { ":=" }
| EQUAL      { "=" }
| INFIXOP0   { $1 }
| INFIXOP1   { $1 }
| INFIXOP2   { $1 }
| INFIXOP3   { $1 }
| INFIXOP4   { $1 }
| PREFIXOP   { $1 }
;


/* Miscellaneous */

rec_flag:
| /* empty */ { NonRecursive }
| REC         { Recursive }
;

opt_bar:
| /* empty */ { () }
| BAR         { () }
;

subtractive:
| MINUS    { "-" }
| MINUSDOT { "-." }
;
