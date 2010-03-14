%{
  open Syntax

  let mkexp e = e
  let mkabstr idl e = mkexp (if idl = [] then e else (Abstr(idl, e)))
  let mkapp e el = mkexp (App(e, el))
  let mkident id = mkexp (Ident(id))
  let mkinfix e1 op e2 = mkapp (mkident op) [e1; e2]
%}

/* Tokens */

%token AMPERAMPER
%token BARBAR
%token <char> CHAR
%token COLONEQUAL
%token COMMA
%token DOT
%token ELSE
%token EOF
%token EQUAL
%token <float> FLOAT
%token <string> IDENT
%token IF
%token IN
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token <int> INT
%token LAMBDA
%token LET
%token LPAREN
%token <string> PREFIXOP
%token REC
%token RPAREN
%token SEMI
%token <string> STRING
%token THEN

/* Precedences and associativities */

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc LET
%nonassoc THEN
%nonassoc ELSE
%right    COLONEQUAL
%nonassoc below_COMMA
%left     COMMA
%right    BARBAR
%right    AMPERAMPER
%left     INFIXOP0 EQUAL
%right    INFIXOP1
%left     INFIXOP2
%left     INFIXOP3
%right    INFIXOP4
%nonassoc FLOAT IDENT INT LPAREN PREFIXOP STRING

/* Entry points */

%start expression
%type <Syntax.t> expression

%%


/* Entry points */

expression:
| seq_expr EOF { $1 }
;


/* Expressions */

seq_expr:
| expr %prec below_SEMI
    { $1 }
| expr SEMI
    { $1 }
| expr SEMI seq_expr
    { mkexp (Sequence($1, $3)) }
;

expr:
| simple_expr
    { $1 }
| simple_expr simple_expr_list
    { mkapp $1 (List.rev $2) }
| LET IDENT ident_list EQUAL seq_expr IN seq_expr
    { mkexp (Let($2, mkabstr $3 $5, $7)) }
| LET REC IDENT ident_list EQUAL seq_expr IN seq_expr
    { mkexp (LetRec($3, mkabstr $4 $6, $8)) }
| LET LPAREN ident_comma_list RPAREN EQUAL seq_expr IN seq_expr
    { mkexp (LetTuple($3, $6, $8)) }
| LAMBDA ident_comma_list DOT seq_expr
    { mkabstr $2 $4 }
| expr_comma_list %prec below_COMMA
    { mkexp (Tuple(List.rev $1)) }
| IF seq_expr THEN expr ELSE expr
    { mkexp (If($2, $4, $6)) }
| IF seq_expr THEN expr
    { mkexp (If($2, $4, mkident "()")) }
| expr AMPERAMPER expr
    { mkexp (If($1, $3, mkident "false")) }
| expr BARBAR expr
    { mkexp (If($1, mkident "true", $3)) }
| expr COLONEQUAL expr
    { mkinfix $1 ":=" $3 }
| expr EQUAL expr
    { mkinfix $1 "=" $3 }
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
;

expr_comma_list:
| expr_comma_list COMMA expr
    { $3 :: $1 }
| expr COMMA expr
    { [$3; $1] }
;

simple_expr:
| INT
    { mkexp (Int($1)) }
| CHAR
    { mkexp (Char($1)) }
| FLOAT
    { mkexp (Float($1)) }
| STRING
    { mkexp (String($1)) }
| IDENT
    { mkident $1 }
| LPAREN operator RPAREN
    { mkident $2 }
| LPAREN seq_expr RPAREN
    { $2 }
| PREFIXOP simple_expr
    { mkapp (mkident $1) [$2] }
;

simple_expr_list:
| simple_expr
    { [$1] }
| simple_expr_list simple_expr
    { $2 :: $1 }
;

ident_comma_list:
| IDENT COMMA IDENT
    { [$1; $3] }
| IDENT COMMA ident_comma_list
    { $1 :: $3 }
;

ident_list:
| /* empty */
    { [] }
| IDENT ident_list
    { $1 :: $2 }
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
