%{
(* parserが利用する変数、関数、型などの定義 *)
open SyntaxPlusPos
let addtyp x = (x, Type.gentyp ())
let add_pos s = (s, ((Parsing.symbol_start_pos ()), (Parsing.symbol_end_pos ())))
%}

/* (* 字句を表すデータ型の定義 (caml2html: parser_token) *) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
%token MINUS_DOT
%token PLUS_DOT
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token EOF

/* (* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) *) */
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT

/* (* 開始記号の定義 *) */
%type <SyntaxPlusPos.t> exp
%start exp

%%

simple_exp: /* (* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) *) */
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { add_pos (Unit) }
| BOOL
    { add_pos (Bool($1)) }
| INT
    { add_pos (Int($1)) }
| FLOAT
    { add_pos (Float($1)) }
| IDENT
    { add_pos (Var($1)) }
| simple_exp DOT LPAREN exp RPAREN
    { add_pos (Get($1, $4)) }

exp: /* (* 一般の式 (caml2html: parser_exp) *) */
| simple_exp
    { $1 }
| NOT exp
    %prec prec_app
    { add_pos (Not($2)) }
| MINUS exp
    %prec prec_unary_minus
    { match $2 with
    | Float(f), _ -> add_pos (Float(-.f)) (* -1.23などは型エラーではないので別扱い *)
    | e -> add_pos (Neg(e)) }
| exp PLUS exp /* (* 足し算を構文解析するルール (caml2html: parser_add) *) */
    { add_pos (Add($1, $3)) }
| exp MINUS exp
    { add_pos (Sub($1, $3)) }
| exp EQUAL exp
    { add_pos (Eq($1, $3)) }
| exp LESS_GREATER exp
    { add_pos (Not(add_pos (Eq($1, $3)))) }
| exp LESS exp
    { add_pos (Not(add_pos (LE($3, $1)))) }
| exp GREATER exp
    { add_pos (Not(add_pos (LE($1, $3)))) }
| exp LESS_EQUAL exp
    { add_pos (LE($1, $3)) }
| exp GREATER_EQUAL exp
    { add_pos (LE($3, $1)) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { add_pos (If($2, $4, $6)) }
| MINUS_DOT exp
    %prec prec_unary_minus
    { add_pos (FNeg($2)) }
| exp PLUS_DOT exp
    { add_pos (FAdd($1, $3)) }
| exp MINUS_DOT exp
    { add_pos (FSub($1, $3)) }
| exp AST_DOT exp
    { add_pos (FMul($1, $3)) }
| exp SLASH_DOT exp
    { add_pos (FDiv($1, $3)) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let
    { add_pos (Let(addtyp $2, $4, $6)) }
| LET REC fundef IN exp
    %prec prec_let
    { add_pos (LetRec($3, $5)) }
| exp actual_args
    %prec prec_app
    { add_pos (App($1, $2)) }
| elems
    { add_pos (Tuple($1)) }
| LET LPAREN pat RPAREN EQUAL exp IN exp
    { add_pos (LetTuple($3, $6, $8)) }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
    { add_pos (Put($1, $4, $7)) }
| exp SEMICOLON exp
    { add_pos (Let((Id.gentmp Type.Unit, Type.Unit), $1, $3)) }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { add_pos (Array($2, $3)) }
| error
    { failwith ((Util.sprint_pos (Parsing.symbol_start_pos ()) (Parsing.symbol_end_pos ()))
        ^ "Parse error") }

fundef:
| IDENT formal_args EQUAL exp
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

pat:
| pat COMMA IDENT
    { $1 @ [addtyp $3] }
| IDENT COMMA IDENT
    { [addtyp $1; addtyp $3] }

