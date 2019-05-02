%{
  open Syntax
%}

%token SEMISEMI
%token LPAREN RPAREN
%token TRUE FALSE
%token IF THEN ELSE
%token PLUS MULT LT

%token <int> INTV
%token <Syntax.id> ID

%left LT
%left PLUS
%left MULT
%nonassoc ELSE

%start main
%type <Syntax.exp> main
%%

main :
  | Expr SEMISEMI { $1 }

Expr :
  | ID { EVar $1 }
  | INTV { EInt $1 }
  | TRUE { EBool true }
  | FALSE { EBool false }
  | Expr PLUS Expr { EBinOp (Plus, $1, $3) }
  | Expr MULT Expr { EBinOp (Mult, $1, $3) }
  | Expr LT Expr { EBinOp (Lt, $1, $3) }
  | IF e1=Expr THEN e2=Expr ELSE e3=Expr { EIfThenElse (e1, e2, e3) }
  | LPAREN Expr RPAREN { $2 }
