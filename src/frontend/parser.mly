%{
  open Syntax
%}

%token SEMISEMI
%token LPAREN RPAREN
%token TRUE FALSE
%token IF THEN ELSE
%token LET REC IN
%token EQ
%token FUN
%token RARROW
%token PLUS MULT LT

%token <int> INTV
%token <Syntax.id> ID

%nonassoc LET FUN IF
%right let_exp fun_exp if_exp
%left LT
%left PLUS
%left MULT
%nonassoc LPAREN ID INTV TRUE FALSE
%nonassoc application

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
  | Expr Expr %prec application { EApp ($1, $2) }
  | Expr PLUS Expr { EBinOp (Plus, $1, $3) }
  | Expr MULT Expr { EBinOp (Mult, $1, $3) }
  | Expr LT Expr { EBinOp (Lt, $1, $3) }
  | IF e1=Expr THEN e2=Expr ELSE e3=Expr %prec if_exp { EIfThenElse (e1, e2, e3) }
  | LET x=ID EQ e1=Expr IN e2=Expr %prec let_exp { ELet (x, e1, e2) }
  | LET REC f=ID EQ FUN x=ID RARROW e1=Expr IN e2=Expr %prec let_exp { ELetRec (f, x, e1, e2) }
  | FUN x=ID RARROW e=Expr %prec fun_exp { EAbs (x, e) }
  | LPAREN Expr RPAREN { $2 }
