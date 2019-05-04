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
%token NIL
%token PLUS MULT LT CONS
%token MATCH WITH PIPE
%token UNDEFINED

%token <int> INTV
%token <Syntax.id> ID

%nonassoc LET FUN IF MATCH
%right let_exp fun_exp if_exp match_exp
%left LT
%right CONS
%left PLUS
%left MULT
%nonassoc LPAREN ID INTV TRUE FALSE NIL UNDEFINED
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
  | NIL { ENil }
  | UNDEFINED { EUndefined }
  | Expr Expr %prec application { EApp ($1, $2) }
  | Expr PLUS Expr { EBinOp (Plus, $1, $3) }
  | Expr MULT Expr { EBinOp (Mult, $1, $3) }
  | Expr LT Expr { EBinOp (Lt, $1, $3) }
  | Expr CONS Expr { EBinOp (Cons, $1, $3) }
  | IF e1=Expr THEN e2=Expr ELSE e3=Expr %prec if_exp { EIfThenElse (e1, e2, e3) }
  | LET x=ID xs=list(ID) EQ e1=Expr IN e2=Expr
    %prec let_exp
    {
      let e1' =
        List.fold_right (fun x e ->
          EAbs (x, e)
        ) xs e1
      in
      ELet (x, e1', e2)
    }
  | LET REC f=ID xs=list(ID) EQ e1=Expr IN e2=Expr
    %prec let_exp
    {
      let e1' =
        List.fold_right (fun x e ->
          EAbs (x, e)
        ) xs e1
      in
      ELetRec (f, e1', e2)
    }
  | FUN xs=nonempty_list(ID) RARROW e=Expr
    %prec fun_exp
    {
      let e' =
        List.fold_right (fun x e ->
          EAbs (x, e)
        ) xs e
      in e'
    }
  | MATCH e1=Expr WITH option(PIPE) NIL RARROW enil=Expr PIPE xcar=ID CONS xcdr=ID RARROW econs=Expr
    %prec match_exp
    {
      if xcar = xcdr then failwith "Cons identifier names should not be the same.";
      EMatchWith (e1, enil, xcar, xcdr, econs)
    }
  | LPAREN Expr RPAREN { $2 }
