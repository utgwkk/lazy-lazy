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
%token LLPAREN RLPAREN SEMI
%token PLUS MULT LT CONS
%token MATCH WITH PIPE
%token UNDEFINED

%token <int> INTV
%token <Syntax.id> ID

%right RARROW
%nonassoc LET FUN IF
%right MATCH
%right let_exp fun_exp if_exp
%left LT
%right CONS
%left PLUS
%left MULT
%nonassoc LPAREN ID INTV TRUE FALSE LLPAREN UNDEFINED
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
  | list_expr { $1 }
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
  | MATCH e=Expr WITH option(PIPE) p1=match_pattern RARROW e1=Expr gs=list(match_guard)
    {
      let gs' = (p1, e1) :: gs in
      EMatchWith (e, gs')
    }
  | bioper_fun { $1 }
  | LPAREN Expr RPAREN { $2 }

bioper_fun :
  | LPAREN PLUS RPAREN { EAbs ("##LHS##", EAbs ("##RHS##", EBinOp (Plus, EVar "##LHS##", EVar "##RHS##"))) }
  | LPAREN MULT RPAREN { EAbs ("##LHS##", EAbs ("##RHS##", EBinOp (Mult, EVar "##LHS##", EVar "##RHS##"))) }
  | LPAREN LT RPAREN { EAbs ("##LHS##", EAbs ("##RHS##", EBinOp (Lt, EVar "##LHS##", EVar "##RHS##"))) }

list_expr :
  | LLPAREN RLPAREN { ENil }
  | LLPAREN e=Expr xs=list(list_body) RLPAREN {
      List.fold_right (fun car cdr ->
        EBinOp (Cons, car, cdr)
      ) xs e
    }

list_body :
  | SEMI Expr { $2 }

match_guard :
  | PIPE p=match_pattern RARROW e=Expr { (p, e) }

match_pattern :
  | ID { EVar $1 }
  | INTV { EInt $1 }
  | TRUE { EBool true }
  | FALSE { EBool false }
  | match_list_expr { $1 }
  | hd=match_pattern CONS tl=match_pattern { EBinOp (Cons, hd, tl) }
  | LPAREN match_pattern RPAREN { $2 }

match_list_expr :
  | LLPAREN RLPAREN { ENil }
  | LLPAREN e=Expr xs=list(match_list_body) RLPAREN {
      List.fold_right (fun car cdr ->
        EBinOp (Cons, car, cdr)
      ) xs e
    }

match_list_body :
  | SEMI match_pattern { $2 }
