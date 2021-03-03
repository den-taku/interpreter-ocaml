%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ
%token RARROW FUN
%token REC

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | LET x=ID EQ e=Expr SEMISEMI { Decl (x, e) }
  | LET REC f=ID EQ FUN x=ID RARROW e=Expr SEMISEMI { RecDecl (f, x, e) }

Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=ANDExpr { e } 
  | e=FunExpr { e } 
  | e=AppExpr { e }
  | e=LetRecExpr { e }

LetExpr : 
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }    

ANDExpr : 
    l=ANDExpr AND r=ORExpr { BinOp (And, l, r)}
  | e=ORExpr { e }

ORExpr : 
    l=ORExpr OR r=LTExpr { BinOp (Or, l, r)}
  | e=LTExpr { e } 

LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=AExpr { BinOp (Mult, l, r) }
  | e=AExpr { e }

FunExpr :
    FUN x=ID RARROW e=Expr { FunExp (x, e) }

LetRecExpr : 
    LET REC f=ID EQ FUN p=ID RARROW e1=Expr IN e2=Expr { LetRecExp (f, p, e1, e2) }

AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }

AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }
