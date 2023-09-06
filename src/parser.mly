%{
open Operation
%}

%token <float> NUM
%token <string> ID
%token ADD SUB MUL DIV POW
%token LPAR RPAR COMMA
%token EOL

%left ADD SUB
%left MUL DIV
%left POW

%start <operation_t> main

%%

main:
| e = expr EOL
    { e }

expr:
| i = NUM
    { Val i }
| s = ID LPAR e = separated_list(COMMA, expr) RPAR
    { Fun(s, Array.of_list e) }
| s = ID
    { Var s }
| LPAR e = expr RPAR
    { e }
| e1 = expr ADD e2 = expr
    { Bop (Add, e1, e2) }
| e1 = expr SUB e2 = expr
    { Bop (Sub, e1, e2) }
| SUB e = expr
    { Neg e }
| e1 = expr MUL e2 = expr
    { Bop (Mul, e1, e2) }
| e1 = expr DIV e2 = expr
    { Bop (Div, e1, e2) }
| e1 = expr POW e2 = expr
    { Bop (Pow, e1, e2) }