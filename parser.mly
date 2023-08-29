%token <float> NUM
%token <string> VAR
%token ADD SUB MUL DIV POW
%token LPAR RPAR
%token EOL

%left ADD SUB
%left MUL DIV
%left POW

%start <float> main

%%

main:
| e = expr EOL
    { e }

expr:
| i = NUM
    { i }
| LPAR e = expr RPAR
    { e }
| e1 = expr ADD e2 = expr
    { e1 +. e2 }
| e1 = expr SUB e2 = expr
    { e1 -. e2 }
| e1 = expr MUL e2 = expr
    { e1 *. e2 }
| e1 = expr DIV e2 = expr
    { e1 /. e2 }
| e1 = expr POW e2 = expr
    { Float.pow e1 e2}