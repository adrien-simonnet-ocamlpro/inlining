%{

%}

%token PARENTHESE_OUVRANTE PARENTHESE_FERMANTE
//%token CROCHET_OUVRANT CROCHET_FERMANT
//%token ACCOLADE_OUVRANTE ACCOLADE_FERMANTE

%token<int> NAT
%token PLUS MOINS

%token PRINT

//%token NIL CONS DEUX_POINTS_DEUX_POINTS HD TL

%token IFZERO /*IFEMPTY*/ THEN ELSE WHILE

%token<string> IDENT

%token FUN /*FIX*/ FLECHE

%token LET REC AND IN EGAL

%token MATCH WITH

%token JOKER

%token TYPE OF

%token BARRE

//%token REF EXCLAMATION DEUX_POINTS_EGAL

//%token DEUX_POINTS VIRGULE POINT

%token EOF

%nonassoc PARENTHESE_OUVRANTE
%nonassoc LET
%nonassoc IN
%nonassoc IFZERO
%nonassoc ELSE
%nonassoc WHILE
%nonassoc FUN
%nonassoc FLECHE
%nonassoc NAT
%nonassoc IDENT
%left MOINS
%left PLUS
%left app
%left PRINT

%start <string Ast.expr> programme

%type <string Ast.expr> terme

%%

programme : terme EOF { $1 } ;

terme :
/*| PARENTHESE_OUVRANTE PARENTHESE_FERMANTE { Ast.Unit }*/
| PARENTHESE_OUVRANTE e = terme PARENTHESE_FERMANTE { e }

| NAT { Ast.Prim (Cps.Const $1, []) }
| e1 = terme PLUS e2 = terme { Ast.Prim (Cps.Add, [e1; e2]) }
| e1 = terme MOINS e2 = terme { Ast.Prim (Cps.Sub, [e1; e2]) }

| PRINT e = terme { Ast.Prim (Cps.Print, [e]) }

/*| NIL { Ast.Nil }
| CONS terme terme { Ast.Cons ($2, $3) }
| terme DEUX_POINTS_DEUX_POINTS terme { Ast.Cons ($1, $3) }
| HD terme { Ast.Head ($2) }
| TL terme { Ast.Tail ($2) }*/

| IFZERO cond = terme THEN iftrue = terme ELSE iffalse = terme { Ast.If (cond, iftrue, iffalse) }
/*| IFEMPTY terme THEN terme ELSE terme { Ast.Ifempty ($2, $4, $6) }*/

| i = IDENT { Ast.Var i }

| FUN args = arguments { args }
/*| FIX PARENTHESE_OUVRANTE IDENT FLECHE terme PARENTHESE_FERMANTE { Ast.Fix ($3, $5) }*/
| e1 = terme e2 = terme %prec app { Ast.App (e1, e2) }

| LET REC bindings = bindings IN e2 = terme { Ast.Let_rec (bindings, e2) }
| LET i = IDENT EGAL e1 = terme IN e2 = terme { Ast.Let (i, e1, e2) }

| MATCH e = terme WITH ps = patterns { Ast.Match (e, ps) }

patterns :
| BARRE p = pattern FLECHE e = terme { [p, e] }
| BARRE p = pattern FLECHE e = terme ps = patterns { (p, e)::ps }

pattern :
| n = NAT { Ast.Int n }
| JOKER { Ast.Joker }

bindings :
| i = IDENT EGAL e1 = terme { [i, e1] }
| i = IDENT EGAL e1 = terme AND bindings = bindings { (i, e1)::bindings }

/*| REF terme { Ast.Ref $2 }
| EXCLAMATION terme { Ast.Deref $2 }
| terme DEUX_POINTS_EGAL terme { Ast.Assign ($1, $3) }*/

/*| CROCHET_OUVRANT liste { $2 }*/

/*| ACCOLADE_OUVRANTE enregistrement { $2 }
| terme POINT IDENT { Ast.Field ($1, $3) }*/

arguments :
| i = IDENT FLECHE e = terme { Ast.Fun (i, e) }
| i = IDENT args = arguments { Ast.Fun (i, args) }

/*liste :
| CROCHET_FERMANT { Ast.Nil }
| terme CROCHET_FERMANT { Ast.Cons ($1, Ast.Nil) }
| terme liste { Ast.Cons ($1, $2) }*/

/*enregistrement :
| terme ACCOLADE_FERMANTE { $1 }
| IDENT DEUX_POINTS terme VIRGULE enregistrement { Ast.Record (($1, $3), $5) }*/

%%
