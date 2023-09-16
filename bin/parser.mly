%{

%}

%token PARENTHESE_OUVRANTE PARENTHESE_FERMANTE
%token CROCHET_OUVRANT CROCHET_FERMANT
//%token ACCOLADE_OUVRANTE ACCOLADE_FERMANTE

%token<int> NAT
%token PLUS MOINS

%token DEUX_POINTS_DEUX_POINTS POINT_VIRGULE

%token IF THEN ELSE WHILE

%token<string> IDENT
%token<string> CONSTRUCTOR_NAME

%token FUN FLECHE

%token LET REC AND IN EGAL

%token MATCH WITH

%token JOKER

%token TYPE OF

%token ASTERISQUE

%token BARRE

//%token REF EXCLAMATION DEUX_POINTS_EGAL

%token DEUX_POINTS VIRGULE POINT

%token EOF

%nonassoc PARENTHESE_OUVRANTE
%nonassoc LET
%nonassoc IN
%nonassoc ELSE
%nonassoc WHILE
%nonassoc FUN
%nonassoc FLECHE
%nonassoc IF
%nonassoc NAT
%nonassoc IDENT
%left MOINS
%left PLUS
%left app

%start <Ast.expr> programme

%type <Ast.expr> expr

%%

programme : expr EOF { $1 } ;

expr :
| PARENTHESE_OUVRANTE PARENTHESE_FERMANTE { Ast.Unit }
| PARENTHESE_OUVRANTE e = expr PARENTHESE_FERMANTE { e }
| n = NAT { Ast.Int n }
| LET i = IDENT EGAL e1 = expr IN e2 = expr { Ast.Let (i, e1, e2) }
| LET i = IDENT fund = fun_definition IN e2 = expr { Ast.Let (i, fund, e2) }
| LET REC bindings = bindings IN e2 = expr { Ast.Let_rec (bindings, e2) }
| FUN args = arguments { args }
| e1 = expr e2 = expr %prec app { Ast.App (e1, e2) }
| i = IDENT { Ast.Var i }
| e1 = expr bop = binary_operator e2 = expr { Ast.Binary (bop, e1, e2) }
| IF cond = expr THEN iftrue = expr ELSE iffalse = expr { Ast.If (cond, iftrue, iffalse) }
| TYPE i = IDENT EGAL constructors = constructors expr = expr { Ast.Type (i, constructors, expr) }
| cname = CONSTRUCTOR_NAME { Ast.Constructor (cname, []) }
| cname = CONSTRUCTOR_NAME expr = expr { Ast.Constructor (cname, [expr]) }
| cname = CONSTRUCTOR_NAME PARENTHESE_OUVRANTE pl = payload_exprs PARENTHESE_FERMANTE { Ast.Constructor (cname, pl) }
| MATCH e = expr WITH ps = patterns { Ast.Match (e, ps) }
| CROCHET_OUVRANT l = list { l }
| hd = expr DEUX_POINTS_DEUX_POINTS tl = expr { Ast.Constructor ("Cons", [hd; tl]) }

binary_operator:
| PLUS { Ast.Add }
| MOINS { Ast.Sub }

constructors :
| BARRE cname = CONSTRUCTOR_NAME { [cname, []] }
| BARRE cname = CONSTRUCTOR_NAME OF ctype = constructor_type { [cname, ctype] }
| BARRE cname = CONSTRUCTOR_NAME constructors = constructors { (cname, []) :: constructors }
| BARRE cname = CONSTRUCTOR_NAME OF ctype = constructor_type constructors = constructors { (cname, ctype) :: constructors }

constructor_type :
| tn = type_name { [tn] }
| tn = type_name ASTERISQUE ctype = constructor_type { tn :: ctype }

type_name :
| ident = IDENT { ident }

patterns :
| BARRE p = pattern FLECHE e = expr { [p, e] }
| BARRE p = pattern FLECHE e = expr ps = patterns { (p, e) :: ps }

pattern :
| i = IDENT { Ast.Joker i }
| cname = CONSTRUCTOR_NAME { Ast.Deconstructor (cname, []) }
| cname = CONSTRUCTOR_NAME ident = IDENT { Ast.Deconstructor (cname, [ident]) }
| cname = CONSTRUCTOR_NAME PARENTHESE_OUVRANTE pl = payload_idents PARENTHESE_FERMANTE { Ast.Deconstructor (cname, pl) }
| JOKER { Ast.Joker "_" }

payload_idents:
| ident = IDENT { [ident] }
| ident = IDENT VIRGULE pl = payload_idents { ident :: pl }

payload_exprs:
| expr = expr { [expr] }
| expr = expr VIRGULE pl = payload_exprs { expr :: pl }

bindings :
| i = IDENT fund = fun_definition { [i, fund] }
| i = IDENT fund = fun_definition AND bindings = bindings { (i, fund) :: bindings }

/*| REF expr { Ast.Ref $2 }
| EXCLAMATION expr { Ast.Deref $2 }
| expr DEUX_POINTS_EGAL expr { Ast.Assign ($1, $3) }*/



/*| ACCOLADE_OUVRANTE enregistrement { $2 }
| expr POINT IDENT { Ast.Field ($1, $3) }*/

arguments:
| i = IDENT FLECHE e = expr { Ast.Fun ([i], e) }
| i = IDENT args = arguments { Ast.Fun ([i], args) }

fun_definition:
| i = IDENT EGAL e1 = expr { Ast.Fun ([i], e1) }
| i = IDENT fund = fun_definition { Ast.Fun ([i], fund) }

list:
| CROCHET_FERMANT { Ast.Constructor ("Empty", []) }
| e = expr CROCHET_FERMANT { Ast.Constructor ("Cons", [e; Ast.Constructor ("Empty", [])]) }
| hd = expr POINT_VIRGULE tl = list { Ast.Constructor ("Cons", [hd; tl]) }

/*enregistrement :
| expr ACCOLADE_FERMANTE { $1 }
| IDENT DEUX_POINTS expr VIRGULE enregistrement { Ast.Record (($1, $3), $5) }*/

%%
