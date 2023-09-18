%{

%}

%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token LEFT_BRACKET RIGHT_BRACKET
//%token LEFT_BRACE RIGHT_BRACE

%token<int> INT
%token PLUS MINUS

%token CONS SEMICOLON

%token IF THEN ELSE WHILE

%token<string> IDENT
%token<string> CONSTRUCTOR_NAME

%token FUN ARROW

%token LET REC AND IN EGAL

%token MATCH WITH

%token JOKER

%token TYPE OF

%token STAR

%token BAR

//%token REF EXCLAMATION DEUX_POINTS_EGAL

%token COLONS COMMA DOT

%token EOF

%nonassoc LEFT_PARENTHESIS
%nonassoc LET
%nonassoc IN
%nonassoc ELSE
%nonassoc WHILE
%nonassoc FUN
%nonassoc ARROW
%nonassoc IF
%nonassoc INT
%nonassoc IDENT
%left MINUS
%left PLUS
%left app

%start <Ast.expr> file

%type <Ast.expr> expr

%%

file : e = expr EOF { e } ;

expr :
| LEFT_PARENTHESIS RIGHT_PARENTHESIS { Ast.Tuple [] }
| LEFT_PARENTHESIS e = expr RIGHT_PARENTHESIS { e }
| n = INT { Ast.Int n }
| LET i = IDENT EGAL e1 = expr IN e2 = expr { Ast.Let (i, e1, e2) }
| LET i = IDENT COMMA ti = tuple_idents EGAL e1 = expr IN e2 = expr { Ast.Let_tuple (i :: ti, e1, e2) }
| LET i = IDENT fund = fun_definition IN e2 = expr { Ast.Let (i, fund, e2) }
| LET REC bindings = bindings IN e2 = expr { Ast.Let_rec (bindings, e2) }
| FUN args = arguments { args }
| e1 = expr e2 = expr %prec app { Ast.App (e1, e2) }
| i = IDENT { Ast.Var i }
| e1 = expr bop = binary_operator e2 = expr { Ast.Binary (bop, e1, e2) }
| IF cond = expr THEN iftrue = expr ELSE iffalse = expr { Ast.If (cond, iftrue, iffalse) }
| TYPE i = IDENT EGAL constructors = constructors expr = expr { Ast.Type (i, constructors, expr) }
| TYPE i = IDENT EGAL BAR constructors = constructors expr = expr { Ast.Type (i, constructors, expr) }
| cname = CONSTRUCTOR_NAME { Ast.Constructor (cname, []) }
| cname = CONSTRUCTOR_NAME expr = expr { Ast.Constructor (cname, [expr]) }
| cname = CONSTRUCTOR_NAME LEFT_PARENTHESIS pl = payload_exprs RIGHT_PARENTHESIS { Ast.Constructor (cname, pl) }
| MATCH e = expr WITH ps = patterns { Ast.Match (e, ps) }
| LEFT_BRACKET l = list { l }
| hd = expr CONS tl = expr { Ast.Constructor ("Cons", [hd; tl]) }
| LEFT_PARENTHESIS e = expr COMMA t = tuple_exprs RIGHT_PARENTHESIS { Ast.Tuple (e :: t) }

tuple_idents:
| i = IDENT { [i] }
| i = IDENT COMMA ti = tuple_idents { i :: ti }

tuple_exprs:
| e = expr { [e] }
| e = expr COMMA t = tuple_exprs { e :: t }

binary_operator:
| PLUS { Ast.Add }
| MINUS { Ast.Sub }

constructors :
| cname = CONSTRUCTOR_NAME { [cname, []] }
| cname = CONSTRUCTOR_NAME OF ctype = constructor_type { [cname, ctype] }
| cname = CONSTRUCTOR_NAME BAR constructors = constructors { (cname, []) :: constructors }
| cname = CONSTRUCTOR_NAME OF ctype = constructor_type BAR constructors = constructors { (cname, ctype) :: constructors }

constructor_type :
| tn = type_name { [tn] }
| tn = type_name STAR ctype = constructor_type { tn :: ctype }

type_name :
| ident = IDENT { ident }

patterns :
| BAR p = pattern ARROW e = expr { [p, e] }
| BAR p = pattern ARROW e = expr ps = patterns { (p, e) :: ps }

pattern :
| i = IDENT { Ast.Joker i }
| cname = CONSTRUCTOR_NAME { Ast.Deconstructor (cname, []) }
| cname = CONSTRUCTOR_NAME ident = IDENT { Ast.Deconstructor (cname, [ident]) }
| cname = CONSTRUCTOR_NAME LEFT_PARENTHESIS pl = payload_idents RIGHT_PARENTHESIS { Ast.Deconstructor (cname, pl) }
| JOKER { Ast.Joker "_" }

payload_idents:
| ident = IDENT { [ident] }
| ident = IDENT COMMA pl = payload_idents { ident :: pl }

payload_exprs:
| expr = expr { [expr] }
| expr = expr COMMA pl = payload_exprs { expr :: pl }

bindings :
| i = IDENT fund = fun_definition { [i, fund] }
| i = IDENT fund = fun_definition AND bindings = bindings { (i, fund) :: bindings }

/*| REF expr { Ast.Ref $2 }
| EXCLAMATION expr { Ast.Deref $2 }
| expr DEUX_POINTS_EGAL expr { Ast.Assign ($1, $3) }*/



/*| LEFT_BRACE enregistrement { $2 }
| expr DOT IDENT { Ast.Field ($1, $3) }*/

arguments:
| i = IDENT ARROW e = expr { Ast.Fun ([i], e) }
| i = IDENT args = arguments { Ast.Fun ([i], args) }

fun_definition:
| i = IDENT EGAL e1 = expr { Ast.Fun ([i], e1) }
| i = IDENT fund = fun_definition { Ast.Fun ([i], fund) }

list:
| RIGHT_BRACKET { Ast.Constructor ("Empty", []) }
| e = expr RIGHT_BRACKET { Ast.Constructor ("Cons", [e; Ast.Constructor ("Empty", [])]) }
| hd = expr SEMICOLON tl = list { Ast.Constructor ("Cons", [hd; tl]) }

/*enregistrement :
| expr RIGHT_BRACE { $1 }
| IDENT COLONS expr COMMA enregistrement { Ast.Record (($1, $3), $5) }*/

%%
