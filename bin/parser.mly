%{

%}

%token LEFT_PARENTHESIS RIGHT_PARENTHESIS
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_BRACE RIGHT_BRACE

%token<int> INT
%token PLUS MINUS

%token CONS SEMICOLON

%token IF THEN ELSE WHILE

%token<string> IDENT
%token<string> CONSTRUCTOR_NAME

%token FUN ARROW

%token LET REC AND IN EGAL

%token MATCH WITH

%token TYPE OF

%token STAR

%token BAR

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
| TYPE i = IDENT EGAL td = type_declaration expr = expr { Ast.Type (i, td, expr) }
| cname = CONSTRUCTOR_NAME { Ast.Constructor (cname, []) }
| cname = CONSTRUCTOR_NAME expr = expr { Ast.Constructor (cname, [expr]) }
| cname = CONSTRUCTOR_NAME LEFT_PARENTHESIS pl = payload_exprs RIGHT_PARENTHESIS { Ast.Constructor (cname, pl) }
| MATCH e = expr WITH ps = patterns { Ast.Match (e, ps) }
| LEFT_BRACKET l = list { l }
| hd = expr CONS tl = expr { Ast.Constructor ("Cons", [hd; tl]) }
| LEFT_PARENTHESIS e = expr COMMA t = tuple_exprs RIGHT_PARENTHESIS { Ast.Tuple (e :: t) }
| LEFT_BRACE r = record_construction RIGHT_BRACE { Ast.Record_construction r }
| e = expr DOT i = IDENT { Ast.Record_field (e, i) }

type_definition:
| i = IDENT { Ast.Type_name i }
| tdef1 = type_definition STAR tdef2 = type_definition { Ast.Star (tdef1, tdef2) }
| tdef1 = type_definition ARROW tdef2 = type_definition { Ast.Arrow (tdef1, tdef2) }

type_declaration:
| tdef = type_definition { Ast.Alias tdef }
| cdef = constructors_definition { Ast.Data cdef }
| BAR cdef = constructors_definition { Ast.Data cdef }
| LEFT_BRACE fdef = fields_definition RIGHT_BRACE { Ast.Record fdef }

tuple_idents:
| i = IDENT { [i] }
| i = IDENT COMMA ti = tuple_idents { i :: ti }

tuple_exprs:
| e = expr { [e] }
| e = expr COMMA t = tuple_exprs { e :: t }

binary_operator:
| PLUS { Ast.Add }
| MINUS { Ast.Sub }

constructor_types:
| tn = type_definition { [tn] }
| tn = type_definition STAR tdef = constructor_types { tn :: tdef }

constructors_definition :
| cname = CONSTRUCTOR_NAME { [cname, []] }
| cname = CONSTRUCTOR_NAME OF tdef = constructor_types { [cname, tdef] }
| cname = CONSTRUCTOR_NAME BAR cdef = constructors_definition { (cname, []) :: cdef }
| cname = CONSTRUCTOR_NAME OF tdef = constructor_types BAR cdef = constructors_definition { (cname, tdef) :: cdef }

fields_definition:
| i = IDENT COLONS t = type_definition { [i, t] }
| i = IDENT COLONS t = type_definition SEMICOLON r = fields_definition { (i, t) :: r }

record_construction:
| i = IDENT EGAL e = expr { [i, e] }
| i = IDENT EGAL e = expr SEMICOLON r = record_construction { (i, e) :: r }

patterns :
| BAR p = pattern ARROW e = expr { [p, e] }
| BAR p = pattern ARROW e = expr ps = patterns { (p, e) :: ps }

pattern :
| i = IDENT { Ast.Joker i }
| cname = CONSTRUCTOR_NAME { Ast.Deconstructor (cname, []) }
| cname = CONSTRUCTOR_NAME ident = IDENT { Ast.Deconstructor (cname, [ident]) }
| cname = CONSTRUCTOR_NAME LEFT_PARENTHESIS pl = payload_idents RIGHT_PARENTHESIS { Ast.Deconstructor (cname, pl) }

payload_idents:
| ident = IDENT { [ident] }
| ident = IDENT COMMA pl = payload_idents { ident :: pl }

payload_exprs:
| expr = expr { [expr] }
| expr = expr COMMA pl = payload_exprs { expr :: pl }

bindings :
| i = IDENT fund = fun_definition { [i, fund] }
| i = IDENT fund = fun_definition AND bindings = bindings { (i, fund) :: bindings }

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

%%
