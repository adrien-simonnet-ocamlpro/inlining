{
  open Parser

  exception Unexpected_character of string
}

rule jetons = parse
| "(*" { commentaires lexbuf }
| "(" { PARENTHESE_OUVRANTE }
| ")" { PARENTHESE_FERMANTE }
| "[" { CROCHET_OUVRANT }
| "]" { CROCHET_FERMANT }
(*| "{" { ACCOLADE_OUVRANTE }
| "}" { ACCOLADE_FERMANTE }*)

| ['0'-'9']+ as integer  { NAT (int_of_string integer) }
| "+" { PLUS }
| "-" { MOINS }

| ";" { POINT_VIRGULE }
| "::" { DEUX_POINTS_DEUX_POINTS }

| "if" { IF }
| "then" { THEN }
| "else" { ELSE }

| "fun" { FUN }
| "->" { FLECHE }

| "let" { LET }
| "rec" { REC }
| "and" { AND }
| "=" { EGAL }
| "in" { IN }

| "match" { MATCH }
| "with" { WITH }

| "type" { TYPE }
| "of" { OF }
| "|" { BARRE }
| "*" { ASTERISQUE }

(*| "ref" { REF }
| "!" { EXCLAMATION }
| ":=" { DEUX_POINTS_EGAL }*)

| ":" { DEUX_POINTS }
| "," { VIRGULE }
| "." { POINT }

| "_" { JOKER }

| ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as identificateur { CONSTRUCTOR_NAME (identificateur) }
| ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* as identificateur { IDENT (identificateur) }

| [' ' '\t' '\n' '\r'] { jetons lexbuf }

| eof { EOF }

| _ as c { raise(Unexpected_character (String.make 1 c)) }

and commentaires = parse
| "*)" { jetons lexbuf }
| _ { commentaires lexbuf }
