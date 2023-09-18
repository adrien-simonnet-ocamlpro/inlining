{
  open Parser

  exception Unexpected_character of string
}

rule jetons = parse
| "(*" { commentaires lexbuf }
| "(" { LEFT_PARENTHESIS }
| ")" { RIGHT_PARENTHESIS }
| "[" { LEFT_BRACKET }
| "]" { RIGHT_BRACKET }
(*| "{" { LEFT_BRACE }
| "}" { RIGHT_BRACE }*)

| ['0'-'9']+ as integer  { INT (int_of_string integer) }
| "+" { PLUS }
| "-" { MINUS }

| ";" { SEMICOLON }
| "::" { CONS }

| "if" { IF }
| "then" { THEN }
| "else" { ELSE }

| "fun" { FUN }
| "->" { ARROW }

| "let" { LET }
| "rec" { REC }
| "and" { AND }
| "=" { EGAL }
| "in" { IN }

| "match" { MATCH }
| "with" { WITH }

| "type" { TYPE }
| "of" { OF }
| "|" { BAR }
| "*" { STAR }

(*| "ref" { REF }
| "!" { EXCLAMATION }
| ":=" { DEUX_POINTS_EGAL }*)

| ":" { COLONS }
| "," { COMMA }
| "." { DOT }

| "_" { JOKER }

| ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as identificateur { CONSTRUCTOR_NAME (identificateur) }
| ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* as identificateur { IDENT (identificateur) }

| [' ' '\t' '\n' '\r'] { jetons lexbuf }

| eof { EOF }

| _ as c { raise(Unexpected_character (String.make 1 c)) }

and commentaires = parse
| "*)" { jetons lexbuf }
| _ { commentaires lexbuf }
