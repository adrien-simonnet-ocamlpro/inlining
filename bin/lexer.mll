{
  open Parser

  exception Unexpected_character of string
}

rule tokens = parse
| "(*" { comments lexbuf }
| "(" { LEFT_PARENTHESIS }
| ")" { RIGHT_PARENTHESIS }
| "[" { LEFT_BRACKET }
| "]" { RIGHT_BRACKET }
| "{" { LEFT_BRACE }
| "}" { RIGHT_BRACE }

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

| ":" { COLONS }
| "," { COMMA }
| "." { DOT }

| "_" { JOKER }

| ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']* as id { CONSTRUCTOR_NAME (id) }
| ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* as id { IDENT (id) }

| [' ' '\t' '\n' '\r'] { tokens lexbuf }

| eof { EOF }

| _ as c { raise (Unexpected_character (String.make 1 c)) }

and comments = parse
| "*)" { tokens lexbuf }
| _ { comments lexbuf }
