exception Error

type token = 
  | TRUE
  | STRING of (string)
  | RIGHT_BRACKET
  | RIGHT_BRACE
  | NULL
  | LEFT_BRACKET
  | LEFT_BRACE
  | INT of (int)
  | ID of (string)
  | FLOAT of (float)
  | FALSE
  | EOF
  | COMMA
  | COLON


val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.value option)