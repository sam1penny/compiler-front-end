type token_type =
  | PLUS
  | MINUS
  | MULT (* TODO: delete at some point, not used in summer work grammar, however used in testing grammar *)
  | CARAT
  | COS
  | EXCLAMATION_MARK
  | NUM
  | LPAREN
  | RPAREN
  | WS
  | EOF

let string_of_token_type = function
  | PLUS -> "+"
  | MINUS -> "-"
  | MULT -> "*"
  | CARAT -> "^"
  | COS -> "cos"
  | EXCLAMATION_MARK -> "!"
  | NUM -> "num"
  | LPAREN -> "("
  | RPAREN -> ")"
  | WS -> "WS"
  | EOF -> "EOF"

type token = Token of token_type * string option

let string_of_token = function
  | Token(ttype, attr) -> Printf.sprintf "%s:%s" (string_of_token_type ttype) (Utils.default "none" attr)
