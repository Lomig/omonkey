type t =
  | ILLEGAL
  | EOF
  | IDENTIFIER of string
  | INTEGER of int
  | EQUAL
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | LT
  | GT
  | GTE
  | LTE
  | EQ
  | NOT_EQ
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | FUNCTION
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN
[@@deriving show]

let pp fmt token = Fmt.pf fmt "%s" (show token)
