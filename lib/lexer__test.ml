open Lexer

let rec print_next_tokens number lexer =
  let token, lexer = next_token lexer in
  Fmt.pr "%a@." Token.pp token;
  match number with
  | 0 | 1 -> ()
  | x -> print_next_tokens (x - 1) lexer
;;

(******************************************************************************)
(* Lexer.make                                                                 *)
(******************************************************************************)

(* When the input is empty *)
let%expect_test "make" =
  Fmt.pr "%a" pp (make "");
  [%expect {| { Lexer.input = ""; position = 0; read_position = 1; char = None } |}];
  ()
;;

(* When the input has some characters*)
let%expect_test "make" =
  Fmt.pr "%a" pp (make "=+(){},;");
  [%expect
    {|
    { Lexer.input = "=+(){},;"; position = 0; read_position = 1;
      char = (Some '=') }
    |}];
  ()
;;

(******************************************************************************)
(* Lexer.make                                                                 *)
(******************************************************************************)

(* When the next token is illegal *)
let%expect_test "next_token" =
  let lexer = make "%" in
  let token, _ = next_token lexer in
  Fmt.pr "%a" Token.pp token;
  [%expect {| Token.ILLEGAL |}];
  ()
;;

(* When there is no more token *)
let%expect_test "next_token" =
  let lexer = make "" in
  let token, _ = next_token lexer in
  Fmt.pr "%a" Token.pp token;
  [%expect {| Token.EOF |}];
  ()
;;

(* When reading all tokens one after the other *)
let%expect_test "next_token" =
  let lexer = make "=+(){},;" in
  print_next_tokens 9 lexer;
  [%expect
    {|
    Token.EQUAL
    Token.PLUS
    Token.LPAREN
    Token.RPAREN
    Token.LBRACE
    Token.RBRACE
    Token.COMMA
    Token.SEMICOLON
    Token.EOF
    |}];
  ()
;;

(* When having space characters in the input *)
let%expect_test "space" =
  let lexer = make " +  ; " in
  print_next_tokens 3 lexer;
  [%expect {|
    Token.PLUS
    Token.SEMICOLON
    Token.EOF
    |}];
  ()
;;

(* When the token is an identifier *)
let%expect_test "next_token" =
  let lexer = make "let fn; let @;" in
  print_next_tokens 7 lexer;
  [%expect
    {|
    Token.LET
    Token.FUNCTION
    Token.SEMICOLON
    Token.LET
    Token.ILLEGAL
    Token.SEMICOLON
    Token.EOF
    |}];
  ()
;;

(* When parsing a simple expression *)
let%expect_test "next_token expression" =
  let input =
    {|let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}



10 == 10;
10 != 9;|}
  in
  let lexer = make input in
  print_next_tokens 74 lexer;
  [%expect
    {|
    Token.LET
    (Token.IDENTIFIER "five")
    Token.EQUAL
    (Token.INTEGER 5)
    Token.SEMICOLON
    Token.LET
    (Token.IDENTIFIER "ten")
    Token.EQUAL
    (Token.INTEGER 10)
    Token.SEMICOLON
    Token.LET
    (Token.IDENTIFIER "add")
    Token.EQUAL
    Token.FUNCTION
    Token.LPAREN
    (Token.IDENTIFIER "x")
    Token.COMMA
    (Token.IDENTIFIER "y")
    Token.RPAREN
    Token.LBRACE
    (Token.IDENTIFIER "x")
    Token.PLUS
    (Token.IDENTIFIER "y")
    Token.SEMICOLON
    Token.RBRACE
    Token.SEMICOLON
    Token.LET
    (Token.IDENTIFIER "result")
    Token.EQUAL
    (Token.IDENTIFIER "add")
    Token.LPAREN
    (Token.IDENTIFIER "five")
    Token.COMMA
    (Token.IDENTIFIER "ten")
    Token.RPAREN
    Token.SEMICOLON
    Token.BANG
    Token.MINUS
    Token.SLASH
    Token.ASTERISK
    (Token.INTEGER 5)
    Token.SEMICOLON
    (Token.INTEGER 5)
    Token.LT
    (Token.INTEGER 10)
    Token.GT
    (Token.INTEGER 5)
    Token.SEMICOLON
    Token.IF
    Token.LPAREN
    (Token.INTEGER 5)
    Token.LT
    (Token.INTEGER 10)
    Token.RPAREN
    Token.LBRACE
    Token.RETURN
    Token.TRUE
    Token.SEMICOLON
    Token.RBRACE
    Token.ELSE
    Token.LBRACE
    Token.RETURN
    Token.FALSE
    Token.SEMICOLON
    Token.RBRACE
    (Token.INTEGER 10)
    Token.EQ
    (Token.INTEGER 10)
    Token.SEMICOLON
    (Token.INTEGER 10)
    Token.NOT_EQ
    (Token.INTEGER 9)
    Token.SEMICOLON
    Token.EOF
    |}];
  ()
;;
