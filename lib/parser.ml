type t =
  { lexer : Lexer.t
  ; current_token : Token.t
  ; peek_token : Token.t
  ; errors : string list
  }
[@@deriving show]

let make lexer =
  let current_token, lexer = Lexer.next_token lexer in
  let peek_token, lexer = Lexer.next_token lexer in
  { lexer; current_token; peek_token; errors = [] }
;;

type identifier = { id : int } [@@deriving show]
type expression = { id2 : int } [@@deriving show]
type statement = Binding of identifier * expression [@@deriving show]
type program = statement list [@@deriving show]

let next_token parser =
  let current_token = parser.peek_token in
  let peek_token, lexer = Lexer.next_token parser.lexer in
  { lexer; current_token; peek_token; errors = parser.errors }
;;

let peek_error parser expected_token =
  let error_message =
    Fmt.str
      "expected next token to be %a, got %a instead"
      Token.pp
      expected_token
      Token.pp
      parser.peek_token
  in
  { parser with errors = error_message :: parser.errors }
;;

let parse_expression parser =
  let rec aux parser =
    match parser.current_token with
    | Token.SEMICOLON -> Ast.False, parser
    | _ -> aux (next_token parser)
  in
  aux parser
;;

let parse_let_statement parser =
  match parser.peek_token with
  | Token.IDENTIFIER x ->
    let parser = next_token parser in
    let identifier = Ast.Identifier (Ast.Identifier.of_string x) in
    (match parser.peek_token with
     | Token.EQUAL ->
       let expression, parser = parse_expression (next_token parser) in
       Some (Ast.Binding (identifier, expression)), parser
     | _ -> None, peek_error parser Token.EQUAL)
  | _ -> None, peek_error parser (Token.IDENTIFIER "an_identifier")
;;

let parse_return_statement parser =
  let expression, parser = parse_expression (next_token parser) in
  Some (Ast.Return expression), parser
;;

let parse_statement parser =
  match parser.current_token with
  | Token.LET -> parse_let_statement parser
  | Token.RETURN -> parse_return_statement parser
  | _ -> None, parser
;;

let parse input =
  let lexer = Lexer.make input in
  let parser = make lexer in
  let rec aux acc parser =
    match parser.current_token with
    | Token.EOF -> List.rev acc
    | _ ->
      let statement, parser = parse_statement parser in
      (match statement with
       | None ->
         (match parser.errors with
          | [] -> aux acc (next_token parser)
          | error :: _ ->
            aux (Ast.Error error :: acc) { (next_token parser) with errors = [] })
       | Some statement -> aux (statement :: acc) (next_token parser))
  in
  aux [] parser
;;

let errors parser = parser.errors
