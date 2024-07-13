open Core

(******************************************************************************)
(* TYPE Lexer.t                                                               *)
(******************************************************************************)

type t =
  { input : string
  ; position : int
  ; read_position : int
  ; char : char option
  }
[@@deriving show]

(******************************************************************************)
(* HELPER FUNCTIONS                                                           *)
(******************************************************************************)

let peek_char lexer =
  let { input; position = _; read_position; char = _ } = lexer in
  match read_position >= String.length input with
  | true -> None
  | false -> Some input.[read_position]
;;

let read_char lexer =
  { input = lexer.input
  ; position = lexer.read_position
  ; read_position = lexer.read_position + 1
  ; char = peek_char lexer
  }
;;

let read_identifier lexer =
  let is_letter char =
    Char.(('a' <= char && char <= 'z') || ('A' <= char && char <= 'Z') || char = '_')
  in
  let rec aux acc lexer =
    let char = Option.value ~default:' ' lexer.char in
    match is_letter char with
    | false when List.is_empty acc -> None, lexer
    | false -> Some (acc |> List.rev |> String.of_char_list), lexer
    | true -> aux (char :: acc) (read_char lexer)
  in
  aux [] lexer
;;

let read_number lexer =
  let is_digit char = Char.('0' <= char && char <= '9') in
  let rec aux acc lexer =
    let char = Option.value ~default:' ' lexer.char in
    match is_digit char with
    | false when List.is_empty acc -> None, lexer
    | false -> Some (acc |> List.rev |> String.of_char_list |> int_of_string), lexer
    | true -> aux (char :: acc) (read_char lexer)
  in
  aux [] lexer
;;

(******************************************************************************)
(* PUBLIC INTERFACE                                                           *)
(******************************************************************************)

let make input = { input; position = 0; read_position = 0; char = None } |> read_char

let rec next_token lexer =
  match lexer.char, peek_char lexer with
  | None, _ -> Token.EOF, read_char lexer
  (* Spaces are irrelevant in Monkey *)
  | Some ' ', _ | Some '\n', _ | Some '\t', _ | Some '\r', _ ->
    next_token @@ read_char lexer
  | Some '=', Some '=' -> Token.EQ, read_char @@ read_char lexer
  | Some '=', _ -> Token.EQUAL, read_char lexer
  | Some '+', _ -> Token.PLUS, read_char lexer
  | Some '-', _ -> Token.MINUS, read_char lexer
  | Some '!', Some '=' -> Token.NOT_EQ, read_char @@ read_char lexer
  | Some '!', _ -> Token.BANG, read_char lexer
  | Some '/', _ -> Token.SLASH, read_char lexer
  | Some '*', _ -> Token.ASTERISK, read_char lexer
  | Some '<', _ -> Token.LT, read_char lexer
  | Some '>', _ -> Token.GT, read_char lexer
  | Some ';', _ -> Token.SEMICOLON, read_char lexer
  | Some ',', _ -> Token.COMMA, read_char lexer
  | Some '(', _ -> Token.LPAREN, read_char lexer
  | Some ')', _ -> Token.RPAREN, read_char lexer
  | Some '{', _ -> Token.LBRACE, read_char lexer
  | Some '}', _ -> Token.RBRACE, read_char lexer
  | _ ->
    let identifier, lexer = read_identifier lexer in
    (match identifier with
     | Some "fn" -> Token.FUNCTION, lexer
     | Some "let" -> Token.LET, lexer
     | Some "true" -> Token.TRUE, lexer
     | Some "false" -> Token.FALSE, lexer
     | Some "if" -> Token.IF, lexer
     | Some "else" -> Token.ELSE, lexer
     | Some "return" -> Token.RETURN, lexer
     | Some name -> Token.IDENTIFIER name, lexer
     | None ->
       let integer, lexer = read_number lexer in
       (match integer with
        | Some x -> Token.INTEGER x, lexer
        | None -> Token.ILLEGAL, read_char lexer))
;;

let pp fmt lexer = Fmt.pf fmt "%s" (show lexer)
