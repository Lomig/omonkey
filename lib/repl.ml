let[@const] prompt = ">> "

let eval input =
  let lexer = Lexer.make input in
  let rec aux acc lexer =
    let token, lexer = Lexer.next_token lexer in
    match token with
    | Token.EOF -> acc |> List.rev_map (Fmt.to_to_string Token.pp) |> String.concat "\n"
    | _ -> aux (token :: acc) lexer
  in
  aux [] lexer
;;

let start =
  let rec loop () =
    print_string prompt;
    flush stdout;
    match read_line () with
    | "exit" -> print_endline "Goodbye!"
    | input ->
      print_endline @@ eval input;
      loop ()
  in
  print_endline
    ("\nHello "
     ^ (Unix.getpwuid (Unix.geteuid ())).Unix.pw_name
     ^ "! This is the Monkey programming language!\n"
     ^ "(`exit` to quit)\n");
  loop
;;
