open Core
open Parser

let print_statements string =
  Fmt.pr "--@.";
  parse string |> List.iter ~f:(Fmt.pr "%a@." Ast.pp_statement)
;;

(* Valid Let statements *)
let%expect_test "parse let statements" =
  print_statements {|
    let x = 5;
    let y = 10;
    let foobar = 838383;
  |};
  [%expect {|
    --
    (Ast.Binding ((Ast.Identifier "x"), Ast.False))
    (Ast.Binding ((Ast.Identifier "y"), Ast.False))
    (Ast.Binding ((Ast.Identifier "foobar"), Ast.False))
    |}];
  ()
;;

(* Valid Return statements *)
let%expect_test "parse let statements" =
  print_statements {|
    return 5;
    return 10;
    return 838383;
  |};
  [%expect {|
    --
    (Ast.Return Ast.False)
    (Ast.Return Ast.False)
    (Ast.Return Ast.False)
    |}];
  ()
;;


(* Invalid statements *)
let%expect_test "parse invalid let statement" =
  print_statements {|
    let x 5;
    let = 10;
    let foobar = 838383;
  |};
  [%expect {|
    --
    (Ast.Error
       "expected next token to be Token.EQUAL, got (Token.INTEGER 5) instead")
    (Ast.Error
       "expected next token to be (Token.IDENTIFIER \"an_identifier\"), got Token.EQUAL instead")
    (Ast.Binding ((Ast.Identifier "foobar"), Ast.False))
    |}];
  ()
;;

