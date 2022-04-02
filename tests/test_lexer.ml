(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Reex
open OUnit2


let test_trivial_lexer _ =
  let open Leex in
  let cases = [
      (chr 'a' => fun _ -> Return .< `a >.);
      (chr 'b' => fun _ -> Return .< `b >.);
      (chr 'c' => fun _ -> Return .< `c >.);
    ] in
  let lexer_code = compile cases in
  let lexer = Runnative.run lexer_code in
  begin
    assert (lexer ~start:0 "a" = (`a, 1));
    assert (lexer ~start:0 "b" = (`b, 1));
    assert (lexer ~start:0 "c" = (`c, 1));
  end


let test_sexp_lexer _ =
  let open Leex in
  let idchar = range 'A' 'Z' <|> range 'a' 'z' <|> chr '-' in
  let whitespace = chr ' ' <|> chr '\n' <|> chr '\t' <|> chr '\r' in
  let cases = [
      (chr '(' => fun _ -> Return .< `LPAREN >.);
      (chr ')' => fun _ -> Return .< `RPAREN >.);
      (idchar >>> star idchar => fun s -> Return .< `ID .~s >.);
      (whitespace => fun _ -> Skip);
      (epsilon => fun _ -> Return .< `EOF >.);
      (any => fun _ -> Error "Unexpected character")
    ] in
  let lexer_code = compile cases in
  let lexer = Runnative.run lexer_code in
  let tokens_of_string s =
    let rec go s i acc =
      match lexer ~start:i s with
      | `EOF, _ -> List.rev (`EOF :: acc)
      | tok, i' -> go s i' (tok :: acc)
    in go s 0 []
  in
  begin
    assert (lexer ~start:0 "(" = (`LPAREN, 1));
    assert (lexer ~start:0 ")" = (`RPAREN, 1));
    assert (lexer ~start:0 "aaa" = (`ID "aaa", 3));
    assert (lexer ~start:0 "aaa(" = (`ID "aaa", 3));
    assert (tokens_of_string "(a-b-c  ())  " = [`LPAREN; `ID "a-b-c"; `LPAREN; `RPAREN; `RPAREN; `EOF]);
    assert (tokens_of_string "  " = [`EOF]);
  end


let suite = "Lexer tests" >::: [
      "trivial" >:: test_trivial_lexer;
      "sexp"    >:: test_sexp_lexer
    ]


let _ =
  run_test_tt_main suite
