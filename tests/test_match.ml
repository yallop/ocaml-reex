(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Reex

let matches_ r = .< fun s -> .~(Reex_match.match_  .<0>. .<s>. [r,fun _ ~index ~len _ -> .<.~index = .~len>.] ~otherwise: .<false>.) >.
let matchn rs = .< fun s -> .~(Reex_match.match_  .<0>. .<s>. 
                                 (List.mapi (fun i r -> (r, fun _ ~index ~len:_ s -> .<(i, String.sub .~s 0 .~index)>.)) rs)
                                   ~otherwise: .<(-1, "")>.) >.
let rec repeat n x = if n = 0 then epsilon else x >>> repeat (pred n) x


let test_basics _ =
  let (=~) = Runnative.run
  and (/=~) r = Runnative.run .< fun s -> Stdlib.not (.~r s) >. in begin
    let r1 = matches_ (chr 'a' >>> star (chr 'b') >>> chr 'a') in
    assert (r1 /=~ "");
    assert (r1 /=~ "a");
    assert (r1 =~ "aa");
    assert (r1 =~ "aba");
    assert (r1 =~ "abbba");
    assert (r1 /=~ "abbb");
    assert (r1 /=~ "caba");

    let r2 = matches_ (plus (range '0' '9')) in
    assert (r2 /=~ "");
    assert (r2 /=~ "a");
    assert (r2 =~ "0");
    assert (r2 =~ "0123");
    assert (r2 =~ "4444");

    let r3 = matches_ epsilon in
    assert (r3 =~ "");
    assert (r3 /=~ "a");

    let r4 = matches_ empty in
    assert (r4 /=~ "");
    assert (r4 /=~ "a");

    let r5 = matches_ (repeat 10 (opt (chr 'a')) >>> repeat 10 (chr 'a')) in
    assert (r5 =~ "aaaaaaaaaa");
  end


let test_longest_match _ =
  let case = Runnative.run @@ matchn [regex "aa"; regex "a*b"] in
  begin
    assert_equal (0,"aa") (case "aa");
    assert_equal (0,"aa") (case "aac");
    assert_equal (0,"aa") (case "aaaaac");
    assert_equal (1,"aab") (case "aab");
    assert_equal (1,"ab") (case "abc");
    assert_equal (1,"b") (case "baa");
  end


let test_with_regenerate _ =
  (* (1) generate random regular expressions
     (2) generate random test cases for the regular expressions *)

  let check (re, pos, neg) =
    (* 1. Compile the regular expression. *)
    let cre = Runnative.run (matches_ re) in
    (* 2. Test! *)
    List.for_all (fun s ->  cre s) pos &&
    List.for_all (fun s -> Stdlib.not @@ cre s) neg
  in

  assert_equal 0 (QCheck_runner.run_tests ~verbose:true
                    [QCheck.Test.make Regex_generator.regex_testcase_gen check])


let suite = "Match tests" >::: [
      "basics"     >:: test_basics;
      "longest   " >:: test_longest_match;
      (* "regenerate" >:: test_with_regenerate; *)
    ]


let _ =
  run_test_tt_main suite
