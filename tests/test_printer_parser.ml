(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2

let show = Format.asprintf "%a" Reex.pp and read = Reex.regex

let high_chars = String.init 128 (fun x -> Char.chr (128 + x))
let lowercase = "abcdefghijklmnopqrstuvwxyz"
let uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let digits = "0123456789"

let bracket_cases = [
    (* basics *)
    "[a-z]", lowercase;
    "[a-zA-Z]", lowercase ^ uppercase;
    "[A-Za-z]", lowercase ^ uppercase;
    "[A-Z0-9a-z]", lowercase ^ uppercase ^ digits;
    "[0-9A-Za-z]", lowercase ^ uppercase ^ digits;

    (* all ASCII characters *)
    (* full range *)
    "[\000-\127]", String.init 128 Char.chr;
    (* full enumeration *)
    "[]\000\001\002\003\004\005\006\007\b\t\n\011\012\r\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\^_`abcdefghijklmnopqrstuvwxyz{|}~\127]", String.init 128 Char.chr;
    (* multiple ranges *)
    "[\000\001-\015\016\017-%&-z{-\127]", String.init 128 Char.chr;
    (* reordered ranges *)
    "[\017-%\000{-\127\001-\015\016&-z]", String.init 128 Char.chr;

    (* all characters *)
    (* full range *)
    "[\000-\255]", String.init 256 Char.chr;
    (* full enumeration *)
    "[]\000\001\002\003\004\005\006\007\b\t\n\011\012\r\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\^_`abcdefghijklmnopqrstuvwxyz{|}~\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159\160\161\162\163\164\165\166\167\168\169\170\171\172\173\174\175\176\177\178\179\180\181\182\183\184\185\186\187\188\189\190\191\192\193\194\195\196\197\198\199\200\201\202\203\204\205\206\207\208\209\210\211\212\213\214\215\216\217\218\219\220\221\222\223\224\225\226\227\228\229\230\231\232\233\234\235\236\237\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\253\254\255]", String.init 256 Char.chr;
    (* multiple ranges *)
    "[\000\001-\031 -?@-wx-\169\170\171\172\173\174-\255]", String.init 256 Char.chr;

    (* reordered ranges *)
    "[\174-\255\001-\031 -?x-\169\000\170\171\172@-w\173]", String.init 256 Char.chr;

    (* initial and non-initial carets *)
    "[^a-zA-Z0-9]", "[]\000\001\002\003\004\005\006\007\b\t\n\011\012\r\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./:;<=>?@[\\^_`{|}~\127]" ^ high_chars;
    "[a-z^A-Z0-9]", lowercase ^ uppercase ^ digits ^ "^";
    "[a-zA-Z0-9^]", lowercase ^ uppercase ^ digits ^ "^";

    (* right bracket placement *)
    "[]a]", "]a";
    "[a]", "a";

    (* sets with a left bracket *)
    "[[a]", "[a";
    "[a[]", "[a";
  ]


let test_bracket_parsing _ =
  List.iter 
    (fun (bracketed, cs) -> assert_equal 
                              (Reex.alt (List.map Reex.chr (List.init (String.length cs) (fun i -> cs.[i]))))
                              (read bracketed)
                              ~cmp:Reex.equal
                              ~msg:bracketed
                              ~printer:show)
    bracket_cases


let test_bracket_roundtrip _ =
  List.iter 
    (fun (bracketed, _) -> assert_equal 
                             (read bracketed)
                             (read (show (read bracketed)))
                             ~cmp:Reex.equal
                             ~msg:bracketed
                             ~printer:show)
    bracket_cases


let test_regenerate_roundtrip _ =
  (* Test round-trip printing & parsing for randomly-generated regular expressions *)
  let check re = Reex.equal (read (show re)) re in
  assert_equal 0 (QCheck_runner.run_tests ~verbose:true [QCheck.Test.make Regex_generator.regex_gen check])


let suite = "Printer-parser tests" >::: [
      "bracket_parsing"      >:: test_bracket_parsing;
      "bracket_roundtrip"    >:: test_bracket_roundtrip;
      "regenerate_roundtrip" >:: test_regenerate_roundtrip;
    ]


let _ =
  run_test_tt_main suite
