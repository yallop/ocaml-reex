(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Reex

let repn i j re = seq (List.init i (fun _ -> re)
                     @ List.init (j - i) (fun _ -> opt re))

let chrs_except exclude =
  let l = ref empty in
  for i = 255 downto 0 do
    let c = Char.chr i in
    if Stdlib.not (List.mem c exclude) then
      l := chr c <|> !l
  done;
  !l

let test_chrs = List.init 12 (fun _ -> Char.chr (Random.int 256))

let rec to_re : char Regenerate.Regex.t -> Reex.t = function
  | Regenerate.Regex.One -> epsilon
  | Set (true, l) -> alt (List.map chr l)
  | Set (false, l) -> chrs_except l
  | Seq (l, r) -> to_re l >>> to_re r
  | Or (l, r) -> to_re l <|> to_re r
  | And (l, r) -> (to_re l <&> to_re r)
  | Not r -> not (to_re r)
  | Rep (i, Some j, r) -> repn i j (to_re r)
  | Rep (n, None, r) -> repn n n (to_re r) >>> star (to_re r)

let regex_testcase_gen : (Reex.t * string list * string list) QCheck.arbitrary =
  QCheck.map (fun (r,p,n) -> (to_re r, p, n)) @@
  Regenerate.arbitrary
    (module Regenerate.Word.String)
    (module Regenerate.Segments.Trie.Make(Regenerate.Word.String))
    ~compl:false
    ~pp:Format.pp_print_char
    ~samples:10
    test_chrs

let regex_gen : Reex.t QCheck.arbitrary =
  QCheck.make (QCheck.Gen.map to_re (Regenerate.Regex.gen ~compl:false 
                                       QCheck.Gen.char))
    ~print:(fun r -> Format.asprintf "%a@\n  @[%a@]" Reex.pp r Reex.pp_ast r)
