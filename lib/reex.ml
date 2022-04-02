(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Reex_types
module Types = Reex_types

type t = Reex_types.Normal.t = private
           Chars of Reex_types.Charset.t
         | Seq of t list (* r1 r2 ... rn ε (n != 1) *)
         | Alt of Reex_types.NormalSet.t (* r1 | r2 | ... | rn (n >= 2) *)
         | Inter of Reex_types.NormalSet.t (* r1 & r2 & ... & rn (n >= 2) *)
         | Star of t
         | Not of t
module Re_set = Reex_types.NormalSet

let equal = equal
let compare = compare
let pp_ast = pp
let opt t = t <|> epsilon
let plus t = t >>> star t
let str s = Seq.fold_left (fun s c -> s >>> chr c) epsilon (String.to_seq s)
let alt = List.fold_left (<|>) empty
let inter = List.fold_left (<&>) top 
let seq = List.fold_left (>>>) epsilon
let epsilon = epsilon
let any = any
let empty = empty
let chr = chr
let (<|>) = (<|>)
let (<&>) = (<&>)
let (>>>) = (>>>)
let star = star
let not = not
let top = top
let range l h = chars (Charset.range l h)

exception Parse_error = Reex_parse.Parse_error
let regex = Reex_parse.regex

let pp = Reex_print.pp
