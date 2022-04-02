(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type 'a case
and 'f rhs = Skip | Error of string | Return of 'f code

val (=>) : Reex.t -> (string code -> 'a rhs) -> 'a case

val compile : 'a case list -> (?start:int -> string -> 'a * int) code
