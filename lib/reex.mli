(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type t = Reex_types.Normal.t = private
           Chars of Reex_types.Charset.t
         | Seq of t list (* r1 r2 ... rn ε (n != 1) *)
         | Alt of Reex_types.NormalSet.t (* r1 | r2 | ... | rn (n >= 2) *)
         | Inter of Reex_types.NormalSet.t (* r1 & r2 & ... & rn (n >= 2) *)
         | Star of t
         | Not of t
module Re_set = Reex_types.NormalSet

val compare : t -> t -> int
val equal : t -> t -> bool
val pp_ast : Format.formatter -> t -> unit
val pp : Format.formatter -> t -> unit

val empty : t
val epsilon : t
val (<&>) : t -> t -> t
val inter : t list -> t
val (>>>) : t -> t -> t
val seq : t list -> t
val (<|>) : t -> t -> t
val alt : t list -> t
val star : t -> t
val plus : t -> t
val opt : t -> t
val not : t -> t
val any : t
val top : t
val chr : char -> t
val str : string -> t
val range : char -> char -> t

exception Parse_error of string
val regex : string -> t
