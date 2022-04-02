(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Charset :
sig
  include Set.S with type elt = char and type t = Charset.t
  val pp : Format.formatter -> t -> unit
  val range : char -> char -> t
  val all : t
end

module rec Normal : sig             
  type t = private
           Chars of Charset.t
         | Seq of t list (* r1 r2 ... rn ε (n != 1) *)
         | Alt of NormalSet.t (* r1 | r2 | ... | rn (n >= 2) *)
         | Inter of NormalSet.t (* r1 & r2 & ... & rn (n >= 2) *)
         | Star of t
         | Not of t [@@deriving ord, eq, show]
end and NormalSet : Set.S with type elt = Normal.t 

include module type of Normal with type t = Normal.t

val epsilon : t
val any : t
val top : t
val empty : t
val chr : char -> t
val (<|>) : t -> t -> t
val (<&>) : t -> t -> t
val (>>>) : t -> t -> t
val star : t -> t
val not : t -> t
val chars : Charset.t -> t

