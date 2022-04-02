(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Reex

val nullable : t -> bool
val deriv : char -> t -> t
val capprox : t -> Set.Make(Charset).t
val capproxes : t list -> Set.Make(Charset).t
