(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

val regex_testcase_gen : (Reex.t * string list * string list) QCheck.arbitrary

val regex_gen : Reex.t QCheck.arbitrary
