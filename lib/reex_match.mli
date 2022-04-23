
(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Reex

type options =
  { match_type: [`table | `ranges];
    (** Whether to generate a table lookup for each character match *)

    null_optimization: bool;
    (** Whether to optimize the generated code using OCaml's
        null-terminated string representation *) }

val default_options : options

val match_ : ?options:options ->
             int code -> string code -> ?otherwise:'a code -> (t * (int code -> index:int code -> len:int code -> string code -> 'a code)) list -> 'a code
(** [match_ i s [(r1, f1); (r2, f2); ...; (rn, fn)] generates code that
     matches [s] against [r1] ... [rn] starting from index [i] and calls
     the function [fi] corresponding to the first regular expression [ri]
     that matches the longest prefix of [s], passing [fi] the start and end
     indexes of the matched substring. *)
    
val matchk_ : ?options:options ->
              ?otherwise:'a code -> ((t * ('b code -> index:int code -> len:int code -> string code -> 'a code)) list ->
             ('b -> index:int -> len:int -> string -> 'a) code as 'k) -> 'k
(** [matchk_] is a variant of [match_] in continuation passing style *)
