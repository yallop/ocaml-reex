(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Reex_types

exception Parse_error of string

exception Fail

module Bracket =
struct
  (** Follows the POSIX spec
        9.3.5 RE Bracket Expression
         http://pubs.opengroup.org/onlinepubs/009696899/basedefs/xbd_chap09.html#tag_09_03_05
      but there is currently no support for character classes.

      Bracket expressions are delimited by [ and ], with an optional "complement"
      operator ^ immediately after the [.  Special characters:
         ^ (immediately after the opening [)
         ] (except immediately after the opening [ or [^)
         - (except at the beginning or end)                                   *)

  type element = Char of char | Range of char * char
  type t = { negated: bool; elements: element list }

  let interpret { negated; elements } =
    let s =
      List.fold_right
        (function Char c -> Charset.add c
                | Range (c1,c2) -> 
                   Charset.union (Charset.range c1 c2))
        elements Charset.empty in
    if negated then Charset.diff Charset.all s
    else s

  let parse_element = function
    | [] -> raise Fail
    | ']' :: s -> (None, s)
    | c :: ('-' :: ']' :: _ as s) -> (Some (Char c), s)
    | c1 :: '-' :: c2 :: s -> (Some (Range (c1, c2)), s)
    | c :: s -> (Some (Char c), s)

  let parse_initial = function
    | [] -> raise Fail
    | c :: ('-' :: ']' :: _ as s) -> (Some (Char c), s)
    | c1 :: '-' :: c2 :: s -> (Some (Range (c1, c2)), s)
    | c :: s -> (Some (Char c), s)

  let parse_elements s = 
    let rec loop elements s =
      match parse_element s with
      | None, s -> (List.rev elements, s)
      | Some e, s -> loop (e :: elements) s in
    match parse_initial s with
    | None, s -> ([], s)
    | Some e, s -> loop [e] s

end

type t =
  | Opt : t -> t
  | Chr : char -> t 
  | Alt : t * t -> t
  | Int : t * t -> t
  | Seq : t * t -> t
  | Star : t -> t
  | Plus : t -> t
  | Bracketed : Bracket.t -> t
  | Any : t
  | Eps : t

let rec interpret : t -> Normal.t = function
  | Opt t -> interpret t <|> epsilon
  | Chr c -> chr c
  | Alt (l, r) -> interpret l <|> interpret r
  | Int (l, r) -> interpret l <&> interpret r
  | Seq (l, r) -> interpret l >>> interpret r
  | Star t -> star (interpret t)
  | Plus t -> let it = interpret t in it >>> star it
  | Bracketed elements -> chars (Bracket.interpret elements)
  | Any -> any
  | Eps -> epsilon

 (* We've seen [.  Special characters:
      ^   (beginning of negation)  *)
let re_parse_bracketed : char list -> (t * char list) = function
  | '^' :: s -> let elements, rest = Bracket.parse_elements s in
                (Bracketed { negated = true; elements }, rest)
  | _ :: _ as s -> let elements, rest = Bracket.parse_elements s in
              (Bracketed { negated = false; elements }, rest)
  | [] -> raise Fail

let digit d = Char.code d - Char.code '0'

(** ratom ::= .
              <character>
              [ bracket ]
              ( ralt )           *)
let rec re_parse_atom : char list -> (t * char list) option = function
  | '('::rest ->
     begin match re_parse_alt rest with
     | (r, ')' :: rest) -> Some (r, rest)
     | _ -> raise Fail
     end
  | '\\'::('.'|'\\'|'|'|'+'|'*'|'?'|'&'|'('|')'|'['as c)::rest -> Some (Chr c, rest)
  | '\\'::('b')::rest -> Some (Chr '\b', rest)
  | '\\'::('t')::rest -> Some (Chr '\t', rest)
  | '\\'::('n')::rest -> Some (Chr '\n', rest)
  | '\\'::('r')::rest -> Some (Chr '\r', rest)
  | '\\'::('0'..'9' as h)::('0'..'9' as t)::('0'..'9' as d) ::rest ->
     let h = digit h and t = digit t and d = digit d in
     let code = 100 * h + 10 * t + d in
     if code > 255 then Printf.ksprintf failwith "invalid code: %d\n" code
     else Some (Chr (Char.chr code), rest)
  | '['::rest -> Some (re_parse_bracketed rest)
  | [] | ((')'|'|'|'&'|'*'|'?'|'+') :: _) -> None
  | '.' :: rest -> Some (Any, rest)
  | h :: rest -> Some (Chr h, rest)

(** rsuffixed ::= ratom
                  atom *  
                  atom +
                  atom ?         *)
and re_parse_suffixed : char list -> (t * char list) option =
  fun s -> match re_parse_atom s with
  | None -> None
  | Some (r, '*' :: rest) -> Some (Star r, rest)
  | Some (r, '+' :: rest) -> Some (Plus r, rest)
  | Some (r, '?' :: rest) -> Some (Opt r, rest)
  | Some (r, rest) -> Some (r, rest)

(** rseq ::= <empty>
             rsuffixed rseq      *)
and re_parse_seq : char list -> t * char list = fun (s: char list) ->
  match re_parse_suffixed s with
  | None -> (Eps, s)
  | Some (r, rest) -> let r', s' = re_parse_seq rest in (Seq (r, r'), s')

(** rseq ::= rseq
             rseq | rint         *)
and re_parse_int (s: char list) =
  match re_parse_seq s with
  | (r, '&' :: rest) -> let r', s' = re_parse_alt rest in (Int (r, r'), s')
  | (r, rest) -> (r, rest)

(** ralt ::= rint
             rint | ralt         *)
and re_parse_alt (s: char list) =
  match re_parse_int s with
  | (r, '|' :: rest) -> let r', s' = re_parse_alt rest in (Alt (r, r'), s')
  | (r, rest) -> (r, rest)

let explode s = List.init (String.length s) (String.get s)

let parse s =
  match re_parse_alt (explode s) with
  | (r, []) -> r
  | exception Fail -> raise (Parse_error s)
  | (_, _::_) -> raise (Parse_error s)

let regex s = interpret (parse s)
