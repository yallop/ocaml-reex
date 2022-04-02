(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Reex_types

module X =
struct

  let regex_specials = "*+?.|()["

  type result = { hyphen: bool;
                  caret: bool;
                  lbracket: bool;
                  ranges: (char * char) list; }
  let ranges (set : Charset.t) =
    let adjacent c1 c2 = abs (Char.code c1 - Char.code c2) = 1 in
    match
      Charset.fold
        (fun c (co, r) ->
          match c, co with
          | '-', co -> (co, {r with hyphen = true})
          | '^', co -> (co, {r with caret = true})
          | ']', co -> (co, {r with lbracket = true})
          | c, None -> (Some (c,c), r) 
          | c, Some (c1,c2) when adjacent c c2 -> (Some (c1, c), r)
          | c, Some (c1,c2) -> (Some (c,c),
                                { r with ranges = (c1,c2) :: r.ranges }))
        set
        (None, {hyphen = false; caret = false; lbracket = false; ranges = []})
    with
    | None  , r -> {r with ranges = List.rev r.ranges}
    | Some p, r -> {r with ranges = List.rev (p :: r.ranges)}

  let unparse ?(complement=false) (set : Charset.t) =
    let pr = Printf.sprintf in
    let r = ranges set in
    let conc =
      List.fold_left
        (fun s (x,y) -> if x = y then pr "%c%s" x s else pr "%c-%c%s" x y s)
        "" in
    let whenever p s = if p then s else "" in
    let bracket s = if complement then pr "[^%s]" s else pr "[%s]" s in
    match r.ranges, r.lbracket, r.caret, r.hyphen with
    | [(x,y)], false, false, false
        (* If we have a single non-special character then
           there's no need for a range expression *)
         when x = y
           && Stdlib.not complement
           && Stdlib.not (String.contains regex_specials x) -> pr "%c" x
    | _ :: _ as rs, lbracket, caret, hyphen ->
       (* If we have some non-special characters then we don't need to
          take extra care to avoid accidentally positioning special
          characters like ^ the beginning or end *)
       bracket @@
       pr "%s%s%s%s"
         (whenever lbracket "]")
         (conc rs)
         (whenever caret "^")
         (whenever hyphen "-")
    | [], true, _, _ ->
       bracket (pr "]%s%s" (whenever r.caret "^") (whenever r.hyphen "-"))
    | [], false, true , true  -> bracket "-^"
    | [], false, true , false -> if complement then "[^^]" else "^"
    | [], false, false, true  -> bracket "-"
    | [], false, false, false -> if complement then pr "[\000-\255]" else pr "[^\000-\255]"
end

let pr = Format.fprintf

type prec = Seq    (* parenthesize: Alt, Inter, Not *)
          | Binary (* parenthesize: Alt, Inter *)
          | Unary  (* parenthesize: Star, Not, Alt, Inter, Seq *)
          | None   (* parenthesize: *)

let char : Format.formatter -> char -> unit =
  fun fmt -> function
  | '\b' -> pr fmt "\\b"
  | '\t' -> pr fmt "\\t"
  | '\n' -> pr fmt "\\n"
  | '\r' -> pr fmt "\\r"
  | '\000'..'\007'|'\011'|'\012'|'\014'..'\031'|'\127' as c -> pr fmt "\\%.3d" (Char.code c)
  | '.'| '*'| '?'| '+'| '&'| '('| ')'| '|'|'['|'\\' as c -> pr fmt "\\%c" c
  | '\128'..'\255' as c -> pr fmt "%c" c
  | ' '..'Z'|'^'..'~'|']' as c -> pr fmt "%c" c

let range : Format.formatter -> Charset.t -> unit =
  fun fmt cs ->
  match Charset.cardinal cs with
  | 0 -> pr fmt "(a&b)"
  | 1 -> char fmt (Charset.choose cs)
  | 256 -> pr fmt "."
  | _ -> pr fmt "%s" (X.unparse cs)

let lpr fmt = pr fmt "(" and rpr fmt = pr fmt ")"

let rec printl prec sep unit fmt = function
  | [] -> pr fmt unit
  | [x] -> print prec fmt x
  | x :: xs -> pr fmt "%a%s%a" (print prec) x sep (printl prec sep unit) xs

and print prec fmt (r : Normal.t) =
  match prec, r with
  | _                  , Chars cs -> range fmt cs
  | Unary              , Seq rs   -> lpr fmt; List.iter (print Seq fmt) rs; rpr fmt
  | _                  , Seq rs   ->          List.iter (print Seq fmt) rs
  | (Seq|Binary|Unary) , Alt rs   -> lpr fmt; printl Binary "|" "(a&b)" fmt (NormalSet.elements rs); rpr fmt
  | None               , Alt rs   ->          printl Binary "|" "(a&b)" fmt (NormalSet.elements rs)
  | (Seq|Binary|Unary) , Inter rs -> lpr fmt; printl Binary "&" ".*" fmt (NormalSet.elements rs); rpr fmt
  | None               , Inter rs ->          printl Binary "&" ".*" fmt (NormalSet.elements rs)
  | Unary              , Star r   -> lpr fmt; pr fmt "%a*" (print Unary) r; rpr fmt
  | _                  , Star r   ->          pr fmt "%a*" (print Unary) r
  | (Seq|Unary)        , Not r    -> lpr fmt; pr fmt "¬%a" (print Unary) r; rpr fmt
  | _                  , Not r    ->          pr fmt "¬%a" (print Unary) r

let pp = print None
