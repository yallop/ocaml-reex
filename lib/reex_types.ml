(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Charset =
struct
  include Charset
  let pp fmt cs =
    if cardinal cs = 256 then Format.fprintf fmt "." else begin
        Format.fprintf fmt "{";
        iter (fun c -> Format.fprintf fmt "%c," c) cs;
        Format.fprintf fmt "}";
      end

  let range l h = of_list (List.init (succ (Char.code h - Char.code l))
                             (fun i -> Char.chr (Char.code l + i)))
  let all = range (Char.chr 0) (Char.chr 255)
end

module rec Normal : sig             
  type t = Chars of Charset.t
         | Seq of t list (* r1 r2 ... rn ε (n != 1) *)
         | Alt of NormalSet.t (* r1 | r2 | ... | rn (n >= 2) *)
         | Inter of NormalSet.t (* r1 & r2 & ... & rn (n >= 2) *)
         | Star of t
         | Not of t [@@deriving ord, eq, show]
end = struct
  type t = Chars of Charset.t
         | Seq of t list (* r1 r2 ... rn ε (n != 1) *)
         | Alt of NormalSet.t (* r1 | r2 | ... | rn (n >= 2) *)
         | Inter of NormalSet.t (* r1 & r2 & ... & rn (n >= 2) *)
         | Star of t
         | Not of t [@@deriving ord, eq, show]
end and NormalSet : sig include Set.S with type elt = Normal.t val pp : Format.formatter -> t -> unit end =
struct include Set.Make(Normal)
  let pp fmt cs =
    begin
      Format.fprintf fmt "{";
      iter (fun c -> Format.fprintf fmt "%a," Normal.pp c) cs;
      Format.fprintf fmt "}";
    end
end
include Normal

let empty = Chars Charset.empty
let epsilon = Seq []
let any = (Chars Charset.all)
let top = Star any

let chr c = Chars (Charset.singleton c)

let inter' s = (* (invariant: Inter cardinality >= 2) *)
  match NormalSet.cardinal s with
  | 0 -> top
  | 1 -> NormalSet.choose s
  | _ -> Inter s

let (<&>) l r = match l, r with
  (* TODO: should we distribute over sums? *)
  (* TODO: should we look at the inter of the first sets? *)
  | Chars s, _ when Charset.is_empty s -> empty
  | _, Chars s when Charset.is_empty s -> empty
  | Chars x, Chars y -> Chars (Charset.inter x y)
  | Inter rs1, Inter rs2 -> inter' (NormalSet.union rs1 rs2)
  | Inter rs, r | r, Inter rs -> inter' (NormalSet.add r rs)
  | Not (Chars s), r when Charset.is_empty s -> r
  | r, Not (Chars s) when Charset.is_empty s -> r
  | r, s -> inter' (NormalSet.of_list [r; s])

let seq' = function (* (invariant: Seq cardinality != 1) *)
  | [r] -> r
  | rs -> Seq rs

let (>>>) l r = match l, r with
  | Chars s, _ when Charset.is_empty s -> empty
  | _, Chars s when Charset.is_empty s -> empty
  | Seq l , Seq r -> seq' (l @ r)
  | l     , Seq r -> seq' (l :: r)
  | Seq l , r     -> seq' (l @ [r])
  | l     , r     -> seq' [l; r]

let alt' s = (* (invariant: Alt cardinality >= 2) *)
  match NormalSet.cardinal s with
  | 0 -> empty
  | 1 -> NormalSet.choose s
  | _ -> Alt s

let (<|>) l r =
  match l, r with
  | Chars x, Chars y -> Chars (Charset.union x y)
  | (Chars s, r  (* Can we do better?  What about checking that s is any empty regex? *)
     | r, Chars s) when Charset.is_empty s -> r
  | Not (Chars s), _ when Charset.is_empty s -> Not empty
  | _, Not (Chars s) when Charset.is_empty s -> Not empty
  | Alt r  , Alt s -> alt' (NormalSet.union r s)
  | r      , Alt rs
  | Alt rs , r     -> alt' (NormalSet.add r rs)
  | r      , s     -> alt' (NormalSet.of_list [r; s])

let star = function
  | Star r -> Star r
  | Seq [] -> epsilon
  | Chars s when Charset.is_empty s -> epsilon
  | r -> Star r

let not = function
  | Not r -> r
  | r -> Not r

let chars cs = Chars cs

