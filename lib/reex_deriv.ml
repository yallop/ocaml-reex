(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Reex

module CharsetSet = Set.Make(Charset)

let rec nullable : Reex.t -> bool = function
  | Chars _ -> false
  | Seq rs -> List.for_all nullable rs
  | Alt rs -> Re_set.exists nullable rs
  | Inter rs -> Re_set.for_all nullable rs
  | Star _ -> true
  | Not r -> Stdlib.not (nullable r)

let cssprodWithInter (rs : CharsetSet.t) (ss : CharsetSet.t) : CharsetSet.t =
  CharsetSet.fold
    (fun r ->
      CharsetSet.union
        (CharsetSet.fold (fun s -> CharsetSet.add (Charset.inter r s)) ss CharsetSet.empty))
    rs
    CharsetSet.empty

(* TODO: memoize *)
let rec deriv (a : char) : Reex.t -> Reex.t = function
  | Reex.Chars cs when Charset.mem a cs -> epsilon
  | Chars _ -> empty
  | Seq [] (* ε *) -> empty 
  | Seq [r] -> deriv a r
  | Seq (r :: (_ :: _ as rs)) -> 
     (deriv a r >>> seq rs) <|>
       ((if nullable r then epsilon else empty) >>> deriv a (seq rs))
  | Alt rs -> Re_set.fold (fun r d -> deriv a r <|> d) rs empty
  | Inter rs -> Re_set.fold (fun r d -> deriv a r <&> d) rs (deriv a top)
  | Star r -> deriv a r >>> star r
  | Not r -> not (deriv a r)

let all = Charset.of_list (List.init 256 Char.chr)

(** approximation of the derivative classes (pp10-11) *)
let rec capprox : t -> CharsetSet.t = function
  | Seq [] (* ε *) -> CharsetSet.singleton all
  | Chars s -> CharsetSet.of_list [s; Charset.diff all s]
  | Seq [r] -> capprox r
  | Seq (r :: _) when Stdlib.not (nullable r) -> capprox r
  | Seq (r :: s) -> cand r (Reex.seq s)
  | Alt rs -> cands rs
  | Inter rs -> cands rs
  | Star r -> capprox r
  | Not r -> capprox r
and cand r s = cssprodWithInter (capprox r) (capprox s)
and cands (rs : Re_set.t) : CharsetSet.t =
  Re_set.fold (fun r -> cssprodWithInter (capprox r)) rs (capprox top)

let capproxes : Reex.t list -> CharsetSet.t = function
  | [] -> CharsetSet.empty
  | r :: rs -> List.fold_left (fun css y -> cssprodWithInter (capprox y) css) (capprox r) rs
