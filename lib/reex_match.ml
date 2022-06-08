(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Reex_deriv
module Cm = Charmatch.Make(Charset)
module CharsetSet = Set.Make(Charset)

type options = { match_type: [`table | `ranges]; null_optimization: bool; }
let default_options = { match_type = `table; null_optimization = true }

let all_empty l = List.for_all (fun (x,_) -> Reex.(equal x empty)) l
let deriv_all c l = List.map (fun (t,rhs) -> (deriv c t, rhs)) l
let charsetset_filtermap : (Charset.t -> 'a option) -> CharsetSet.t -> 'a list =
  fun f css -> List.filter_map f (CharsetSet.elements css)

let ifmem options e cases =
  Cm.ifmem ~options:{ match_type = options.match_type } e cases

let char_match_index options
    : string code -> int code -> int code -> (Charset.t * 'a code) list -> eof:'a code -> 'a code =
  fun s i len cases ~eof ->
  .< if .~i = .~len then .~eof
     else .~(ifmem options .<String.unsafe_get .~s .~i>. cases) >.

let char_match_index' options
    : string code -> int code -> int code -> (Charset.t * 'a code) list -> eof:'a code -> 'a code =
  fun s i len cases ~eof ->
  (* As char_match_index, but with an optimization.  Instead of testing for end-of-string initially

         if i = n then .~eof else
         match s.[i] with
         | ...

     incorporate the end-of-string test into the match:

         match s.[i] with
         | '\000' -> if .~i = .~len then .~eof else ...
         | ...
   
     This works because OCaml represents strings with a terminating nil for C interoperability. *)
  let cases' =
    List.fold_left
      (fun cases' (lhs, rhs) ->
        if Charset.mem '\000' lhs then 
          let lhs' = Charset.remove '\000' lhs in
          if rhs == eof then (lhs, rhs) :: cases'
          else if Charset.is_empty lhs' then ((lhs, .< if .~i = .~len then .~eof else .~rhs>.) :: cases')
          else ((Charset.singleton '\000', .< if .~i = .~len then .~eof else .~rhs>.) :: (lhs', rhs) :: cases')
        else (lhs, rhs) :: cases') [] cases in
  match cases' with (* Also skip the match altogether if there's just one exhaustive case *) 
  | [cs, rhs] when Charset.cardinal cs = 256 -> rhs
  | cases' -> ifmem options .<String.unsafe_get .~s .~i>. cases'

let char_match options s i len cases ~eof =
  (if options.null_optimization then char_match_index' else char_match_index)
    options s i len cases ~eof

let matchk_ ?(options=default_options) self (indexes, fallback) =
  .< fun start ~index:i ~prev ~len s -> 
    .~(let eof = match List.find (fun (x,_) -> nullable x) indexes with
         | exception Not_found -> (fallback .<start>. ~index:.<prev>. ~len:.<len>. .<s>.)
         | (_,rhs) -> (rhs .<start>. ~index:.<i>. ~len:.<len>. .<s>.) in
       char_match options .<s>. .<i>. .<len>. ~eof @@ charsetset_filtermap
           (fun ss -> match Charset.choose_opt ss with
                      | None -> None
                      | Some c'' -> let indexes' = deriv_all c'' indexes in
                                    let prev, fallback = (* Do we have a new match? *)
                                      match List.find (fun (x,_) -> nullable x) indexes' with
                                      | exception Not_found -> .<prev>., fallback
                                      | (_, rhs) -> .< i+1 >., rhs
                                    in
                                    Some (ss, if all_empty indexes' then eof
                                              else .< .~(self (indexes', fallback)) start ~index:(i + 1) ~prev:.~prev ~len s >.))
           (capproxes (List.map fst indexes))) >.

let match_ ?options i x  ?(otherwise = .<failwith "no match" >.) cases =
  let fallback _ ~index:_ ~len:_ _ = otherwise in
  let equal (xs, fx) (ys, fy) = List.for_all2 (fun (x,_) (y,_) -> Reex.equal x y) xs ys && fx == fy in
  Letrec.letrec ~equal (matchk_ ?options)
    (fun self -> .< .~(self (cases, fallback)) .~i ~index:.~i ~prev:0 ~len:(String.length .~x) .~x >.)
