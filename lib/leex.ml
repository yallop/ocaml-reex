(*
 * Copyright (c) 2022 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type 'f rhs = Skip | Error of string | Return of 'f code
type 'a case = Case of Reex.t * (string code -> 'a rhs)

let (=>) lhs rhs = Case (lhs, rhs)

let compile cases =
  .< let rec lex ?(start=0) s =
     .~Reex.(Reex_match.match_ .<start>. .<s>.
          (List.rev @@ fst @@ List.fold_left
            (fun (cases, seen) (Case (lhs, rhs)) ->
              let lhs' = lhs <&> not seen in
              let rhs' _ i = match rhs .<String.sub s start (.~i - start) >. with
                | Skip     -> .< lex ~start:.~i s >.
                | Error s  -> .< failwith s >.
                | Return v -> .< (.~v, .~i) >. in
              ((lhs', rhs') :: cases, seen <|> lhs'))
             ([], empty)
             cases))
     in lex >.
