A staged regular expression library for MetaOCaml, based on ["Regular-expression derivatives reexamined"][reex-paper]

## Installing `reex`

1. Install the [BER MetaOCaml][ber-metaocaml] compiler using [OPAM][opam]:

   ```
   opam switch 4.14.1+BER
   eval $(opam env)
   ```

2. Add the [metaocaml-opam][metaocaml-opam] repository:

   ```
   opam remote add metaocaml git+https://github.com/metaocaml/metaocaml-opam.git
   ```

3. Install the `reex` and `reex_match` packages:

   ```
   opam pin add reex reex_match
   ```

## Using `reex`

The functions in the [`Reex`][reex] module construct regular expressions:

```ocaml
let letters = plus (range 'A' 'Z')   (* [A-Z]+ *)
let keyword = str "let" <|> str "and" <|> str "in"
```

Alternatively, the `Reex.regex` function builds a regular expression from a string:

```ocaml
let letters = regex "[A-Z]+"
let keyword = regex "let|and|in"
```

The [`Reex_match.match_`][reex_match] function generates OCaml code that matches one or more regular expressions.  For example, the following call

```ocaml
.<fun i s ->
   .~(match_ ~options:{default_options with match_type=`ranges} .<i>. .<s>.
        [chr 'a'       , (fun _ _ -> .<"a">.);
         plus (chr 'b'), (fun _ _ -> .<"b">.)]) >.
```

generates the following OCaml code for matching "a" or "b+":

```ocaml
fun i s ->
  let rec f ~start ~index ~len s =
    match String.unsafe_get s index with
    | 'c'..'\255'|'\000'..'`' -> failwith "no match"
    | 'b' -> g ~start ~index:(index + 1) ~len s
    | 'a' -> h ~start ~index:(index + 1) ~len s
  and g ~start ~index ~len s =
    match String.unsafe_get s index with
    | 'c'..'\255'|'\000'..'a' -> "b"
    | 'b' -> g ~start ~index:(index + 1) ~len s
  and h ~start ~index ~len s = "a" in
  f ~start:i ~index:i ~len:(String.length s) s
```

[reex-paper]: https://www.ccs.neu.edu/home/turon/re-deriv.pdf
[metaocaml-opam]: https://github.com/metaocaml/metaocaml-opam
[ber-metaocaml]: https://okmij.org/ftp/ML/MetaOCaml.html
[opam]: https://opam.ocaml.org/
[reex]: https://github.com/yallop/reex/blob/master/lib/reex.mli
[reex_match]: https://github.com/yallop/reex/blob/master/lib/reex_match.mli
