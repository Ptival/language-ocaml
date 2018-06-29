[![CircleCI](https://circleci.com/gh/Ptival/language-ocaml.svg?style=svg)](https://circleci.com/gh/Ptival/language-ocaml)

# language-ocaml: Language tools for manipulating OCaml programs in Haskell (parser, pretty-printer, ...)

Current target version: OCaml 4.07.0rc2

This is work-in-progress.

`Language.OCaml.Parser` provides:

- `parseImplementationC`, written using parser combinators, i.e. Megaparsec
  (slow?)

- `parseImplementationG`, written using parser generators, i.e. Alex and Happy
  (fast?)

Note that the current parser only supports productions I have needed so far, and
is not complete in any way!
