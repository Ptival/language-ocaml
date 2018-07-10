[![CircleCI](https://circleci.com/gh/Ptival/language-ocaml.svg?style=svg)](https://circleci.com/gh/Ptival/language-ocaml)

# language-ocaml: Language tools for manipulating OCaml programs in Haskell (parser, pretty-printer, ...)

Current target version: OCaml 4.07.0rc2

This is work-in-progress.

`Language.OCaml.Parser` provides:

- `parseImplementationC`, written using parser combinators, i.e. Megaparsec
  (slow?) (IMPORTANT: currently most precedences are wrong, DO NOT USE)

- `parseImplementationG`, written using parser generators, i.e. Alex and Happy
  (fast?), more likely to be correct as it follows the OCaml parser very
  closely, though at the moment locations and documentation might be missing.

Note that the current parser only supports productions I have needed so far, and
is not complete in any way!
