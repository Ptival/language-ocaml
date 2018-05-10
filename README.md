[![CircleCI](https://circleci.com/gh/Ptival/language-ocaml.svg?style=svg)](https://circleci.com/gh/Ptival/language-ocaml)

# language-ocaml: Language tools for manipulating OCaml programs in Haskell (parser, pretty-printer, ...)

This is work-in-progress.  For now, we only provide `Language.OCaml.Parser`,
which itself only provides `implementation_P`, a parser for `.ml` files.

Note that the current parser only supports productions I have needed so far, and
is not complete in any way!

Finally, it is built using parser combinators, so do not expect extreme
performance.
