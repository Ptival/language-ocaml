{ nixpkgs ? import <nixpkgs> {}
}:
with nixpkgs;
let
  language-ocaml = haskellPackages.callCabal2nix "language-ocaml" ./. {};
in
mkShell {
  buildInputs = [
    cabal-install
  ];
  inputsFrom = [
    language-ocaml.env
  ];
  name = "language-ocaml";
}
