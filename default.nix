# { nur ? (import <nixpkgs> {}).nur.repos.ptival
{ nur     ? import ~/personal/nur-packages {}
, nixpkgs ? import <nixpkgs> {}
}:
let
  config = import ./config.nix;
in
nixpkgs.haskell.lib.doCheck (nur.lib.callCabal2nixGitignore nixpkgs "language-ocaml" ./. {})
