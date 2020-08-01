let

  name = "language-ocaml";
  compiler-nix-name = "ghc884";

  sources = import ./nix/sources.nix {};
  haskellNix = import (fetchTarball { inherit (sources."haskell.nix") url sha256; }) {
      sourcesOverride = {
        hackageSrc = fetchTarball { inherit (sources."hackage.nix") url sha256; };
      };
  };

  # Ideally you'd want to use haskellNix.sources.nixpkgs down here (to hit their
  # cache).  However, at the moment, haskell-language-server depends on a
  # version of regex-base that is not compatible with what they point to, and so
  # you need to override it which breaks everything (infinite recursion).
  #
  # Good news: fixed by using a more recent nixpkgs
  # Bad news: no caching
  pkgs = import sources.nixpkgs haskellNix.nixpkgsArgs;
  # pkgs = import haskellNix.sources.nixpkgs haskellNix.nixpkgsArgs;

  set = pkgs.haskell-nix.cabalProject' {

    inherit compiler-nix-name;

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name;
      src = ./.;
    };

  };

in

set.hsPkgs.${name}.components.exes // {

  shell = set.hsPkgs.shellFor {

    buildInputs =
      let
        hsPkgs = pkgs.haskell.packages.${compiler-nix-name};
      in [
        # packages that must be installed for development
        hsPkgs.haskell-language-server
      ];

    exactDeps = true;

    packages = p: [
      # packages whose dependencies you need in scope
      p.language-ocaml
    ];

    shellHook = ''
       export HIE_HOOGLE_DATABASE=$(realpath "$(dirname "$(realpath "$(which hoogle)")")/../share/doc/hoogle/default.hoo")
     '';

    tools = {
      alex = "3.2.5";
      cabal = "3.2.0.0";
      happy = "1.19.12";
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
    };

  };

}
