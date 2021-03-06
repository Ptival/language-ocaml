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
  # pkgs = import haskellNix.sources.nixpkgs-2003 haskellNix.nixpkgsArgs;

  project = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name;
      src = ./.;
    };

  };

in

project.${name}.components.library // {

  shell = project.shellFor {

    buildInputs =
      let
        hsPkgs = pkgs.haskell.packages.${compiler-nix-name};
      in [
        hsPkgs.haskell-language-server
      ];

    packages = p: [
      p.language-ocaml
    ];

    tools = {
      cabal = "3.2.0.0";
      hlint = "2.2.11";
      hpack = "0.34.2";
      ormolu = "0.1.2.0";
    };

  };

}
