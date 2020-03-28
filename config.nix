rec {

  ghcVersion = "ghc882";

  language-ocaml-overlay = self: super:
    let

      dontCheck = super.haskell.lib.dontCheck;

    in
      {
        haskell = super.haskell // {
          inherit ghcVersion;
          packages = super.haskell.packages // {
            "${ghcVersion}" = super.haskell.packages.${ghcVersion}.extend (selfH: superH: {

              # polysemy =
              #   (selfH.callCabal2nix
              #     "polysemy"
              #     (builtins.fetchGit {
              #       url = "https://github.com/polysemy-research/polysemy.git";
              #       rev = "3c731186cb5ebcfc9319586e0b691b985c6730e2"; # 1.3.0
              #     })
              #     {});

              # snap =
              #   (selfH.callCabal2nix
              #     "snap"
              #     (builtins.fetchGit {
              #       url = "https://github.com/snapframework/snap.git";
              #       rev = "a1c876ba0a72e8c5e052f5f0919caee6c4bcd129"; # 1.1.3.0
              #     })
              #     {});


            });
          };
        };
      };

  nixpkgsRev = "c2dcdea8c68631fc15ec837d0df7def2b66d0676";

  pkg = {
    name = "language-ocaml";
    path = ./.;
    args = {};
  };

}
