{ nur ? (import ~/personal/nur-packages {})
}:
let
  config = import ./config.nix;
in
nur.lib.stackShell {

  inherit (config) nixpkgsRev pkg;

  nixpkgsArgs = {
    overlays = [
      (nur.overlays.haskell-dev { inherit (config) ghcVersion; })
      config.language-ocaml-overlay
    ];
  };

}
