let
  externalStgDapPkgs = (import ./default.nix {});
in
with externalStgDapPkgs;

dap.env.overrideAttrs (drv: {
  shellHook = ''
     export PATH=$PATH:${pkgs.haskell.packages.ghc924.stack}/bin
     function ghcid () {
        ${ghcid}/bin/ghcid --poll --allow-eval -c 'cabal repl'
     }
   '';
 })
