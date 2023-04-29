let
  externalStgDapPkgs = (import ./default.nix {});
in
with externalStgDapPkgs;

dap.env.overrideAttrs (drv: {
  shellHook = ''
     export PATH=$PATH:${pkgs.gdb}/bin
     function ghcid () {
        ${ghcid}/bin/ghcid --poll --allow-eval -c 'cabal repl'
     }
     function compile () {
        echo "compiling cbits/main.c with debug info (-g) to main"
        gcc -Wall -Werror -ansi -pedantic-errors -g cbits/main.c -o a.out
     }
   '';
 })
