with (import ./default.nix {});

dap.env.overrideAttrs (drv: {
  shellHook = ''
    export PATH=$PATH:${pkgs.cabal-install}/bin
  '';
})
