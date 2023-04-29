{ pkgs ? import ./nix
}:

with pkgs.haskell.lib;

let
  ghcidSrc = pkgs.fetchFromGitHub
    { owner = "ndmitchell";
      repo = "ghcid";
      sha256 = "0bsjbb6n7ssg411k2xj4f881v392hvb7xln99bq1r3vkg14mqqsd";
      rev = "e2852979aa644c8fed92d46ab529d2c6c1c62b59";
    };
  hgdbmiSrc = pkgs.fetchFromGitHub
    { owner = "copton";
      repo = "hgdbmi";
      sha256 = "1s6gqd1680sm4xlxy324s6vvdn90hrw17s57zl34ljcck3r5qj6x";
      rev = "faa0881cad2ac3cc3c28009fd589b9bd7866c8dc";
    };

  overrides = self: super: {
    ghcid =
      doJailbreak (self.callCabal2nix "ghcid" ghcidSrc {});
    hgdbmi =
      dontCheck (doJailbreak (self.callCabal2nix "hgdbmi" hgdbmiSrc {}));
    dap =
      self.callCabal2nix "dap" ./. {};
  };

  hPkgs =
    pkgs.haskell.packages.ghc924.override { inherit overrides; };
in
{
  inherit pkgs;
  inherit (hPkgs)
    hgdbmi
    ghcid
    dap;
}
