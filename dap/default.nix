{ pkgs ? import ./nix
}:

with pkgs.haskell.lib;

let
  cabal-src = pkgs.fetchFromGitHub
    { owner = "haskell";
      repo = "cabal";
      sha256 = "13x3dr257ivalhgiffjnyazffimn4a817dj3p96vvi50nx67cml2";
      rev = "3af1731c01c35614fd902ee5c1aec40f5587fde6";
    };
  ghcidSrc = pkgs.fetchFromGitHub
    { owner = "ndmitchell";
      repo = "ghcid";
      sha256 = "0bsjbb6n7ssg411k2xj4f881v392hvb7xln99bq1r3vkg14mqqsd";
      rev = "e2852979aa644c8fed92d46ab529d2c6c1c62b59";
    };
  zip-cmd-src = pkgs.fetchFromGitHub
    { owner = "grin-compiler";
      repo = "zip-cmd";
      sha256 = "1gsdcip4qrd8bbxira7v9yz1b05c0y7jbbd440hwdh5z6y94ah9g";
      rev = "97a6a768803958faee855de115c0402f29bad32b";
    };
  external-stg-src = pkgs.fetchFromGitHub
    { owner = "grin-compiler";
      repo = "ghc-whole-program-compiler-project";
      sha256 = "0a9qxm29cn0vy5v0d399944j0155ck8pqqk58w1y350g70anych0";
      rev = "9d7a96a0b831f980d8c9d5a30a9185b64fbbfa31";
    };
  souffle-haskell-src = pkgs.fetchFromGitHub
    { owner = "luc-tielen";
      repo = "souffle-haskell";
      sha256 = "sha256-/BdDkSTlxh3m3ApxqdbDQ1yIGiE6mTfDljfpEYgE5Tg=";
      rev = "f8c9fc45eed709110af3d3301393f63f4535c71e";
    };
  type-errors-pretty-src = pkgs.fetchFromGitHub
    { owner = "kowainik";
      repo = "type-errors-pretty";
      sha256 = "1yylw5c8ffzybcv7cm6ff0k88an4iz0fhc59md09s9zlns03f3d0";
      rev = "c85d6d0a7bf2278ddb03abddb5782a5b6095d343";
    };
  ghc-wpc-src = builtins.fetchGit
    { url = "https://github.com/grin-compiler/ghc-wpc.git";
      ref = "ghc-whole-program-compiler";
      submodules = true;
    };
  ghc-patch = pkgs.fetchpatch {
    url = "https://gitlab.haskell.org/ghc/ghc/-/commit/ad2ef3a13f1eb000eab8e3d64592373b91a52806.patch";
    sha256 = "sha256-Dm9nOcS20wiA5Op4vF9Y8YqcgSSC3IKRlYusBukzf8Y=";
  };

  overrides961 = self: super: {
    Cabal = self.callCabal2nix "Cabal" "${cabal-src}/Cabal" {};
    Cabal-syntax = self.callCabal2nix "Cabal-syntax" "${cabal-src}/Cabal-syntax" {};
    Cabal-tests =  (self.callCabal2nix "Cabal-tests" "${cabal-src}/Cabal-tests" {});
    cabal-install-solver =  (self.callCabal2nix "cabal-install-solver" "${cabal-src}/cabal-install-solver" {});
    cabal-install =  (self.callCabal2nix "cabal-install" "${cabal-src}/cabal-install" {});
    Cabal-described =  (self.callCabal2nix "Cabal-described" "${cabal-src}/Cabal-described" {});
    Cabal-QuickCheck =  (self.callCabal2nix "Cabal-QuickCheck" "${cabal-src}/Cabal-QuickCheck" {});
    Cabal-tree-diff =  (self.callCabal2nix "Cabal-tree-diff" "${cabal-src}/Cabal-tree-diff" {});
    zip-cmd = doJailbreak (self.callCabal2nix "zip-cmd" zip-cmd-src {});
    zip = dontCheck (doJailbreak (super.zip));
    wpc-plugin = self.callCabal2nix "wpc-plugin" "${external-stg-src}/wpc-plugin" {};
    external-stg = self.callCabal2nix "external-stg" "${external-stg-src}/external-stg" {};
    external-stg-syntax = self.callCabal2nix "external-stg-syntax" "${external-stg-src}/external-stg-syntax" {};
    hello = with self;
      pkgs.lib.overrideDerivation (addBuildDepends (callCabal2nix "hello" ./hello {}) [zip-cmd external-stg]) (drv: {
        postInstall = ''
          ${external-stg}/bin/mkfullpak -a dist/build/hello/hello.o_ghc_stgapp
          mv -v dist/build/hello/hello.fullpak $out/bin/hello.fullpak
        '';
      });
  };

  overrides924 = self: super: {
    type-errors-pretty =
      dontCheck (doJailbreak (self.callCabal2nix "type-errors-pretty" type-errors-pretty-src {}));
    external-stg =
      self.callCabal2nix "external-stg" "${external-stg-src}/external-stg" {};
    external-stg-interpreter = with pkgs.haskell.lib;
      self.callCabal2nix "external-stg-interpreter" "${external-stg-src}/external-stg-interpreter"
        (pkgs.lib.optionalAttrs (pkgs.stdenv.isDarwin) { omp = pkgs.llvmPackages.openmp; });
    external-stg-syntax =
      self.callCabal2nix "external-stg-syntax" "${external-stg-src}/external-stg-syntax" {};
    souffle-haskell = with pkgs;
      overrideCabal
        (addBuildTool (self.callCabal2nix "souffle-haskell" souffle-haskell-src { }) souffle)
         (o: {
          doCheck = true;
          checkPhase = ''
            runHook preCheck
            DATALOG_DIR="${o.src}/tests/fixtures/" SOUFFLE_BIN="${souffle}/bin/souffle" ./Setup test
            runHook postCheck
          '';
        });

    ghcid =
      doJailbreak (self.callCabal2nix "ghcid" ghcidSrc {});
    dap =
      self.callCabal2nix "dap" ./. {};
  };

  hPkgs924 = pkgs.haskell.packages.ghc924.override { overrides = overrides924; };
  hPkgs961 = pkgs.haskell.packages.ghc961.override { overrides = overrides961; };

in
{
  inherit pkgs;
  inherit (hPkgs961)
    hello
    zip-cmd
    cabal-install
    wpc-plugin;
  inherit (hPkgs924)
    external-stg
    external-stg-interpreter
    external-stg-syntax
    souffle-haskell
    ghcid
    dap;
}
