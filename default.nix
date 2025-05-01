with (builtins.fromJSON (builtins.readFile ./nixpkgs.json));

{ nixpkgs ? builtins.fetchTarball {
     url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }
}:

let
  # all the nix things
  pkgs = import nixpkgs {};
  sources = builtins.fromJSON (builtins.readFile ./source.json);

  ghc-wpo-src =
    pkgs.fetchFromGitHub sources.ghc-whole-program-compiler-project;

  # this is the reachability analysis binary (cretaed by souffle) that runs
  # the mark n' sweep GC pass. This is call by the external-stg-intepreter.
  ext-stg-gc =
    pkgs.stdenv.mkDerivation {
      name = "ext-stg-gc";
      src = "${ghc-wpo-src}/external-stg-interpreter";
      buildInputs = with pkgs; [ souffle openmpi ];
      buildPhase = ''
        mkdir -pv $out/bin
        g++ -fopenmp $src/datalog/ext-stg-gc.cpp \
           -D_OPENMP -std=c++17 \
           -o $out/bin/ext-stg-gc
      '';
    };

  overrides = self: super: with pkgs.haskell.lib; {

    type-errors-pretty = dontCheck (
      doJailbreak (
        self.callCabal2nix
          "type-errors-pretty"
          (pkgs.fetchFromGitHub sources.type-errors-pretty)
          {}
      )
    );

    digest =
      self.callCabal2nix
        "digest"
        (pkgs.fetchFromGitHub sources.digest)
        {};

    final-pretty-printer = doJailbreak (
      self.callCabal2nix
        "final-pretty-printer"
        (pkgs.fetchFromGitHub sources.final-pretty-printer)
        {}
    );

    dap = doJailbreak (
      self.callCabal2nix
        "dap"
        (pkgs.fetchFromGitHub sources.dap)
        {}
    );

    dap-estgi-server =
      self.callCabal2nix
        "dap-estgi-server"
        ./dap-estgi-server
        {};

    external-stg =
      self.callCabal2nix
        "external-stg"
        "${ghc-wpo-src}/external-stg"
        {};

    external-stg-syntax =
      self.callCabal2nix
        "external-stg-syntax"
        "${ghc-wpo-src}/external-stg-syntax"
        {};

    external-stg-interpreter =
      self.callCabal2nixWithOptions
        "external-stg-interpreter"
        "${ghc-wpo-src}/external-stg-interpreter"
        "-fexternal-ext-stg-gc"
        {};

    souffle-haskell =
      dontCheck
        (doJailbreak
          (self.callCabal2nix "souffle-haskell"
            (pkgs.fetchFromGitHub sources.souffle-haskell) {}
          ));
  };

  hPkgs =
    pkgs.haskellPackages.override { inherit overrides; };

in

# this is the set we export for CI, and for shell.nix
{
  inherit (hPkgs) dap-estgi-server;
  inherit ext-stg-gc;
  inherit pkgs;
}
