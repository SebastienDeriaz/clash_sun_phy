# HelloClash
# Sébastien Deriaz
# 07.10.2022

{
  inputs = {
    # Use the latest nixpkgs repository
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    clash-compiler = {
      url = github:clash-lang/clash-compiler;
      flake = false;
    };
    clash-utils = {
      url = github:adamwalker/clash-utils;
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.nix-filter.overlays.default
        ];
        config.allowUnfree = true;
      };

      # Generate VHDL sources (clash)
      vhdl = pkgs.stdenvNoCC.mkDerivation {
        name = "vhdl";
        nativeBuildInputs = with pkgs; [
          quartus-prime-lite
          (haskellPackages.ghcWithPackages (ps: with ps; [
            clash-ghc
            ghc-typelits-extra
            ghc-typelits-knownnat
            ghc-typelits-natnormalise
          ]))
        ];
        src = pkgs.nix-filter {
          root = ./src;
          include = with pkgs.nix-filter; [
            (matchExt "hs")
            (isDirectory)
          ];
        };
        buildPhase = ''
          clash \
            -odir . \
            -hidir . \
            -tmpdir . \
            --vhdl $src/Top.hs
        '';
        installPhase = ''
          mv vhdl $out
        '';
      };
      
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: with pkgs.haskell.lib; {
          clash-utils = dontCheck (doJailbreak (self.callCabal2nix "clash-utils" inputs.clash-utils { }));
          SunPhy = self.callCabal2nix "SunPhy" ./. { };
          concurrent-supply = doJailbreak (markUnbroken super.concurrent-supply);
        } // builtins.foldl' (acc: name: acc // { "${name}" = self.callCabal2nix name "${inputs.clash-compiler}/${name}" { }; }) { }
          [
            "clash-ffi"
            "clash-ghc"
            "clash-lib"
            "clash-lib-hedgehog"
            "clash-prelude"
            "clash-prelude-hedgehog"
            "clash-cores"
          ];
      };
    in
    {
      inherit pkgs;

      packages = {
        default = vhdl;
        inherit vhdl;
        inherit (haskellPackages) SunPhy;
      };

      devShells.default = haskellPackages.shellFor {
        packages = ps: with ps; [
          SunPhy
          clash-ghc
          ghc-typelits-extra
          ghc-typelits-knownnat
          ghc-typelits-natnormalise
        ];
        nativeBuildInputs = with haskellPackages; [
          pkgs.gtkwave
          cabal-install
          clash-ghc
          haskell-language-server
          hpack
        ];
      };
    });
}
