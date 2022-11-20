# HelloClash
# Sébastien Deriaz
# 07.10.2022

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      # Temporary fix until fix-quartus pull request is accepted
      quartus-overlay = self: super: with builtins; {
        quartus-prime-lite = super.quartus-prime-lite.override {
         buildFHSUserEnv = attrs: super.buildFHSUserEnv (attrs // {
           # it's a bug in the quartus package
           multiPkgs = pkgs: (attrs.multiPkgs pkgs) ++ [
             pkgs.libxcrypt
           ];
         });
        };
      };
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.nix-filter.overlays.default
          quartus-overlay
        ];
        config.allowUnfree = true;
      };
      # Name of the Quartus project
      # The project files are ${name}.xxx
      name = "HelloClash";

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

      # Build binary file from VHDL source
      build = pkgs.stdenvNoCC.mkDerivation {
        inherit name;

        # Use of Quartus-prime-lite from nixpkgs
        nativeBuildInputs = with pkgs; [
          quartus-prime-lite
        ];
        # Sources
        srcs = [
          ./HelloClash.qpf # Quartus project file
          ./HelloClash.qsf # Quartus settings file
          ./pin_assignment_DE1_SoC.tcl # Location of pins in the De1-SoC
          vhdl
        ];
        version = "22.10";
        phases = [ "buildPhase" ];
        # Main build script
        # 1) Copy all of the necessary files inside the out directory
        # 2) Compile with Quartus command line
        # 3) Remove unnecessary files
        # Note that the build happens in nix's own directory and only the "result" folder allows
        # the user to have access to the output files
        buildPhase = ''
          # Create the output directory
          mkdir -p $out
          # Go the to the output directory and build everything there
          cd $out
          
          # Copy all of the files, without the nix hash at the beginning
          # and without mode and ownership preservation (to allow deletion later)
          for s in $srcs; do
            cp --no-preserve=mode,ownership -r $s $(stripHash $s)
          done

          # Compile 
          quartus_sh --flow compile ${name}

          # Remove unecessary files
          for s in $srcs; do
            rm -rf $(stripHash $s)
          done
        '';
      };

      # Flash package
      # Use the previously generated .sof file and flash it inside the first
      # JTAG-capable De1-SOC found connected to the computer
      flash = pkgs.writeShellApplication {
        name = "flash";
        text = ''
          CABLE=''${CABLE:-$(quartus_pgm -l | grep -iv info: | head -n1 | sed 's/[^ ]* //')}
          MODE=''${MODE:-JTAG}
          echo "Flashing cable '$CABLE' with mode '$MODE'"
          quartus_pgm \
            --cable="$CABLE" \
            --mode="$MODE" \
            --operation='p;${build}/${name}.sof@2'
        '';
        runtimeInputs = with pkgs; [ quartus-prime-lite ];
      };
    in
    {
      inherit pkgs;

      packages = {
        default = build;
        inherit vhdl build flash;
      };

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          quartus-prime-lite
          (haskellPackages.ghcWithPackages (ps: with ps; [
            clash-ghc
            ghc-typelits-extra
            ghc-typelits-knownnat
            ghc-typelits-natnormalise
          ]))
        ];
      };
    });
}
