{
  description = ''
    A template flake for development in haskell on x86_64-linux
    providing ghc, cabal, stack and all libraries needed for gloss,
    as well as various development tools useful for haskell.
  '';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskell.packages."ghc967";
          # need to match Stackage LTS version from stack.yaml snapshot

        myDevTools = [
          stack-wrapped # Stack, haskell package manager
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          hPkgs.ghcid # Continuous terminal Haskell compile checker
          hPkgs.fourmolu # Haskell formatter
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          hPkgs.retrie # Haskell refactoring tool
          hPkgs.cabal-install

          # External dependencies of gloss
          pkgs.libGL
          pkgs.libGLU
          pkgs.freeglut

          # External dependencies of openAL
          pkgs.openal
          pkgs.freealut

        ];

        # Wrap Stack to work with our Nix integration. We do not want to modify
        # stack.yaml so non-Nix users do not notice anything.
        # - no-nix: We do not want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Do not try to install GHC if no matching GHC found on PATH
        # Otherwise stack would shadow all env variables during `stack exec / run`
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in {

        packages.default = hPkgs.developPackage {
            root = ./.;
            name = "glossy-ball-demo";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          # Freeglut is a dynamic runtime dependency of gloss,
          # so we have to point `LD_LIBRARY_PATH` to it.
          LD_LIBRARY_PATH = "${pkgs.freeglut}/lib";
          # Inform stack on where to find `libGL.so` and the concrete OpenGL library.
          #EXTRA_INCLUDE_DIRS="${pkgs.libGL}/include";
          #EXTRA_LIB_DIRS="${pkgs.libGL}/lib ${pkgs.libGLU}/lib";
        };
      });
}
