{
  description = "Development environment with npm";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.cabal-install # Build tool
            haskellPackages.ghc # Compiler
            haskellPackages.hlint # Linter
            haskellPackages.ormolu # Formatter
          ];

          shellHook = ''
            export SHELL=${pkgs.bashInteractive}/bin/bash
          '';
        };
      }
    );
}
