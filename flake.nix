{
  description = "Merkle Mountain Belt Cloure";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem
    (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
    in {
      devShells.default = pkgs.mkShell {
         buildInputs = [
          pkgs.clojure
          pkgs.graphviz
         ];
         shellHook = ''
           echo "Welcome to Clojure!"
           echo "Run 'clojure' to start the REPL"
          '';
      };
    });
}
