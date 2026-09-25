{
  description = "Merkle Mountain Belt Cloure";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.jdk21
            pkgs.clojure
            pkgs.clojure-lsp
            pkgs.git
            # studies/snowbridge: on-chain data collection and analysis
            (pkgs.python3.withPackages (ps: [ ps.web3 ps.requests ps.xxhash ps.matplotlib ps.numpy ]))
          ];
          buildInputs = [
            pkgs.clojure
            pkgs.clojure-lsp
            pkgs.graphviz
          ];
          shellHook = ''
            echo "Welcome to Clojure!"
            echo "Run 'clojure' to start the REPL"
          '';
        };
      }
    );
}
