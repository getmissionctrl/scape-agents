{
  description = "Scape agent, protocol types, NixOS modules, and VM templates";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    natskell = {
      url = "github:getmissionctrl/natskell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, natskell, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      # GHC 9.10 with natskell from source
      hsPkgs = pkgs.haskell.packages.ghc910.override {
        overrides = hfinal: hprev: {
          natskell = pkgs.haskell.lib.doJailbreak
            (hfinal.callCabal2nix "natskell" natskell {});
        };
      };

      # scape-agents Haskell package
      scapeAgentsPkg = pkgs.haskell.lib.overrideCabal
        (hsPkgs.callCabal2nix "scape-agents" ./. {})
        (old: {
          librarySystemDepends = (old.librarySystemDepends or []) ++ [ pkgs.zlib ];
          testToolDepends = (old.testToolDepends or []) ++ [ pkgs.nats-server ];
        });
    in {
      nixosModules = {
        # Agent-only mode: for inside VMs
        agent = { lib, ... }: {
          imports = [
            ./modules/agent.nix
            ./modules/template.nix
          ];
          services.scape.agent.package = lib.mkDefault scapeAgentsPkg;
        };

        # Base microVM configuration
        base-vm = ./modules/base-vm.nix;
      };

      packages.${system} = {
        default = scapeAgentsPkg;
        scape-agents = scapeAgentsPkg;
      };

      # Template flake outputs for `scape-ctl template add --flake-ref`
      templates = {
        python-sandbox = {
          path = ./templates/python-sandbox;
          description = "Python sandbox with pip";
        };
        duckdb-analyst = {
          path = ./templates/duckdb-analyst;
          description = "DuckDB analyst with Python and data tools";
        };
      };

      devShells.${system}.default = hsPkgs.shellFor {
        packages = p: [ scapeAgentsPkg ];
        nativeBuildInputs = with pkgs; [
          cabal-install
          hsPkgs.haskell-language-server
          pkg-config
          zlib.dev
          nats-server
        ];
      };
    };
}
