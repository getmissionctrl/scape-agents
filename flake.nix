{
  description = "Scape agent, protocol types, NixOS modules, and VM templates";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    natskell = {
      url = "github:getmissionctrl/natskell/fix/msg-payload-framing";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    microvm = {
      url = "github:microvm-nix/microvm.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    llm-agents = {
      url = "github:getmissionctrl/llm-agents.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zeroclaw = {
      url = "github:getmissionctrl/zeroclaw";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    skills = {
      url = "git+file:///home/ben/dev/skills";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, natskell, microvm, llm-agents, zeroclaw, skills, home-manager, ... }:
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

      # ZeroClaw web UI — React SPA + Hono server, built from packages/ workspace
      zeroclawUiPkg = pkgs.buildNpmPackage {
        pname = "zeroclaw-ui";
        version = "0.1.0";
        src = ./packages;

        npmDepsHash = "sha256-zzn9KjPakn0hD5VflD2/Rv1wOvv6Wg/Iw49ykUJ3RjQ=";

        buildPhase = ''
          runHook preBuild
          npm run build --workspace=zeroclaw-ui
          runHook postBuild
        '';

        installPhase = ''
          runHook preInstall
          mkdir -p $out/dist
          cp -r zeroclaw-ui/dist/client $out/dist/
          cp -r zeroclaw-ui/dist/server $out/dist/
          runHook postInstall
        '';
      };

      # Build a template as a NixOS microVM configuration
      mkTemplate = name: nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit self llm-agents zeroclaw zeroclawUiPkg skills; };
        modules = [
          microvm.nixosModules.microvm
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
          }
          ./templates/${name}/default.nix
        ];
      };
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

      # NixOS configurations for each template (microVM systems)
      nixosConfigurations = {
        debug = mkTemplate "debug";
        python-sandbox = mkTemplate "python-sandbox";
        duckdb-analyst = mkTemplate "duckdb-analyst";
        claude-code = mkTemplate "claude-code";
        openclaw = mkTemplate "openclaw";
        zeroclaw = mkTemplate "zeroclaw";
      };

      packages.${system} = {
        default = scapeAgentsPkg;
        scape-agents = scapeAgentsPkg;
        zeroclaw-ui = zeroclawUiPkg;

        # MicroVM runners (what `nix build .#debug` produces)
        debug = self.nixosConfigurations.debug.config.microvm.declaredRunner;
        python-sandbox = self.nixosConfigurations.python-sandbox.config.microvm.declaredRunner;
        duckdb-analyst = self.nixosConfigurations.duckdb-analyst.config.microvm.declaredRunner;
        claude-code = self.nixosConfigurations.claude-code.config.microvm.declaredRunner;
        openclaw = self.nixosConfigurations.openclaw.config.microvm.declaredRunner;
        zeroclaw = self.nixosConfigurations.zeroclaw.config.microvm.declaredRunner;
      };

      # Template flake outputs for `nix flake init`
      templates = {
        python-sandbox = {
          path = ./templates/python-sandbox;
          description = "Python sandbox with pip";
        };
        duckdb-analyst = {
          path = ./templates/duckdb-analyst;
          description = "DuckDB analyst with Python and data tools";
        };
        debug = {
          path = ./templates/debug;
          description = "Debug template with SSH and verbose logging";
        };
        claude-code = {
          path = ./templates/claude-code;
          description = "Claude Code terminal-based AI agent";
        };
        openclaw = {
          path = ./templates/openclaw;
          description = "OpenClaw AI assistant with gateway web service";
        };
        zeroclaw = {
          path = ./templates/zeroclaw;
          description = "ZeroClaw AI assistant with gateway web service";
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
