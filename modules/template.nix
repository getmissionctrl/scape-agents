# nix/modules/scape/template.nix
#
# NixOS module for defining Scape MicroVM templates.
# Templates define the resource limits, networking, and packages for VMs.
# The actual microvm derivation is created by a separate builder.
{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.scape.template;

  # Template options submodule
  templateOpts = { name, ... }: {
    options = {
      name = mkOption {
        type = types.str;
        default = name;
        description = "Template name (defaults to attribute name)";
      };

      resources = {
        memory = mkOption {
          type = types.int;
          default = 512;
          description = "Memory allocation in MB";
        };

        cpu = mkOption {
          type = types.int;
          default = 100;
          description = "CPU quota (100 = 1 core equivalent)";
        };

        disk = mkOption {
          type = types.int;
          default = 1024;
          description = "Disk size in MB";
        };
      };

      egress = mkOption {
        type = types.nullOr (types.enum [ "allow-all" "llm-providers" "deny-all" ]);
        default = "deny-all";
        description = ''
          Egress network policy:
          - deny-all: No outbound network access (default, most secure)
          - llm-providers: Allow connections to known LLM API endpoints only
          - allow-all: Unrestricted outbound access
        '';
      };

      packages = mkOption {
        type = types.listOf types.package;
        default = [];
        description = "Additional packages to include in the template";
        example = literalExpression "[ pkgs.python3 pkgs.nodejs ]";
      };

      systemd.services = mkOption {
        type = types.attrsOf types.anything;
        default = {};
        description = "Additional systemd services to run in the template";
      };

      environment = mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = "Environment variables to set in the template";
        example = { LANG = "en_US.UTF-8"; };
      };

      mounts = {
        readOnly = mkOption {
          type = types.listOf (types.submodule {
            options = {
              hostPath = mkOption {
                type = types.str;
                description = "Path on the host to mount";
              };
              guestPath = mkOption {
                type = types.str;
                description = "Mount point inside the VM";
              };
            };
          });
          default = [];
          description = "Read-only bind mounts from host to guest";
        };

        volumes = mkOption {
          type = types.listOf (types.submodule {
            options = {
              name = mkOption {
                type = types.str;
                description = "Volume name";
              };
              guestPath = mkOption {
                type = types.str;
                description = "Mount point inside the VM";
              };
            };
          });
          default = [];
          description = "Named volumes (persisted across instances)";
        };
      };

      secrets = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Secret names to inject into the template at /run/secrets";
        example = [ "api-key" "database-password" ];
      };
    };
  };

in
{
  options.scape.template = mkOption {
    type = types.attrsOf (types.submodule templateOpts);
    default = {};
    description = ''
      Scape template definitions.

      Templates define the configuration for MicroVM instances including
      resource limits, network policies, packages, and services.

      Example:
        scape.template.python-sandbox = {
          resources.memory = 512;
          resources.cpu = 100;
          egress = "deny-all";
          packages = [ pkgs.python3 ];
        };
    '';
    example = literalExpression ''
      {
        python-sandbox = {
          resources.memory = 512;
          resources.cpu = 100;
          egress = "deny-all";
          packages = [ pkgs.python3 pkgs.pip ];
        };

        web-runner = {
          resources.memory = 1024;
          resources.cpu = 200;
          egress = "allow-all";
          packages = [ pkgs.nodejs pkgs.chromium ];
        };
      }
    '';
  };

  # Templates are defined here but built by a separate derivation builder.
  # This module only defines the schema and stores the configuration.
  # The scape orchestrator reads these configurations to build MicroVMs.
  config = {
    # No runtime config - this is a pure schema/data definition
    # The scape-ctl and orchestrator consume scape.template.* directly
  };
}
