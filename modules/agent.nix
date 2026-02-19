# nix/modules/scape/agent.nix
#
# Scape agent systemd service for microVMs.
#
# The agent:
# 1. Boots and fetches config from Firecracker MMDS (http://169.254.169.254/scape)
# 2. MMDS provides: instance ID, NATS URL, and NATS credentials
# 3. Agent connects to NATS for command/observation messaging
# 4. Agent runs HTTP server for health checks (/healthz)
#
# If MMDS is not available (e.g., running outside Firecracker), agent runs
# in HTTP-only mode unless --nats-url is provided via CLI.
{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.services.scape.agent;
in
{
  options.services.scape.agent = {
    enable = mkEnableOption "Scape agent service";

    package = mkOption {
      type = types.package;
      description = "Scape agent package";
    };

    httpPort = mkOption {
      type = types.port;
      default = 8080;
      description = "HTTP server port for health checks and metrics";
    };

    logLevel = mkOption {
      type = types.enum [ "debug" "info" "notice" "warning" "error" ];
      default = "info";
      description = "Log level";
    };

    # For testing outside Firecracker (when MMDS is not available)
    natsUrl = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "NATS URL override (for testing without MMDS)";
    };

    natsCreds = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "NATS credentials file (for testing without MMDS)";
    };

    instanceId = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Instance ID override (for testing without MMDS)";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.scape-agent = {
      description = "Scape Agent - MicroVM Execution Agent";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = concatStringsSep " " ([
          "${cfg.package}/bin/scape-agent"
          "--port ${toString cfg.httpPort}"
          "--log-level ${cfg.logLevel}"
        ] ++ optionals (cfg.natsUrl != null) [
          "--nats-url ${cfg.natsUrl}"
        ] ++ optionals (cfg.natsCreds != null) [
          "--nats-creds ${cfg.natsCreds}"
        ] ++ optionals (cfg.instanceId != null) [
          "--instance-id ${cfg.instanceId}"
        ]);
        Restart = "always";
        RestartSec = "1s";

        # RuntimeDirectory creates /run/scape automatically with proper ownership
        RuntimeDirectory = "scape";
        RuntimeDirectoryMode = "0755";

        # Security hardening (sandbox-appropriate)
        # Note: PrivateTmp disabled so file transfers to /tmp are accessible
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = false;
        ReadWritePaths = [ "/tmp" "/home/operator" ];  # /run/scape is handled by RuntimeDirectory
      };
    };

    # Ensure /tmp and /run/scape are available for command execution
    systemd.tmpfiles.rules = [
      "d /tmp 1777 root root -"
      "d /run/scape 0755 root root -"
    ];
  };
}
