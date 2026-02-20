# ZeroClaw AI assistant template
#
# Runs the ZeroClaw gateway as an operator user systemd service,
# exposing the HTTP/WebSocket API on port 3000.
# Secrets (OPENROUTER_API_KEY etc.) are injected at /run/secrets by the orchestrator.
{ self, pkgs, llm-agents, ... }:

{
  imports = [
    self.nixosModules.base-vm
  ];

  # ZeroClaw from llm-agents fork
  environment.systemPackages = [
    llm-agents.packages.${pkgs.system}.zeroclaw
  ];

  # More resources for AI workloads
  microvm.mem = 2048;
  microvm.vcpu = 2;

  # ZeroClaw gateway as operator user service
  systemd.services.zeroclaw-gateway = {
    description = "ZeroClaw Gateway";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" "scape-agent.service" ];
    serviceConfig = {
      User = "operator";
      Group = "operator";
      Restart = "always";
      RestartSec = "2s";
      WorkingDirectory = "/home/operator";
    };
    # Read secrets from /run/secrets (injected by orchestrator via NATS)
    # and export them as environment variables before starting the gateway
    script = ''
      for f in /run/secrets/*; do
        [ -f "$f" ] && export "$(basename "$f")"="$(cat "$f")"
      done
      exec ${llm-agents.packages.${pkgs.system}.zeroclaw}/bin/zeroclaw gateway run
    '';
  };

  # Template metadata
  scape.template.zeroclaw = {
    resources.memory = 2048;
    resources.cpu = 200;
    egress = "llm-providers";
    secrets = [
      "OPENROUTER_API_KEY"
      "ZEROCLAW_MODEL"
      "ZEROCLAW_WORKSPACE"
    ];
    services = [
      { name = "gateway"; port = 3000; path = "/"; type = "http"; }
    ];
  };
}
