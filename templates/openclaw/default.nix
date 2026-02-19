# OpenClaw AI assistant template
#
# Runs the OpenClaw gateway as an operator user systemd service,
# exposing the web UI and WebSocket control plane on port 18789.
{ self, pkgs, llm-agents, ... }:

{
  imports = [
    self.nixosModules.base-vm
  ];

  # OpenClaw from llm-agents.nix
  environment.systemPackages = [
    llm-agents.packages.${pkgs.system}.openclaw
  ];

  # More resources for AI workloads
  microvm.mem = 2048;
  microvm.vcpu = 2;

  # OpenClaw gateway as operator user service
  systemd.services.openclaw-gateway = {
    description = "OpenClaw Gateway";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" "scape-agent.service" ];
    serviceConfig = {
      User = "operator";
      Group = "operator";
      ExecStart = "${llm-agents.packages.${pkgs.system}.openclaw}/bin/openclaw gateway run";
      Restart = "always";
      RestartSec = "2s";
      WorkingDirectory = "/home/operator";
    };
  };

  # Template metadata
  scape.template.openclaw = {
    resources.memory = 2048;
    resources.cpu = 200;
    egress = "llm-providers";
    services = [
      { name = "gateway"; port = 18789; path = "/"; type = "websocket"; }
    ];
  };
}
