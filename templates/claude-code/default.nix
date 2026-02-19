# Claude Code AI agent template
#
# Terminal-based AI coding assistant. User connects via the
# WebSocket terminal and runs Claude Code interactively.
{ self, pkgs, llm-agents, ... }:

{
  imports = [
    self.nixosModules.base-vm
  ];

  # Claude Code from llm-agents.nix
  environment.systemPackages = [
    llm-agents.packages.${pkgs.system}.claude-code
  ];

  # More resources for AI workloads
  microvm.mem = 2048;
  microvm.vcpu = 2;

  # Template metadata
  scape.template.claude-code = {
    resources.memory = 2048;
    resources.cpu = 200;
    egress = "llm-providers";
  };
}
