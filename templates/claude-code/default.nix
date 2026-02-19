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
  microvm.mem = 16384;
  microvm.vcpu = 4;

  # Template metadata
  scape.template.claude-code = {
    resources.memory = 16384;
    resources.cpu = 400;
    egress = "llm-providers";
  };
}
