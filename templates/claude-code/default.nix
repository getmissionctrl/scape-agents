# Claude Code AI agent template
#
# Terminal-based AI coding assistant. User connects via the
# WebSocket terminal and runs Claude Code interactively.
{ self, pkgs, llm-agents, skills, ... }:

{
  imports = [
    self.nixosModules.base-vm
  ];

  # Claude Code from llm-agents.nix
  environment.systemPackages = [
    llm-agents.packages.${pkgs.system}.claude-code
    llm-agents.packages.${pkgs.system}.openclaw
    skills.packages.${pkgs.system}.skill-deps
  ];

  # Nix-managed skills — symlinked into Claude's skills directory
  environment.etc."skel/.claude/skills/research".source =
    "${skills.packages.${pkgs.system}.global-skills}/research";
  environment.etc."skel/.claude/skills/fabric".source =
    "${skills.packages.${pkgs.system}.global-skills}/fabric";
  environment.etc."skel/.claude/skills/pdf-manipulation".source =
    "${skills.packages.${pkgs.system}.global-skills}/pdf-manipulation";

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
