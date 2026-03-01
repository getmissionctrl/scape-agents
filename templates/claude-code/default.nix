# Claude Code AI agent template
#
# Terminal-based AI coding assistant. User connects via the
# WebSocket terminal and runs Claude Code interactively.
{ self, pkgs, lib, llm-agents, skills, ... }:

let
  globalSkills = skills.packages.${pkgs.system}.global-skills;

  # Build skill symlinks for ~/.claude/skills/
  skillFiles = lib.mapAttrs'
    (name: _: lib.nameValuePair ".claude/skills/${name}" { source = "${globalSkills}/${name}"; })
    (builtins.readDir globalSkills);
in
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

  # Nix-managed skills via Home Manager for operator user
  home-manager.users.operator = {
    home.stateVersion = "24.11";
    home.file = skillFiles;
  };

  # HM activation must run after the persistent home volume is mounted
  systemd.services."home-manager-operator" = {
    after = [ "home-operator.mount" "fix-operator-home.service" ];
    wants = [ "home-operator.mount" ];
  };

  # SSH for debugging
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "yes";
      PasswordAuthentication = false;
    };
  };
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJI4iaVjJcoj4La4dcWYDRyjlyDADrL3kbZ9Eux6I6s2 ben@scape"
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
