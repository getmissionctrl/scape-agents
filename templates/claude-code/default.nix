# Claude Code AI agent template
#
# Terminal-based AI coding assistant. User connects via the
# WebSocket terminal and runs Claude Code interactively.
{ self, pkgs, lib, llm-agents, skills, ... }:

let
  globalSkills = skills.packages.${pkgs.system}.global-skills;
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

  # Symlink Nix-managed skills into operator's Claude config
  # Uses a systemd service because /nix/store is read-only erofs in the
  # microVM, so Home Manager activation fails. Skills docs are already
  # in the store image — we just symlink them into the home volume.
  systemd.services.claude-skills = {
    description = "Symlink Nix-managed skills into operator Claude config";
    wantedBy = [ "multi-user.target" ];
    after = [ "home-operator.mount" "fix-operator-home.service" ];
    wants = [ "home-operator.mount" ];
    serviceConfig = {
      Type = "oneshot";
      User = "operator";
      Group = "operator";
      RemainAfterExit = true;
    };
    script = ''
      mkdir -p /home/operator/.claude/skills
      for skill in ${globalSkills}/*; do
        name=$(basename "$skill")
        target="/home/operator/.claude/skills/$name"
        if [ -L "$target" ]; then
          rm "$target"
        fi
        if [ ! -e "$target" ]; then
          ln -s "$skill" "$target"
        fi
      done
    '';
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
