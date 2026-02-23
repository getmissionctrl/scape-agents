# ZeroClaw AI assistant template
#
# Runs the ZeroClaw daemon (gateway + channels + heartbeat + scheduler)
# as an operator user systemd service with a headless desktop environment
# for browser automation / computer-use tools.
# Secrets (OPENROUTER_API_KEY etc.) are injected at /run/scape/secrets by the orchestrator.
{ self, pkgs, llm-agents, ... }:

{
  imports = [
    self.nixosModules.base-vm
  ];

  # --- Packages ---
  environment.systemPackages = [
    llm-agents.packages.${pkgs.system}.zeroclaw

    # Browser for automation / computer-use
    pkgs.chromium

    # Headless display
    pkgs.xorg-server      # Xvfb

    # Computer-use tools
    pkgs.xdotool          # mouse/keyboard control
    pkgs.scrot            # screenshots
    pkgs.xclip            # clipboard

    # Shell essentials
    pkgs.bash
    pkgs.coreutils
    pkgs.gnugrep
    pkgs.gnused
    pkgs.gawk
    pkgs.findutils
    pkgs.which
    pkgs.tree
    pkgs.file
    pkgs.less

    # Data processing
    pkgs.jq
    pkgs.yq-go
    pkgs.duckdb
    pkgs.sqlite
    pkgs.csvkit

    # Networking
    pkgs.curl
    pkgs.wget
    pkgs.httpie

    # Version control
    pkgs.git

    # Text search
    pkgs.ripgrep
    pkgs.fd

    # Scripting runtimes
    pkgs.python3

    # System tools
    pkgs.procps           # ps, top, pgrep, pkill
    pkgs.htop
    pkgs.tmux
  ];

  # SSH for interactive debugging
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "yes";
      PasswordAuthentication = false;
    };
  };

  # SSH keys for root and operator
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJI4iaVjJcoj4La4dcWYDRyjlyDADrL3kbZ9Eux6I6s2 ben@scape"
  ];
  users.users.operator.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJI4iaVjJcoj4La4dcWYDRyjlyDADrL3kbZ9Eux6I6s2 ben@scape"
  ];

  # More resources for AI + desktop workloads
  microvm.mem = 4096;
  microvm.vcpu = 2;

  # --- Headless Desktop (XFCE + Xvfb) ---
  environment.variables.DISPLAY = ":99";

  services.xserver = {
    enable = true;
    displayManager.lightdm.enable = false;
    desktopManager.xfce.enable = true;
  };

  # Xvfb virtual framebuffer — provides :99 display for computer-use tools
  systemd.services.xvfb = {
    description = "Xvfb virtual framebuffer";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.xorg-server}/bin/Xvfb :99 -screen 0 1920x1080x24 -ac";
      Restart = "always";
      User = "operator";
    };
  };

  # XFCE session on the virtual display
  systemd.services.xfce-session = {
    description = "XFCE desktop session";
    after = [ "xvfb.service" ];
    requires = [ "xvfb.service" ];
    wantedBy = [ "multi-user.target" ];
    environment = {
      DISPLAY = ":99";
      HOME = "/home/operator";
    };
    serviceConfig = {
      ExecStart = "${pkgs.xfce4-session}/bin/xfce4-session";
      Restart = "always";
      User = "operator";
    };
  };

  # --- ZeroClaw daemon service ---
  # Runs as operator but managed as a system service so it can depend on
  # home-operator.mount (user services can't depend on system mounts).
  # Operator can restart via: kill $(pgrep -f 'zeroclaw daemon')
  # (systemd Restart=always will relaunch it automatically)
  systemd.services.zeroclaw-daemon = {
    description = "ZeroClaw Daemon";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" "home-operator.mount" "fix-operator-home.service" "xvfb.service" ];
    requires = [ "home-operator.mount" ];
    environment.DISPLAY = ":99";
    path = [ "/run/current-system/sw" ];
    serviceConfig = {
      User = "operator";
      Group = "operator";
      Restart = "always";
      RestartSec = "2s";
      WorkingDirectory = "/home/operator";
    };
    # Read secrets from /run/scape/secrets (injected by orchestrator via NATS)
    # and export them as environment variables before starting the daemon
    script = ''
      for f in /run/scape/secrets/*; do
        [ -f "$f" ] && export "$(basename "$f")"="$(cat "$f")"
      done
      exec ${llm-agents.packages.${pkgs.system}.zeroclaw}/bin/zeroclaw daemon
    '';
  };

  # Allow operator to restart the daemon without sudo
  security.sudo.extraRules = [{
    users = [ "operator" ];
    commands = [
      { command = "/run/current-system/sw/bin/systemctl restart zeroclaw-daemon"; options = [ "NOPASSWD" ]; }
      { command = "/run/current-system/sw/bin/systemctl stop zeroclaw-daemon"; options = [ "NOPASSWD" ]; }
      { command = "/run/current-system/sw/bin/systemctl start zeroclaw-daemon"; options = [ "NOPASSWD" ]; }
      { command = "/run/current-system/sw/bin/systemctl status zeroclaw-daemon"; options = [ "NOPASSWD" ]; }
    ];
  }];

  # Template metadata
  scape.template.zeroclaw = {
    resources.memory = 4096;
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
