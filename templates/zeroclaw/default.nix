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

  # --- ZeroClaw daemon as operator user service ---
  # Manageable with: systemctl --user {start,stop,restart,status} zeroclaw-daemon
  systemd.user.services.zeroclaw-daemon = {
    description = "ZeroClaw Daemon";
    wantedBy = [ "default.target" ];
    after = [ "network-online.target" "xvfb.service" ];
    environment.DISPLAY = ":99";
    serviceConfig = {
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
