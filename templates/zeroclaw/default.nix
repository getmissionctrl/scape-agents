# ZeroClaw AI assistant template
#
# Runs the ZeroClaw daemon (gateway + channels + heartbeat + scheduler)
# as an operator user systemd service with a headless desktop environment
# for browser automation / computer-use tools.
# Secrets (OPENROUTER_API_KEY etc.) are injected at /run/scape/secrets by the orchestrator.
{ self, pkgs, llm-agents, zeroclaw, zeroclawUiPkg, ... }:

{
  imports = [
    self.nixosModules.base-vm
  ];

  # Restrict file browser to zeroclaw data directory
  services.scape.agent.filesRoot = "/home/operator/.zeroclaw";

  # --- Packages ---
  environment.systemPackages = [
    zeroclaw.packages.${pkgs.system}.default

    # Browser automation — agent-browser bundles chromium + chromedriver
    llm-agents.packages.${pkgs.system}.agent-browser
    pkgs.chromium

    # Headless display
    pkgs.xorg-server  # Xvfb

    # Computer-use tools
    pkgs.xdotool          # mouse/keyboard control
    pkgs.scrot            # screenshots
    pkgs.xclip            # clipboard
    pkgs.imagemagick      # image processing for screenshots
    pkgs.x11vnc           # VNC server for desktop streaming

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
    pkgs.nodejs-slim

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
  # Wrapped with dbus-launch so xfce4-settings and other XFCE components
  # get a session bus (without it: "Unable to contact settings server").
  systemd.services.xfce-session = {
    description = "XFCE desktop session";
    after = [ "xvfb.service" ];
    requires = [ "xvfb.service" ];
    wantedBy = [ "multi-user.target" ];
    environment = {
      DISPLAY = ":99";
      HOME = "/home/operator";
      XDG_CONFIG_DIRS = "/etc/xdg:/run/current-system/sw/etc/xdg";
      XDG_DATA_DIRS = "/run/current-system/sw/share";
    };
    serviceConfig = {
      ExecStart = "${pkgs.dbus}/bin/dbus-launch --exit-with-session ${pkgs.xfce4-session}/bin/xfce4-session";
      Restart = "always";
      User = "operator";
    };
  };

  # x11vnc — exposes the Xvfb framebuffer as VNC on localhost:5900
  # The agent bridges this to WebSocket for browser-based desktop viewing.
  systemd.services.x11vnc = {
    description = "x11vnc VNC server";
    after = [ "xvfb.service" ];
    requires = [ "xvfb.service" ];
    wantedBy = [ "multi-user.target" ];
    environment = {
      DISPLAY = ":99";
    };
    serviceConfig = {
      ExecStart = "${pkgs.x11vnc}/bin/x11vnc -display :99 -forever -shared -rfbport 5900 -nopw -localhost -wait 50";
      Restart = "always";
      User = "operator";
    };
  };

  # --- ZeroClaw Web UI ---
  # Serves the React chat SPA on port 5001 and proxies chat WS to the WebChannel.
  # Caddy (port 5000) sits in front and routes admin traffic to the gateway.
  systemd.services.zeroclaw-ui = {
    description = "ZeroClaw Web UI";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" "zeroclaw-daemon.service" ];
    wants = [ "network-online.target" "zeroclaw-daemon.service" ];
    environment = {
      PORT = "5001";
      GATEWAY_URL = "ws://127.0.0.1:5100";
      NODE_ENV = "production";
      DATA_DIR = "/home/operator/.zeroclaw-ui";
    };
    serviceConfig = {
      User = "operator";
      Group = "operator";
      Restart = "always";
      RestartSec = "2s";
      WorkingDirectory = "/home/operator";
      ExecStart = "${pkgs.nodejs-slim}/bin/node ${zeroclawUiPkg}/dist/server/index.js";
    };
  };

  # --- Caddy reverse proxy ---
  # Routes admin dashboard (/_app/*, /api/*, /ws/*, /pair, /health) to the
  # zeroclaw gateway (port 42617) and everything else to zeroclaw-ui (port 5001).
  services.caddy = {
    enable = true;
    virtualHosts.":5000".extraConfig = ''
      # Admin dashboard SPA (embedded in gateway binary)
      @admin path /_app /_app/*
      handle @admin {
        reverse_proxy localhost:42617
      }

      # Gateway REST API
      handle /api/* {
        reverse_proxy localhost:42617
      }

      # Gateway WebSocket chat
      handle /ws/* {
        reverse_proxy localhost:42617
      }

      # Gateway pairing endpoint
      handle /pair {
        reverse_proxy localhost:42617
      }

      # Gateway health / metrics
      handle /health {
        reverse_proxy localhost:42617
      }
      handle /metrics {
        reverse_proxy localhost:42617
      }

      # OpenAI-compatible API
      handle /v1/* {
        reverse_proxy localhost:42617
      }

      # Everything else → chat UI
      handle {
        reverse_proxy localhost:5001
      }
    '';
  };

  # --- ZeroClaw config ---
  # Seed a minimal config.toml that enables the WebChannel (port 5100) so
  # zeroclaw-ui can proxy browser WebSocket connections into the agent loop.
  # The gateway runs on its default port (42617) and serves the admin dashboard.
  systemd.services.zeroclaw-config = {
    description = "Seed ZeroClaw config";
    wantedBy = [ "multi-user.target" ];
    after = [ "home-operator.mount" "fix-operator-home.service" ];
    requires = [ "home-operator.mount" ];
    serviceConfig = {
      Type = "oneshot";
      User = "operator";
      Group = "operator";
      RemainAfterExit = true;
    };
    script = ''
      mkdir -p /home/operator/.zeroclaw
      # Only seed if no config exists yet (don't overwrite user edits)
      if [ ! -f /home/operator/.zeroclaw/config.toml ]; then
        cat > /home/operator/.zeroclaw/config.toml << 'TOML'
[channels.web]
port = 5100
bind = "127.0.0.1"
stream_mode = "partial"
TOML
      fi
    '';
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
    after = [ "network-online.target" "home-operator.mount" "fix-operator-home.service" "xvfb.service" "zeroclaw-config.service" ];
    requires = [ "home-operator.mount" "zeroclaw-config.service" ];
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
      exec ${zeroclaw.packages.${pkgs.system}.default}/bin/zeroclaw daemon
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
    # UI must be first — platform proxy routes non-/ws/* traffic to services[0].port
    services = [
      { name = "ui"; port = 5000; path = "/"; type = "http"; }
    ];
  };
}
