# modules/base-vm.nix
# Base microVM configuration for scape agent VMs.
#
# This provides the minimal Firecracker microVM setup:
# - Firecracker hypervisor with erofs compression
# - DHCP networking on eth0
# - Scape agent service enabled
# - Minimal userland for command execution
#
# Templates import this module and add their own packages/services.
# Usage:
#   { self, pkgs, ... }: {
#     imports = [ self.nixosModules.base-vm ];
#     environment.systemPackages = [ pkgs.python3 ];
#   }
{ self, pkgs, lib, config, ... }:

{
  imports = [
    ./agent.nix
    ./template.nix
  ];

  microvm = {
    # Hypervisor - Firecracker for fast snapshots
    hypervisor = "firecracker";

    # Minimal resources (templates can override)
    mem = lib.mkDefault 256;  # 256MB RAM
    vcpu = lib.mkDefault 1;   # 1 vCPU

    # Fast disk compression for quick builds
    storeDiskErofsFlags = [ "-zlz4" ];

    # Network interface - actual TAP name set by orchestrator at runtime
    interfaces = [{
      type = "tap";
      id = "scape-vm";
      mac = "02:00:00:00:00:01";
    }];

    # Vsock for control plane (agent <-> orchestrator)
    vsock = {
      cid = 3;  # Guest CID (host is always 2)
    };

    # No persistent microvm volumes - persistence is via Firecracker
    # secondary block device (/dev/vdb) mounted at /home/operator
    volumes = [];
  };

  # Persistent home volume (attached by orchestrator as /dev/vdb)
  # With nofail, instances without a volume boot normally with ephemeral home.
  fileSystems."/home/operator" = {
    device = "/dev/vdb";
    fsType = "ext4";
    options = [ "nofail" "defaults" ];
  };

  # Ensure operator owns their home directory after volume mount
  systemd.services.fix-operator-home = {
    description = "Ensure operator home ownership";
    wantedBy = [ "multi-user.target" ];
    after = [ "home-operator.mount" ];
    requires = [ "home-operator.mount" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.coreutils}/bin/chown -R operator:operator /home/operator";
    };
    unitConfig.ConditionPathExists = "/dev/vdb";
  };

  # Enable scape-agent
  # Agent gets NATS config from MMDS (injected by orchestrator after VM boot)
  services.scape.agent = {
    enable = true;
    package = self.packages.${pkgs.system}.default;
    httpPort = 8080;
    logLevel = lib.mkDefault "info";
  };

  # DHCP networking
  networking = {
    hostName = "scape-agent";
    useNetworkd = true;
    firewall.enable = false;
  };

  systemd.network = {
    enable = true;
    networks."10-eth0" = {
      matchConfig.Name = "eth0";
      networkConfig = {
        DHCP = "ipv4";
      };
      dhcpV4Config = {
        UseDNS = true;
        UseRoutes = true;
      };
    };
  };

  # Prometheus node exporter for host-level metrics scraping
  services.prometheus.exporters.node = {
    enable = true;
  };

  # Journal log shipping to Loki via Promtail
  # Fetches instance ID from MMDS at boot, generates Promtail config,
  # and ships journal logs to Loki on the host bridge.
  systemd.services.scape-log-shipper = {
    description = "Ship VM journal logs to Loki";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];

    path = with pkgs; [ curl jq iproute2 coreutils ];

    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = "10s";
      SupplementaryGroups = [ "systemd-journal" ];
      DynamicUser = true;
      RuntimeDirectory = "scape-log-shipper";
      StateDirectory = "scape-log-shipper";

      ExecStartPre = pkgs.writeShellScript "gen-promtail-config" ''
        # Wait for MMDS to become available and fetch instance ID
        INSTANCE_ID=""
        for i in $(seq 1 30); do
          INSTANCE_ID=$(curl -sf -H 'Accept: application/json' \
            http://169.254.169.254/scape \
            | jq -r '.instanceId // empty' 2>/dev/null) \
            && [ -n "$INSTANCE_ID" ] && break
          sleep 2
        done
        INSTANCE_ID="''${INSTANCE_ID:-unknown}"

        # Discover Loki host from default gateway (= bridge host)
        GATEWAY=$(ip route | awk '/default/ {print $3; exit}')
        LOKI_URL="http://''${GATEWAY:-10.99.0.1}:3100/loki/api/v1/push"

        cat > /run/scape-log-shipper/config.yaml <<EOF
        server:
          http_listen_port: 0
          grpc_listen_port: 0

        positions:
          filename: /var/lib/scape-log-shipper/positions.yaml

        clients:
          - url: $LOKI_URL

        scrape_configs:
          - job_name: vm-journal
            journal:
              max_age: 12h
              labels:
                job: vm-journal
                instance_id: "$INSTANCE_ID"
            relabel_configs:
              - source_labels: ["__journal__systemd_unit"]
                target_label: unit
              - source_labels: ["__journal_priority"]
                target_label: priority
        EOF
      '';

      ExecStart = "${pkgs.grafana-loki}/bin/promtail -config.file=/run/scape-log-shipper/config.yaml";
    };
  };

  # System basics
  system.stateVersion = "24.05";
  i18n.defaultLocale = "C.UTF-8";

  # Unprivileged operator user for running all user workloads
  users.users.operator = {
    isNormalUser = true;
    uid = 1000;
    group = "operator";
    home = "/home/operator";
    shell = "${pkgs.bash}/bin/bash";
  };

  users.groups.operator.gid = 1000;

  # Minimal userland for command execution
  environment.systemPackages = with pkgs; [
    busybox
    coreutils
    bash
    curl
    jq
    iproute2
    tmux
    util-linux
    btop
  ];

  # tmux configuration for operator (core settings, no TPM plugins)
  environment.etc."skel/.config/tmux/tmux.conf".text = ''
    set-option -g default-terminal 'tmux-256color'
    set-option -g terminal-overrides ',xterm-256color:RGB'

    set -g prefix ^A
    set -g base-index 1
    set -g detach-on-destroy off
    set -g escape-time 0
    set -g history-limit 1000000
    set -g renumber-windows on
    set -g set-clipboard on
    set -g status-position top
    set -g mouse on
    setw -g mode-keys vi
    set -g pane-active-border-style 'fg=magenta,bg=default'
    set -g pane-border-style 'fg=brightblack,bg=default'

    # Keybindings
    bind ^X lock-server
    bind ^C new-window -c "#{pane_current_path}"
    bind ^D detach
    bind H previous-window
    bind L next-window
    bind r command-prompt "rename-window %%"
    bind R source-file ~/.config/tmux/tmux.conf
    bind ^A last-window
    bind ^W list-windows
    bind w list-windows
    bind z resize-pane -Z
    bind ^L refresh-client
    bind | split-window
    bind s split-window -v -c "#{pane_current_path}"
    bind v split-window -h -c "#{pane_current_path}"
    bind h select-pane -L
    bind j select-pane -D
    bind k select-pane -U
    bind l select-pane -R
    bind -r -T prefix , resize-pane -L 20
    bind -r -T prefix . resize-pane -R 20
    bind -r -T prefix - resize-pane -D 7
    bind -r -T prefix = resize-pane -U 7
    bind : command-prompt
    bind * setw synchronize-panes
    bind c kill-pane
    bind x swap-pane -D
    bind S choose-session
    bind K send-keys "clear"\; send-keys "Enter"
    bind-key -T copy-mode-vi v send-keys -X begin-selection
  '';

  # Copy skel tmux config into operator's home on first boot
  systemd.services.operator-tmux-config = {
    description = "Set up tmux config for operator";
    wantedBy = [ "multi-user.target" ];
    after = [ "home-operator.mount" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "setup-tmux" ''
        dir=/home/operator/.config/tmux
        if [ ! -f "$dir/tmux.conf" ]; then
          mkdir -p "$dir"
          cp /etc/skel/.config/tmux/tmux.conf "$dir/tmux.conf"
          chown -R operator:operator /home/operator/.config
        fi
      '';
    };
  };

  # Enable systemd lingering for operator so user services persist
  systemd.tmpfiles.rules = [
    "d /var/lib/systemd/linger 0755 root root -"
    "f /var/lib/systemd/linger/operator 0644 root root -"
  ];

  # Fast boot
  boot.isContainer = false;
  systemd.services."serial-getty@".enable = false;
}
