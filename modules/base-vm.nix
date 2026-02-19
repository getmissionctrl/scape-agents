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

    # No persistent volumes - ephemeral sandbox
    # Persistent home for operator will be added when orchestrator
    # supports provisioning and injecting volume images at spawn time
    volumes = [];
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

  # System basics
  system.stateVersion = "24.05";

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
  ];

  # Enable systemd lingering for operator so user services persist
  systemd.tmpfiles.rules = [
    "d /var/lib/systemd/linger 0755 root root -"
    "f /var/lib/systemd/linger/operator 0644 root root -"
  ];

  # Fast boot
  boot.isContainer = false;
  systemd.services."serial-getty@".enable = false;
}
