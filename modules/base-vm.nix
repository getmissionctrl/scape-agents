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
    mem = 256;  # 256MB RAM
    vcpu = 1;   # 1 vCPU

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

  # System basics
  system.stateVersion = "24.05";

  # Minimal userland for command execution
  environment.systemPackages = with pkgs; [
    busybox
    coreutils
    bash
    curl
    jq
    iproute2
  ];

  # Fast boot
  boot.isContainer = false;
  systemd.services."serial-getty@".enable = false;
}
