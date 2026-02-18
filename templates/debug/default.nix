# templates/debug/default.nix
# Debug template with SSH access and verbose logging
#
# Adds SSH and debug log level on top of the base VM configuration.
# Useful for troubleshooting boot issues and inspecting VM state.
#
# Usage:
#   scape-ctl template add --name debug \
#     --flake-ref github:getmissionctrl/scape-agents#debug
{ self, pkgs, ... }:

{
  imports = [
    self.nixosModules.base-vm
  ];

  # Debug log level for troubleshooting
  services.scape.agent.logLevel = "debug";

  # SSH for interactive debugging
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "yes";
      PasswordAuthentication = false;
    };
  };

  # Root SSH key for interactive debugging
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJI4iaVjJcoj4La4dcWYDRyjlyDADrL3kbZ9Eux6I6s2 ben@scape"
  ];

  # Simple demo HTTP server
  systemd.services.demo-web = {
    description = "Demo HTTP server";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.python3}/bin/python3 -m http.server 9090 --directory /tmp";
      Restart = "always";
    };
  };

  # Template metadata
  scape.template.debug = {
    resources.memory = 256;
    resources.cpu = 100;
    resources.disk = 1024;
    egress = "deny-all";
    services = [
      { name = "web"; port = 9090; path = "/"; type = "http"; }
    ];
  };
}
