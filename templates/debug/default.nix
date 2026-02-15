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

  # Template metadata
  scape.template.debug = {
    resources.memory = 256;
    resources.cpu = 100;
    resources.disk = 1024;
    egress = "deny-all";
  };
}
