# templates/python-sandbox/default.nix
# Python sandbox template with pip
#
# Usage:
#   scape-ctl template add --name python-sandbox \
#     --flake-ref github:getmissionctrl/scape-agents#python-sandbox
{ self, pkgs, ... }:

{
  imports = [
    self.nixosModules.base-vm
  ];

  # Python with pip and common packages
  environment.systemPackages = with pkgs; [
    (python3.withPackages (ps: with ps; [
      pip
      requests
      numpy
      pandas
    ]))
  ];

  # Template metadata
  scape.template.python-sandbox = {
    resources.memory = 512;
    resources.cpu = 100;
    resources.disk = 1024;
    egress = "deny-all";
  };
}
