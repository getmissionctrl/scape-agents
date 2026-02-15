# templates/duckdb-analyst/default.nix
# DuckDB analyst template with Python and data tools
#
# Usage:
#   scape-ctl template add --name duckdb-analyst \
#     --flake-ref github:getmissionctrl/scape-agents#duckdb-analyst
{ self, pkgs, ... }:

{
  imports = [
    self.nixosModules.base-vm
  ];

  # DuckDB, Python, and data analysis tools
  environment.systemPackages = with pkgs; [
    duckdb
    (python3.withPackages (ps: with ps; [
      pip
      duckdb
      pandas
      pyarrow
      requests
    ]))
    sqlite
  ];

  # Template metadata
  scape.template.duckdb-analyst = {
    resources.memory = 1024;
    resources.cpu = 200;
    resources.disk = 2048;
    egress = "deny-all";
  };
}
