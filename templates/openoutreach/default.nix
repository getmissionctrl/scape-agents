# templates/openoutreach/default.nix
# OpenOutreach LinkedIn automation daemon in a Firecracker microVM.
#
# Runs the Docker image as an OCI container (via Podman) with Xvfb + x11vnc
# on the VM host so the scape agent's /ws/vnc proxy can expose the browser.
#
# First-run setup (onboarding requires a TTY):
#   1. SSH into VM, stop the service: systemctl stop podman-openoutreach
#   2. Run interactively: podman run -it --rm --network=host \
#        -e DISPLAY=:99 \
#        -v /home/operator/assets:/app/assets \
#        -v /tmp/.X11-unix:/tmp/.X11-unix \
#        ghcr.io/eracle/openoutreach:latest
#   3. Complete onboarding prompts, verify LinkedIn login via VNC
#   4. Ctrl-C, then: systemctl start podman-openoutreach
#
# Usage:
#   scape-ctl template add --name openoutreach \
#     --flake-ref github:getmissionctrl/scape-agents#openoutreach
{ self, pkgs, ... }:

{
  imports = [
    self.nixosModules.base-vm
  ];

  # Heavy workload: Chromium + ML pipeline
  microvm.mem = 8192;
  microvm.vcpu = 4;

  # SSH for interactive onboarding and debugging
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "yes";
      PasswordAuthentication = false;
    };
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJI4iaVjJcoj4La4dcWYDRyjlyDADrL3kbZ9Eux6I6s2 ben@scape"
  ];

  # Podman as OCI container runtime — store images on persistent volume
  # (rootfs is a read-only erofs + tmpfs overlay, not enough space for OCI layers)
  virtualisation.podman.enable = true;
  virtualisation.oci-containers.backend = "podman";
  virtualisation.containers.storage.settings.storage.graphroot = "/home/operator/containers/storage";

  # --- VM-level display services ---
  # Xvfb provides the X display; x11vnc exposes it on port 5900
  # (where the scape agent's /ws/vnc proxy expects it)

  systemd.services.xvfb = {
    description = "Xvfb virtual framebuffer";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.xorg.xorgserver}/bin/Xvfb :99 -screen 0 1920x1080x24";
      Restart = "always";
      RestartSec = "2s";
    };
  };

  systemd.services.x11vnc = {
    description = "x11vnc VNC server on display :99";
    wantedBy = [ "multi-user.target" ];
    after = [ "xvfb.service" ];
    requires = [ "xvfb.service" ];
    serviceConfig = {
      ExecStart = "${pkgs.x11vnc}/bin/x11vnc -display :99 -listen 127.0.0.1 -rfbport 5900 -forever -nopw";
      Restart = "always";
      RestartSec = "2s";
    };
  };

  # --- Init oneshot: prepare assets directory and .env from secrets ---

  systemd.services.openoutreach-init = {
    description = "OpenOutreach init: create dirs and copy secrets";
    wantedBy = [ "multi-user.target" ];
    after = [ "fix-operator-home.service" ];
    before = [ "podman-openoutreach.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = pkgs.writeShellScript "openoutreach-init" ''
        set -euo pipefail
        mkdir -p /home/operator/assets/data \
                 /home/operator/assets/cookies \
                 /home/operator/assets/models \
                 /home/operator/assets/diagnostics \
                 /home/operator/assets/templates/prompts \
                 /home/operator/containers/storage

        # Seed default prompt templates (volume mount shadows the image's
        # baked-in copies; migration 0005 reads followup2.j2 unconditionally)
        for f in followup2.j2 qualify_lead.j2 search_keywords.j2; do
          if [ ! -f "/home/operator/assets/templates/prompts/$f" ]; then
            touch "/home/operator/assets/templates/prompts/$f"
          fi
        done

        # Build .env from API key secret + hardcoded model config
        if [ -f /run/scape/secrets/OPENROUTER_API_KEY ]; then
          KEY=$(cat /run/scape/secrets/OPENROUTER_API_KEY)
          printf '%s\n' \
            "LLM_API_KEY=$KEY" \
            "AI_MODEL=minimax/minimax-m2.5" \
            "LLM_API_BASE=https://openrouter.ai/api/v1" \
            > /home/operator/assets/.env
          chmod 600 /home/operator/assets/.env
        fi

        chown -R operator:operator /home/operator/assets
      '';
    };
  };

  # --- OCI container: OpenOutreach daemon ---

  virtualisation.oci-containers.containers.openoutreach = {
    image = "ghcr.io/eracle/openoutreach:latest";
    extraOptions = [ "--network=host" ];
    environment = {
      DISPLAY = ":99";
    };
    volumes = [
      "/home/operator/assets:/app/assets"
      "/tmp/.X11-unix:/tmp/.X11-unix"
    ];
    # Skip the container's own Xvfb/x11vnc — we run them on the VM host
    cmd = [
      "bash" "-c"
      "python manage.py migrate --no-input && python manage.py setup_crm && python manage.py"
    ];
  };

  # Ensure the container starts after init and display services
  systemd.services.podman-openoutreach = {
    after = [ "openoutreach-init.service" "xvfb.service" "x11vnc.service" ];
    requires = [ "openoutreach-init.service" ];
  };

  # Host-level tools for debugging
  environment.systemPackages = with pkgs; [
    sqlite-interactive
  ];

  # Template metadata
  scape.template.openoutreach = {
    resources.memory = 8192;
    resources.cpu = 400;
    resources.disk = 20480;
    egress = "allow-all";
    secrets = [ "OPENROUTER_API_KEY" ];
    services = [
      { name = "admin"; port = 8000; path = "/"; type = "http"; }
    ];
  };
}
