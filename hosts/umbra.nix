{
  inputs,
  mod,
  ...
}:
with mod; {
  system = "aarch64-darwin";
  builder = inputs.nix-darwin.lib.darwinSystem;
  user = "goat";
  modules = [
  ];
  extraConfig = [
    "${inputs.nixpkgs}/installer/cd-dvd/installation-cd-base.nix"
    # taken from graphical base
    ({pkgs, ...}: {
      # Whitelist wheel users to do anything
      # This is useful for things like pkexec
      #
      # WARNING: this is dangerous for systems
      # outside the installation-cd and shouldn't
      # be used anywhere else.
      security.polkit.extraConfig = ''
        polkit.addRule(function(action, subject) {
          if (subject.isInGroup("wheel")) {
            return polkit.Result.YES;
          }
        });
      '';

      services.xserver.enable = true;
      services.desktopManager.budgie.enable = true;

      services.displayManager.gdm = {
        enable = true;
        # autoSuspend makes the machine automatically suspend after inactivity.
        # It's possible someone could/try to ssh'd into the machine and obviously
        # have issues because it's inactive.
        # See:
        # * https://github.com/NixOS/nixpkgs/pull/63790
        # * https://gitlab.gnome.org/GNOME/gnome-control-center/issues/22
        autoSuspend = false;
      };

      services.displayManager.autoLogin = {
        enable = true;
        user = "nixos";
      };
      # there is no power management backend such as upower).
      powerManagement.enable = true;

      # VM guest additions to improve host-guest interaction
      services.spice-vdagentd.enable = true;
      services.qemuGuest.enable = true;
      # set true since this will only be for x86 systems
      virtualisation.vmware.guest.enable = true;
      services.xe-guest-utilities.enable = true;
      # The VirtualBox guest additions rely on an out-of-tree kernel module
      # which lags behind kernel releases, potentially causing broken builds.
      virtualisation.virtualbox.guest.enable = false;

      environment.defaultPackages = with pkgs; [
        vim
        ed
        qutebrowser
        mesa-demos
      ];
    })
  ];
}
