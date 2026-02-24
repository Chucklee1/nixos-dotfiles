{
  inputs,
  mod,
  ...
}:
with mod; {
  system = "x86_64-linux";
  builder = inputs.nixpkgs.lib.nixosSystem;
  user = "goat";
  modules = with mod; [
    net.syncthing
    net.tailscale

    programs.zen-browser
    programs.emacs
    programs.git
    programs.kitty
    programs.niri
    programs.waybar

    software.dev
    software.qol
    software.texlive

    system.boot
    system.home
    system.users
    system.pkgconfig
    system.sys-specs

    drivers.graphical
    drivers.ssh

    shell.variables
    shell.fish

    theming.stylix
    theming.themes.nord
  ];

  extraConfig = [
    inputs.disko.nixosModules.default
    inputs.impermanence.nixosModules.impermanence
    (import ../assets/disko/emphereal.nix {device = "/dev/nvme0n1";})
    ({
      lib,
      config,
      user,
      pkgs,
      ...
    }: {
      boot.initrd = {
        enable = true;
        supportedFilesystems = ["nfs" "btrfs"];

        postResumeCommands = lib.mkAfter ''
          mkdir -p /mnt
          mount -o subvolid=5 /dev/nvme0n1p2 /mnt

          btrfs subvolume list -o /mnt/nixos/root |
          cut -f9 -d' ' |
          while read subvolume; do
              echo "deleting $subvolume subvolume..."
              btrfs subvolume delete "/mnt/$subvolume"
          done &&
          echo "deleting nixos/root subvolume..." &&
          btrfs subvolume delete /mnt/nixos/root

          echo "restoring blank nix/root subvolume..."
          btrfs subvolume snapshot /mnt/nixos/root_blank /mnt/nixos/root

          umount /mnt
        '';
      };

      fileSystems."/persist".neededForBoot = true;

      boot.loader.efi.efiSysMountPoint = "/boot/efi";
      boot.loader.grub.useOSProber = true;

      boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm-intel"];

      hardware.cpu.intel.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;

      users.mutableUsers = false;
      users.users.${user}.passwordFile = "/persist/passwords/goat";

      # just to make sure
      services.openssh = {
        settings.challengeResponseAuthentication = false;
        extraConfig = lib.mkForce ''
          AllowTcpForwarding yes
          X11Forwarding no
          AllowAgentForwarding no
          AllowStreamLocalForwarding no
          AuthenticationMethods publickey
        '';
      };
      environment.persistence."/persist" = {
        hideMounts = true;
        directories = [
          "/var/log"
          "/var/lib/bluetooth"
          "/var/lib/nixos"
          "/var/lib/systemd/coredump"
          "/var/lib/tailscale"
          "/etc/NetworkManager/system-connections"
        ];
        files = [
          "/etc/machine-id"
        ];
        users.${user} = {
          directories = [
            "Downloads"
            "Music"
            "Pictures"
            "Documents"
            "Videos"
            "Repos"
            {
              directory = ".ssh";
              mode = "0700";
            }
            {
              directory = ".local/share/keyrings";
              mode = "0700";
            }
            ".local/share/direnv"
            ".local/share/zoxide"
            ".cache/zen"
            ".zen"
          ];
        };
      };
      systemd.user.services.login-hook = let
          USER_HOME = config.users.users.${user}.home;
          loginhook = pkgs.writeShellScriptBin "login-hookd.sh" ''
            mkdir "${USER_HOME}/.emacs.d" &&
            ln -s ${USER_HOME}/nixos-dotfiles ${USER_HOME} &&
            ln -s "${USER_HOME}/nixos-dotfiles/pkgs/emacs/config.el" "${USER_HOME}/.emacs.d/init.el"
      '';

      in {
  enable = true;
  description = "setup symlinks from persistant mounts";
  wantedBy = [ "default.target" ];
  serviceConfig = {
      Type = "simple";
      ExecStart = "${loginhook}/bin/login-hook.sh";
  };
};
    })
  ];
}
