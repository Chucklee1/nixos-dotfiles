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
    software.qt

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
    (import ../assets/disko/emphereal.nix {device = "/dev/nvme0n1";})
    ({lib, user, ...}: {
      boot.initrd = {
        enable = true;
        supportedFilesystems = ["nfs" "btrfs"];

        postResumeCommands = lib.mkAfter ''
          mkdir -p /mnt
          mount -o subvol=nixos/root /dev/nvme0n1p2 /mnt

          btrfs subvolume list -o /mnt/nixos/root |
          cut -f9 -d' ' |
          while read subvolume; do
              echo "deleting nixos/root$subvolume subvolume..."
              btrfs subvolume delete "/mnt/nixos/root/$subvolume"
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
        passwordAuthentication = false;
        challengeResponseAuthentication = false;
        extraConfig = lib.mkForce ''
          AllowTcpForwarding yes
          X11Forwarding no
          AllowAgentForwarding no
          AllowStreamLocalForwarding no
          AuthenticationMethods publickey
        '';
      };

    })
  ];
}
