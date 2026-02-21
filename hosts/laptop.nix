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
    (import "${self}/assets/disk/emphereal.nix" {device = "/dev/nvme0n1";})
    {
      boot.loader.efi.efiSysMountPoint = "/boot/efi";
      fileSystems."/persist".neededForBoot = true;

      boot.loader.grub.useOSProber = true;
      boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm-intel"];
      boot.supportedFilesystems = ["ntfs" "btrfs"];
      hardware.cpu.intel.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;
    }
  ];
}
