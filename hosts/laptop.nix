{inputs, mod, ...}: with mod; {
  system = "x86_64-linux";
  builder = inputs.nixpkgs.lib.nixosSystem;
  user = "goat";
  modules = with mod; [
    net.syncthing net.tailscale

    programs.zen-browser programs.emacs
    programs.git programs.kitty programs.yazi
    software.wayland programs.niri programs.quickshell

    software.apps software.dev software.texlive
    software.qol

    system.boot system.home system.users
    system.pkgconfig system.sys-specs
    disko.disko disko.ext4

    drivers.graphical drivers.ssh

    shell.variables shell.zsh

    theming.blockgame theming.stylix theming.themes.nord
  ];

  extraConfig = [
    {
      boot.initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod"];
      boot.kernelModules = ["kvm-intel"];
      boot.supportedFilesystems = ["ntfs"];
      hardware.cpu.intel.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;

      # disko config option
      opts.disko.device = "/dev/nvme0n1";
    }
  ];
}
