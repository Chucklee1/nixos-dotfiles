{inputs, mod, ...}: with mod; {
  system = "x86_64-linux";
  builder = inputs.nixpkgs.lib.nixosSystem;
  user = "goat";
  modules = with mod; [
    net.nfs net.syncthing net.tailscale net.the-server

    programs.librewolf
    programs.nixvim
    programs.git programs.kitty programs.yazi
    programs.xserver programs.dwm

    software.apps software.dev software.qol

    system.boot system.home system.users
    system.pkgconfig system.sys-specs

    drivers.graphical drivers.ssh drivers.ext4

    shell.variables shell.zsh

    theming.blockgame theming.stylix theming.themes.nord
  ];


  extraConfig = [
    ({config, ...}: {
      fileSystems."/" = {
        device = "/dev/disk/by-uuid/a06136c2-90a0-445d-89ff-d93ea721f371";
        fsType = "ext4";
      };
      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/08F1-E578";
        fsType = "vfat";
        options = ["fmask=0022" "dmask=0022"];
      };

      swapDevices = [];

      boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_usb_sdmmc"];
      boot.kernelModules = ["kvm-intel"];
      hardware.cpu.intel.updateMicrocode = true;
      hardware.enableRedistributableFirmware = true;

      # startx for manual dwm startup, if needed
      services.xserver.displayManager.startx.enable = config.services.xserver.enable;

      # Ignore laptop lid state
      services.logind.settings.Login = {
        HandleLidSwitch = "ignore";
        HandleLidSwitchDocked = "ignore";
        HandleLidSwitchExternalPower = "ignore";
      };
    })
  ];
}
