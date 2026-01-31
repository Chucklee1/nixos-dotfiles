{inputs, mod, ...}: with mod; {
  system = "aarch64-linux";
  builder = inputs.nixpkgs.lib.nixosSystem;
  user = "goat";
  modules = [
    net.syncthing net.tailscale

    programs.zen-browser  programs.emacs
    programs.git programs.kitty programs.yazi
    software.wayland programs.niri programs.quickshell

    software.dev software.qol
    software.texlive software.java software.rust

    system.boot system.home system.users
    system.pkgconfig system.sys-specs
    disko.disko disko.ext4

    drivers.graphical drivers.ssh drivers.ext4

    shell.variables shell.zsh shell.nushell

    theming.stylix theming.themes.nord
  ];
  extraConfig = [
    ({user, ...}: {
      boot.initrd.availableKernelModules = [ "ehci_pci" "ahci" "xhci_pci" "nvme" "usbhid" "sr_mod" ];
      # boot.initrd.kernelModules = [ ];
      # boot.kernelModules = [ ];
      # boot.extraModulePackages = [ ];

      # vm guest drivers
      virtualisation.vmware.guest.enable = true;
      services.xserver.videoDriver = "modesetting";
      services.udev.enable = true;
      # no proper displayManager, so I use startx
      services.xserver.displayManager.startx.enable = true;

      # disko config option
      opts.disko.device = "/dev/nvme0n1";

      home-manager.users.${user} = {
        programs.niri.settings = {
          outputs."Virtual-1".scale = 1.8;
        };
      };
    })
  ];
}
