
{self, inputs, ...}: {
  nix = [
    inputs.disko.nixosModules.default
    (import "${self}/assets/disko/ext4.nix" {device = "/dev/nvme0n1";})
    {
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
    }
  ];

  home = [{
    programs.niri.settings = {
      outputs."Virtual-1" = {
        variable-refresh-rate = true;
        scale = 1.8;
      };
    };
  }];
}
