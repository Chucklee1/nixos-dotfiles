
{
  nix = [
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

      # disko config option
      opts.disko.device = "/dev/nvme0n1";
    }
  ];

  home = [({config, ...}: {
    programs.niri.settings = {
      outputs."Virtual-1" = {
        variable-refresh-rate = true;
        scale = 1.8;
      };
      binds = {
        "F10".action =
          config.lib.niri.actions.spawn
            "sh" "-c" "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
        "Ctrl+P".action.screenshot = [];
      };
    };
  })];
}
