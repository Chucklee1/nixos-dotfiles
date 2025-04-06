{
  nix.global = [
    {
      boot = {
        loader.efi.canTouchEfiVariables = true;
        loader.grub = {
          enable = true;
          efiSupport = true;
          device = "nodev";
        };
      };

      # display manager
      services.displayManager.ly.enable = true;
    }
  ];
}
