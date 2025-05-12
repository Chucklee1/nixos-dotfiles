{inputs, ...}: {
  nix.global = [
    # grub
    {
      boot.loader = {
        efi.canTouchEfiVariables = true;
        grub = {
          enable = true;
          efiSupport = true;
          device = "nodev";
        };
      };
    }
    # boot theme
    inputs.minegrub-theme.nixosModules.default
    inputs.minesddm.nixosModules.default
    {
      boot.loader.grub.minegrub-theme = {
        enable = true;
        splash = "100% Flakes!";
        background = "background_options/1.8  - [Classic Minecraft].png";
        boot-options-count = 4;
      };
      services.displayManager.sddm = {
        enable = true;
        wayland.enable = true;
        theme = "minesddm";
      };
    }
  ];
}
