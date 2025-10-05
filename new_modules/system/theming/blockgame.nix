{inputs, ...}: {
  nix = [
    # ---- grub theme ----
    inputs.minegrub-theme.nixosModules.default
    {
      stylix.targets.grub.enable = false;
      boot.loader.grub.minegrub-theme = {
        enable = true;
        splash = "100% Flakes!";
        background = "background_options/1.8  - [Classic Minecraft].png";
        boot-options-count = 4;
      };
    }
    # ---- sddm theme ----
    inputs.minesddm.nixosModules.default
    ({pkgs, ...}: {
      services.displayManager.sddm = {
        enable = true;
        package = pkgs.kdePackages.sddm;
        wayland.enable = true;
        theme = "minesddm";
      };
    })
  ];
}
