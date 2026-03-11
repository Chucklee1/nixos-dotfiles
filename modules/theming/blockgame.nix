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
    # ---- plymouth theme ----
    inputs.minecraft-plymouth-theme.nixosModules.default
    {
      stylix.targets.plymouth.enable = false;
      boot = {
        plymouth.enable = true;
        plymouth.plymouth-minecraft-theme.enable = true;
        # quiet boot
        consoleLogLevel = 3;
        initrd.verbose = false;
        kernelParams = [
          "quiet"
          "udev.log_level=3"
          "systemd.show_status=auto"
        ];
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
