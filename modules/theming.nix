{
  inputs,
  self,
  ...
}:
with inputs; {
  nix.global = [
    stylix.nixosModules.stylix
    ({pkgs, ...}: {
      stylix = {
        enable = true;
        autoEnable = true;
        homeManagerIntegration.autoImport = true;
        image = "${self}/assets/wallpapers/wallpaper.png";
        base16Scheme = "${pkgs.base16-schemes}/share/themes/classic-dark.yaml";
        polarity = "dark";

        cursor = {
          package = pkgs.bibata-cursors;
          name = "Bibata-Modern-Classic";
          size = 24;
        };

        fonts = {
          monospace.package = pkgs.nerd-fonts.jetbrains-mono;
          monospace.name = "JetBrainsMono Nerd Font Mono";
          sansSerif.package = pkgs.noto-fonts-cjk-sans;
          sansSerif.name = "Noto Sans CJK";
          serif.package = pkgs.noto-fonts-cjk-serif;
          serif.name = "Noto Serif CJK";

          sizes = {
            applications = 12;
            terminal = 12;
            desktop = 11;
            popups = 12;
          };
        };
      };
    })
    # ---- boot minecraft theme... hehehe ----
    minegrub-theme.nixosModules.default
    minesddm.nixosModules.default
    minecraft-plymouth.default
    ({pkgs, ...}: {
      nixpkgs.overlays = [(_: _: {minecraft-plymouth = inputs.minecraft-plymouth.defaultPackage.x86_64-linux;})];
      stylix.targets.plymouth.enable = false;
      boot = {
        # grub
        loader.grub.minegrub-theme = {
          enable = true;
          splash = "100% Flakes!";
          background = "background_options/1.8  - [Classic Minecraft].png";
          boot-options-count = 4;
        };
        # loading screen
        plymouth = {
          enable = true;
          themePackages = [pkgs.minecraft-plymouth];
        };
        consoleLogLevel = 3;
        initrd.verbose = false;
        initrd.systemd.enable = true;
        kernelParams = [
          "quiet"
          "splash"
          "boot.shell_on_fail"
          "udev.log_priority=3"
          "rd.systemd.show_status=auto"
        ];
        loader.timeout = 0;
      };
      services.displayManager.sddm = {
        enable = true;
        wayland.enable = true;
        theme = "minesddm";
      };
    })
  ];

  home.global = [
    ({pkgs, ...}: {
      stylix = {
        iconTheme = {
          enable = true;
          package = pkgs.papirus-icon-theme;
          dark = "Papirus-Dark";
        };
      };
      gtk.enable = true;
      qt.enable = true;
    })
  ];
}
