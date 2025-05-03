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
    ({pkgs, ...}: {
      stylix.targets = {
        grub.enable = false;
      };
      # grub
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
