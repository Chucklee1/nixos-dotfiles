{
  inputs,
  self,
  ...
}:
with inputs; {
  nix.global = [
    stylix.nixosModules.stylix
    ({pkgs, ...}: let
      classic-dark = {
        name = "Classic Dark";
        author = "Jason Heeris (http://heeris.id.au)";
        base00 = "151515";
        base01 = "202020";
        base02 = "303030";
        base03 = "505050";
        base04 = "B0B0B0";
        base05 = "D0D0D0";
        base06 = "E0E0E0";
        base07 = "F5F5F5";
        base08 = "AC4142";
        base09 = "D28445";
        base0A = "F4BF75";
        base0B = "90A959";
        base0C = "75B5AA";
        base0D = "6A9FB5";
        base0E = "AA759F";
        base0F = "8F5536";
      };
      nordic = {
        scheme = "Nordic";
        author = "goat, based on arcticicestudio";
        base00 = "191D24";
        base01 = "242933";
        base02 = "3B4252";
        base03 = "4C566A";
        base04 = "D8DEE9";
        base05 = "E5E9F0";
        base06 = "ECEFF4";
        base07 = "8FBCBB";
        base08 = "BF616A";
        base09 = "D08770";
        base0A = "EBCB8B";
        base0B = "A3BE8C";
        base0C = "88C0D0";
        base0D = "81A1C1";
        base0E = "B48EAD";
        base0F = "C0C8D8";
      };
    in {
      stylix = {
        enable = true;
        autoEnable = true;
        homeManagerIntegration.autoImport = true;
        image = "${self}/assets/wallpapers/wallpaper.png";
        base16Scheme = config.lib.base16.mkSchemeAttrs nordic;
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
