{inputs, system, extlib, ...}: {
  nix = [
    (extlib.darwinOrLinux system
      inputs.stylix.darwinModules.stylix
      inputs.stylix.nixosModules.stylix
    )
    ({pkgs, ...}: let
      # helpers
      hideHashtag = set: builtins.mapAttrs (_: v: builtins.replaceStrings ["#"] [""] v) set;
      # themes
      nordic = hideHashtag {
        name = "nordic";
        author = "AlexvZyl";
        base00 = "#191D24";
        base01 = "#242933";
        base02 = "#3B4252";
        base03 = "#4C566A";
        base04 = "#D8DEE9";
        base05 = "#E5E9F0";
        base06 = "#ECEFF4";
        base07 = "#8FBCBB";
        base08 = "#BF616A";
        base09 = "#D08770";
        base0A = "#EBCB8B";
        base0B = "#A3BE8C";
        base0C = "#88C0D0";
        base0D = "#81A1C1";
        base0E = "#B48EAD";
        base0F = "#C0C8D8";
      };
      clasic-dark = hideHashtag {
        name = "Classic Dark";
        author = "Jason Heeris";
        base00 = "#151515";
        base01 = "#202020";
        base02 = "#303030";
        base03 = "#505050";
        base04 = "#B0B0B0";
        base05 = "#D0D0D0";
        base06 = "#E0E0E0";
        base07 = "#F5F5F5";
        base08 = "#AC4142";
        base09 = "#D28445";
        base0A = "#F4BF75";
        base0B = "#90A959";
        base0C = "#75B5AA";
        base0D = "#6A9FB5";
        base0E = "#AA759F";
        base0F = "#8F5536";
      };
      painted-lake = pkgs.fetchurl {
        url = "https://w.wallhaven.cc/full/5g/wallhaven-5g22q5.png";
        hash = "sha256-snqkeQecU0opsBfIrnkl6aiV71hSCmqnZBAsibNG4w8=";
      };
      pastel-waves = pkgs.fetchurl {
        url = "https://w.wallhaven.cc/full/r2/wallhaven-r2e391.png";
        hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
      };
      azure-ridge = pkgs.fetchurl {
        url = "https://w.wallhaven.cc/full/lm/wallhaven-lm7edq.png";
        hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
      };
    in {
      stylix = {
        enable = true;
        autoEnable = true;
        homeManagerIntegration.autoImport = true;
        image = painted-lake;
        base16Scheme = nordic;
        polarity = "dark";
        opacity.desktop = 0.8;
        opacity.terminal = 0.8;

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
    (extlib.darwinOrLinux system
      {}
      ({pkgs, ...}: {
        stylix.cursor = {
          package = pkgs.bibata-cursors;
          name = "Bibata-Modern-Classic";
          size = 24;
        };
      })
    )
  ];

  home = [
    (extlib.darwinOrLinux system
      {}
      ({pkgs, ...}: {
        stylix.iconTheme = {
          enable = true;
          package = pkgs.papirus-icon-theme;
          dark = "Papirus-Dark";
        };
        gtk.enable = true;
        qt.enable = true;
      })
    )
  ];
}
