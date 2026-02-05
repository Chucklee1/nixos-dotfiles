{
  self,
  inputs,
  extlib,
  ...
}: {
  nix = [
    (extlib.darwinOrLinux
    inputs.stylix.darwinModules.stylix
    inputs.stylix.nixosModules.stylix)
    ({pkgs, ...}: let
      # helpers
      painted-lake = pkgs.fetchurl {
        url = "https://w.wallhaven.cc/full/5g/wallhaven-5g22q5.png";
        hash = "sha256-snqkeQecU0opsBfIrnkl6aiV71hSCmqnZBAsibNG4w8=";
      };
    in {
      stylix = {
        enable = true;
        autoEnable = true;
        homeManagerIntegration.autoImport = true;
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
    (
      extlib.darwinOrLinux
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
    (
      extlib.darwinOrLinux
      {}
      ({pkgs, ...}: {
        stylix.icons = {
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
