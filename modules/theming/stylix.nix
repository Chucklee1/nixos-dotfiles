/*
color reference
base00 #2e3440
base01 #3b4252
base02 #434c5e
base03 #4c566a
base04 #d8dee9
base05 #e5e9f0
base06 #eceff4
base07 #8fbcbb
base08 #bf616a
base09 #d08770
base0A #ebcb8b
base0B #a3be8c
base0C #88c0d0
base0D #81a1c1
base0E #b48ead
base0F #5e81ac
*/
{
  self,
  inputs,
  extlib,
  target,
  ...
}: {
  univ = [
    ({pkgs, ...}: {
      stylix = {
        enable = true;
        autoEnable = true;
        image = "${self}/assets/wallpaper/nordest.png";
        base16Scheme = "${pkgs.base16-schemes}/share/themes/nord.yaml";
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
        cursor = {
          package = pkgs.bibata-cursors;
          name = "Bibata-Modern-Classic";
          size = 24;
        };
      };
    })
  ];
  nix = [
    inputs.stylix.nixosModules.stylix
    {stylix.homeManagerIntegration.autoImport = true;}
  ];

  home =
    (extlib.homeOrNot target [inputs.stylix.homeModules.stylix])
    ++ [
      ({pkgs, ...}: {
        home.pointerCursor.enable = true;
        stylix.icons = {
          enable = true;
          package = pkgs.papirus-icon-theme;
          dark = "Papirus-Dark";
        };
        gtk.enable = true;
        qt.enable = true;
      })
    ];
}
