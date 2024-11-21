{pkgs, ...}: {
  # waybar
  home.packages = [(pkgs.nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})];
  stylix.targets.waybar.enable = false;
  programs.waybar.enable = true;
}
