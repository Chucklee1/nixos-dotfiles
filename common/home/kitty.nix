{pkgs, ...}: {
  stylix.targets.kitty.enable = false;
  home.file.".config/kitty/kitty.conf" = {
    force = true;
    text = ''
      # GENERAL
      shell_integration no-rc
      confirm_os_window_close 0

      # STYLE - theme file
      ${pkgs.base16-schemes}/share/themes/classic-dark.conf

      # STYLE - font
      font_family JetBrainsMono Nerd Font Mono
      font_size 12

      # STYLE - ui
      background_opacity 1.000000
      hide_window_decorations yes
      tab_bar_edge top
      tab_bar_style slant
    '';
  };
}
