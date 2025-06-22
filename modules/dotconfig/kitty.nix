{
  home.global = [
    ({config, ...}:
      with config.lib.stylix.colors.withHashtag; {
        home.file.".config/kitty/kitty.conf".text = ''
          font_family JetBrainsMono Nerd Font Mono
          font_size 12
          cursor_shape beam

          # Shell integration is sourced and configured manually
          shell_integration no-rc

          background_opacity 0.8
          background_blur 40
          confirm_os_window_close 0
          tab_bar_edge bottom
          tab_bar_style powerline
          tab_powerline_style round

          # The basic colors
          background ${base00}
          foreground ${base05}
          selection_background ${base03}
          selection_foreground ${base05}

          # Cursor colors
          cursor ${base05}
          cursor_text_color ${base00}

          # URL underline color when hovering with mouse
          url_color ${base04}

          # Kitty window border colors
          active_border_color ${base03}
          inactive_border_color ${base01}

          # OS Window titlebar colors
          wayland_titlebar_color ${base00}
          macos_titlebar_color ${base00}

          # Tab bar colors
          active_tab_background ${base00}
          active_tab_foreground ${base05}
          inactive_tab_background ${base01}
          inactive_tab_foreground ${base04}
          tab_bar_background ${base01}

          # The 16 terminal colors
          # normal
          color0 ${base00}
          color1 ${base08}
          color2 ${base0B}
          color3 ${base0A}
          color4 ${base0D}
          color5 ${base0E}
          color6 ${base0C}
          color7 ${base05}

          # bright
          color8 ${base02}
          color9 ${base08}
          color10 ${base0B}
          color11 ${base0A}
          color12 ${base0D}
          color13 ${base0E}
          color14 ${base0C}
          color15 ${base07}

          # extended base16 colors
          color16 ${base09}
          color17 ${base0F}
          color18 ${base01}
          color19 ${base02}
          color20 ${base04}
          color21 ${base06}
        '';
      })
  ];
}
