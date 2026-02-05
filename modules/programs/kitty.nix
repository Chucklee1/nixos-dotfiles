{
  home = [
    {
      programs.kitty = {
        enable = true;
        settings = {
          cursor_shape = "beam";
          background_blur = 40;
          confirm_os_window_close = 0;
          tab_bar_edge = "bottom";
          tab_bar_style = "powerline";
          tab_powerline_style = "round";
        };
      };
    }
  ];
}
