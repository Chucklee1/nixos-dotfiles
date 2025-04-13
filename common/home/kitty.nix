{lib, ...}: {
  programs.kitty = {
    enable = true;
    settings = {
      confirm_os_window_close = 0;
      hide_window_decorations = true;
      tab_bar_edge = "top";
      tab_bar_style = lib.mkForce "slant";
    };
  };
}
