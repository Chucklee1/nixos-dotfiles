{
  home = [
    {
      programs.kitty = {
        enable = true;
        settings = {
          cursor_shape = "beam";
          confirm_os_window_close = 0;
          tab_bar_edge = "bottom";
          tab_bar_style = "separator";
          tab_separator = "\"\"";
          tab_title_template = "\" {title} \"";
        };
      };
    }
    # qol aliases, maybe will ad more later
    {
      home.shellAliases = {
        kssh = "kitten ssh";
        kdiff = "kitten diff";
        kedit = "kitten edit-in-kitty";
      };
    }
  ];
}
