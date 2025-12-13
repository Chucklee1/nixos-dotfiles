{
  plugins = {
    kitty-navigator.enable = true;
    presence.enable = true;
    scrollview.enable = true;
    trouble.enable = true;
    web-devicons.enable = true;
    which-key.enable = true;
    # git
    gitsigns.enable = true;
    lazygit.enable = true;
    # bundle
    snacks = {
      enable = true;
      settings = {
        bigfile.enabled = true;
        indent.enabled = true;
        notifier = {
          enabled = true;
          top_down = false;
        };
        quickfile.enabled = true;
        scroll.enabled = true;
        words.enabled = true;
      };
    };
    # startup menu
    startify = {
      enable = true;
      settings = {
        # essential part of the entire config everything will break without it
        custom_header = [
          "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⣀⣀⠀⠀⠀⠀⠀⠀ ⠀⠀⠀"
          "⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⣾⣿⠿⠿⠿⠿⠿⢿⣿⣷⣤⡀⠀⠀⠀⠀⠀"
          "⠀⠀⠀⠀⠀⠀⠀⣰⣿⠟⠁⠀⠀⣠⣴⣶⣶⣶⣶⣿⣿⣿⣆⠀⠀⠀⠀"
          "⠀⠀⠀⠀⠀⠀⢰⣿⡏⠀⠀⢠⣾⡿⠋⣉⣉⣉⡉⠉⣉⡻⣿⣦⠀⠀⠀"
          "⠀⠀⠀⣠⣶⣶⣾⣿⡇⠀⠀⣾⣿⠁⠀⠛⠛⠛⠃⠘⠛⠃⢸⣿⠄⠀⠀"
          "⠀⠀⢠⣿⡏⠀⢸⣿⡇⠀⠀⠸⣿⣧⡀⠀⠀⠀⠀⠀⠀⣠⣾⡿⠀⠀⠀"
          "⠀⠀⢸⣿⡇⠀⢸⣿⡇⠀⠀⠀⠈⠻⠿⣿⣿⣿⣿⣿⣿⢿⣿⡇⠀⠀⠀"
          "⠀⠀⢸⣿⡇⠀⢸⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⡇⠀⠀⠀"
          "⠀⠀⢸⣿⡇⠀⢸⣿⡇⠀⠀⠀⠀⢀⣀⡀⠀⠀⠀⠀⢀⣸⣿⡇⠀⠀⠀"
          "⠀⠀⠘⣿⣧⣀⣸⣿⡇⠀⠀⠀⠀⠻⠿⢿⣷⡄⠀⠘⠿⠿⠿⣿⣦⠀⠀"
          "⠀⠀⠀⠈⠻⠿⢿⣿⡇⠀⠀⠀⠀⠀⠀⠀⣿⡇⠀⠀⠀⠀⠀⢸⣿⡆⠀"
          "⠀⠀⠀⠀⠀⠀⠀⠻⣿⣦⣄⣀⣀⣀⣀⣠⣿⣷⣄⣀⣀⣀⣀⣼⣿⠁⠀"
          "⠀⠀⠀⠀⠀⠀⠀⠀⠈⠛⠛⠛⠛⠛⠛⠛⠋⠛⠛⠛⠛⠛⠛⠛⠁⠀⠀"
        ];

        change_to_dir = false;
        use_unicode = true;

        lists = [{type = "dir";}];
        files_number = 30;

        skiplist = [
          "flake.lock"
          ".build"
          "result"
        ];
      };
    };
    # lualine
    lualine = {
      enable = true;
      settings = {
        options = {
          component_separators = {
            left = "";
            right = "";
          };
          section_separators = {
            left = "";
            right = "";
          };
        };
      };
    };
  };
  extraConfigLuaPre = ''
    if vim.g.have_nerd_font then
      require('nvim-web-devicons').setup {}
    end
  '';
}
