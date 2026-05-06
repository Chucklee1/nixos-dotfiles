{
  plugins = {
    trouble.enable = true;
    web-devicons.enable = true;
    which-key.enable = true;
    oil.enable = true; # file managers
    # git
    gitsigns.enable = true;
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
      };
    };
    # startup menu
    startify = {
      enable = true;
      settings = {
        # essential part of the entire config everything will break without it
        custom_header = [
          " An idiot admires complexity, a genius admires simplicity. "
          " - Terry A. Davis          "
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
          "build"
          "result"
        ];
      };
    };
    # lualine
    lualine = {
      enable = true;
      settings = {
        options = {
          component_separators = "";
          section_separators = "";
        };
      };
    };
  };
  # don't setup nerd fonts if do not have and if terminal is tty
  extraConfigLuaPre = ''
    if vim.g.have_nerd_font and os.getenv("TERM") ~= "linux" then
      require('nvim-web-devicons').setup {}
    end
  '';
}
