{
  plugins = {
    web-devicons.enable = true;
    trouble.enable = true;
    scrollview.enable = true;
    which-key.enable = true;
    # git
    gitsigns.enable = true;
    lazygit.enable = true;
    # sidetree
    neo-tree = {
      enable = true;
      sources = [
        "buffers"
        "git_status"
      ];
      buffers.window.mappings = {
        "." = "set_root";
        "k" = "navigate_up";
        "C" = "buffer_delete";
      };
    };
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
    # file explorer meets text editor
    oil = {
      enable = true;
      settings = {
        delete_to_trash = true;
        view_options.show_hidden = true;
      };
    };
    noice = {
      enable = true; # fancy cmd window
      settings.presets = {
        bottom_search = true; # use a classic bottom cmdline for search
        command_palette = true; # position the cmdline and popupmenu together
        long_message_to_split = true; #- long messages will be sent to a split
      };
    };
    # startup menu
    startify = {
      enable = true;
      settings = {
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
