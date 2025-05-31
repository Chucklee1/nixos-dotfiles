{
  plugins = {
    bufferline.enable = true;
    noice = {
      enable = true; # fancy cmd window
      settings = {
        notify.enabled = true;
        lsp.override = {
          "cmp.entry.get_documentation" = true;
          "vim.lsp.util.stylize_markdown" = true;
          "vim.lsp.util.convert_input_to_markdown_lines" = true;
        };
        presets = {
          bottom_search = true; # use a classic bottom cmdline for search
          command_palette = true; # position the cmdline and popupmenu together
          long_message_to_split = true; #- long messages will be sent to a split
          inc_rename = false; #- enables an input dialog for inc-rename.nvim
          lsp_doc_border = false; # add a border to hover docs and signature help
        };
      };
    };
    scrollview.enable = true;
    which-key.enable = true;
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
        ];
      };
    };
    # git
    gitsigns.enable = true;
    lazygit.enable = true;
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
    # bundle
    snacks = {
      enable = true;
      settings = {
        indent.enabled = true;
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
  };
}
