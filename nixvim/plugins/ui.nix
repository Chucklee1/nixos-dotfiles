{
  plugins = {
    bufferline.enable = true;
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
    noice.enable = true; # fancy cmd window
    scrollview.enable = true;
    # file explorer meets text editor
    oil = {
      enable = true;
      settings = {
        delete_to_trash = true;
        view_options.show_hidden = true;
      };
    };
    # git
    lazygit = {
      enable = true;
      lazyLoad = {
        settings = {
          cmd = "LazyGit";
          keys = ["<leader>gg"];
        };
      };
    };
    gitsigns.enable = true;
  };
}
