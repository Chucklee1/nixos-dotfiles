{
  home.global = [
    {
      programs.nixvim.plugins = {
        bufferline.enable = true;
        lualine.enable = true;
        noice.enable = true; # fancy cmd window
        scrollview.enable = true;
        # file explorer meets text editor
        oil = {
          enable = true;
          settings = {
            delete_to_trash = true;
            view_options.show_hidden = false;
          };
        };
        # highlighting like terms
        illuminate = {
          enable = true;
          underCursor = false;
          filetypesDenylist = [
            "Outline"
            "TelescopePrompt"
            "alpha"
            "harpoon"
            "reason"
          ];
        };
      };
    }
  ];
}
