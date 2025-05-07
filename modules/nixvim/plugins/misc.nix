{
  home.global = [
    {
      programs.nixvim = {
        # qol plugins
        plugins = {
          intellitab.enable = true;
          colorizer = {
            enable = true;
            settings.user_default_options.names = false;
          };
        };

        # language specific
        plugins = {
          nix.enable = true;
          render-markdown.enable = true;
        };

        # lua cfg
        extraConfigLuaPre = ''
          if vim.g.have_nerd_font then
            require('nvim-web-devicons').setup {}
          end
        '';
        extraConfigLua = ''
          require("telescope").load_extension("lazygit")
        '';
      };
    }
  ];
}
