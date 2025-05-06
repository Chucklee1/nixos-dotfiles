{
  home.global = [
    {
      programs.nixvim = {
        # qol plugins
        plugins = {
          intellitab.enable = true;
          telescope.enable = true;
          colorizer.enable = true;
          which-key.enable = true;
          wilder.enable = true;
        };

        # language specific
        plugins = {
          nix.enable = true;
          render-markdown.enable = true;
          fugitive.enable = true; # remote git acess
          gitsigns.enable = true;
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
