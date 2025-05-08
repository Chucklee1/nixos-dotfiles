{inputs, ...}: {
  home.global = [
    ({pkgs, ...}: {
      programs.nixvim = {
        extraPlugins = [
          (pkgs.vimUtils.buildVimPlugin {
            name = "nordic.nvim";
            src = inputs.nordic-nvim;
          })
        ];
        colorscheme = "nordic";
        extraConfigLuaPre = ''
          require('nordic').setup({
              on_highlight = function(highlights, palette)
              end,
          })
        '';
      };
    })
  ];
}
