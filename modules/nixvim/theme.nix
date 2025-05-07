{inputs, ...}: {
  home.global = [
    ({pkgs, ...}: {
      stylix.targets.nixvim.enable = false;
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
