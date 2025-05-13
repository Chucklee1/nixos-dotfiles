{
  pkgs,
  inputs,
  ...
}: {
  extraPlugins = [
    (pkgs.vimUtils.buildVimPlugin {
      name = "nordic.nvim";
      src = inputs.nordic-nvim;
    })
  ];
  colorscheme = "nordic";
  extraConfigLuaPost = ''
    require('nordic').setup({
        on_palette = function(palette) end,
        after_palette = function(palette) end,
        on_highlight = function(highlights, palette) end,
        transparent = {
            bg = true,
      },
    })
    require('nordic').load()
  '';
}
