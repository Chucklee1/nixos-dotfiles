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
}
