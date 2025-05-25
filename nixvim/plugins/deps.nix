{
  lib,
  pkgs,
  ...
}: {
  dependencies =
    lib.genAttrs
    [
      "fzf"
      "lazygit"
      "tree-sitter"
    ]
    (_: {enable = true;});
  extraPlugins = with pkgs.vimPlugins; [
    plenary-nvim
  ];
  extraPackages = with pkgs; [
    fd
    stylua
    alejandra
  ];
  plugins.web-devicons.enable = true;
  extraConfigLuaPre = ''
    if vim.g.have_nerd_font then
      require('nvim-web-devicons').setup {}
    end
  '';
}
