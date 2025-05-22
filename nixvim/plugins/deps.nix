{
  lib,
  pkgs,
  ...
}: {
  dependencies =
    lib.genAttrs
    [
      "chafa"
      "epub-thumbnailer"
      "ffmpegthumbnailer"
      "fzf"
      "lazygit"
      "poppler-utils"
      "tree-sitter"
    ]
    (_: {enable = true;});
  extraPlugins = with pkgs.vimPlugins; [
    plenary-nvim
  ];
  extraPackages = with pkgs; [
    fd
    mermaid-cli
    python312Packages.pylatexenc # latex2text
    tectonic
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
