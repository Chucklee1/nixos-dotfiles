{pkgs, ...}: {
  extraPlugins = with pkgs.vimPlugins; [
    plenary-nvim
  ];
  extraPackages = with pkgs; [
    # needed
    tree-sitter
    fzf
    # qol
    fd
    poppler-utils
    # formatters
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
