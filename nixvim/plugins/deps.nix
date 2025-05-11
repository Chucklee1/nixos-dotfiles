{pkgs, ...}: {
  extraPlugins = with pkgs.vimPlugins; [
    plenary-nvim
  ];
  extraPackages = with pkgs; [
    chafa
    fd
    poppler_utils
    ffmpegthumbnailer
    #epub-thumbnailer
    #fontpreview
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
