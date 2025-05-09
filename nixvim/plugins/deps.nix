{pkgs, ...}: {
  extraPlugins = with pkgs.vimPlugins; [
    plenary-nvim
  ];
  extraPackages = with pkgs; [
    chafa
    imagemagick
    fd
    poppler_utils
    ffmpegthumbnailer
    epub-thumbnailer
    fontpreview
  ];
  plugins.web-devicons.enable = true;
}
