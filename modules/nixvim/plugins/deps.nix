{
  home.global = [
    ({pkgs, ...}: {
      programs.nixvim = {
        extraPlugins = with pkgs.vimPlugins; [
          plenary-nvim
        ];
        extraPackages = with pkgs; [
          chafa
          ImageMagick
          fd
          poppler_utils
          ffmpegthumbnailer
          epub-thumbnailer
          fontpreview
        ];
        plugins.web-devicons.enable = true;
      };
    })
  ];
}
