{pkgs, ...}: {
  plugins = {
    ltex-extra.enable = true;
    vimtex = {
      enable = true;
      texlivePackage = pkgs.texlive.combined.scheme-full;
      zathuraPackage = pkgs.zathura;
      settings = {
        quickfix_ignore_filters = ["error"];
        quickfix_open_on_warning = 0;
        view_method = "zathura";
        compiler_latexmk = {
          aux_dir = ".build";
          options = [
            "-pdf"
            "-file-line-error"
            "-synctex=1"
            "-interaction=nonstopmode"
          ];
        };
      };
    };
  };
}
