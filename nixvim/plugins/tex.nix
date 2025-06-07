{pkgs, ...}: {
  plugins = {
    vimtex = {
      enable = true;
      texlivePackage = pkgs.texlive.combined.scheme-full;
      zathuraPackage = pkgs.zathura;
      settings = {
        view_method = "zathura";
        # quiet log
        quickfix_ignore_filters = ["error"];
        quickfix_open_on_warning = 0;
        # compiler
        compiler_latexmk = {
          aux_dir = ".build";
          options = [
            "-pdf"
            "-file-line-error"
            "-synctex=1"
          ];
        };
        # formatter
        indent_latexindent_options = ''-y=${pkgs.writeText "latexindent.yaml" "defaultIndent: '    '"}'';
      };
    };
  };
}
