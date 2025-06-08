{pkgs, ...}: let
  latexmkrc =
    #sh
    ''
      $pdf_mode = 4; # 1 = pdflatex, 4 = lualatex
      set_tex_cmds('--shell-escape %O %S');
      $jobs = 4; # parallel cpu jobs
      $clean_ext .= ' acr acn alg glo gls glg ist';

      # glossary building
      add_cus_dep('acn','acr',0,'makeglossaries');
      add_cus_dep('glo','gls',0,'makeglossaries');
      sub makeglossaries {
        my ($base, $path) = fileparse($_[0]);
        return system 'makeglossaries', "-d", $path, $base;
      }
    '';
in {
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
        # QOL
        format_enabled = 1;
        # compiler
        compiler_latexmk = {
          aux_dir = ".build";
          options = [
            "-pdf"
            "-file-line-error"
            "-synctex=1"
            "-interaction=nonstopmode"
            "-shell-escape"
            "-r"
            ''${pkgs.writeText "latexindent.yaml" "${latexmkrc}"}''
          ];
        };
      };
    };
  };
}
