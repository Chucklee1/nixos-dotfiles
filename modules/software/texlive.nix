{
  nix = [
    ({pkgs, ... }: {
      environment.systemPackages = [
        (pkgs.texlive.combine {
          inherit (pkgs.texlive)
            # core
            scheme-small latex latexmk preview
            # font
            fontawesome5 fontspec unicode-math
            microtype titlesec setspace parskip
            # bibliography
            biblatex biber csquotes
            # image stuff
            dvisvgm dvipng
            # math
            amsmath amsfonts mathtools
            # plotting
            pgfplots asymptote
            # graphics
            graphics xcolor geometry wrapfig float caption
            # text candy
            ulem hyperref
            # lua
            luatex luacode lualatex-math luaotfload;

            #(setq org-latex-compiler "lualatex")
            #(setq org-preview-latex-default-process 'dvisvgm)
        })
      ];
    })
  ];
}
