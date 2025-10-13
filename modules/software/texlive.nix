{
  nix = [
    ({pkgs, ... }: {
      environment.systemPackages = [
        (pkgs.texlive.combine {
          inherit (pkgs.texlive)
            # core
            scheme-small latexmk preview
            # font
            fontawesome5 fontspec unicode-math
            microtype titlesec setspace parskip
            # bibliography
            biblatex biber csquotes
            # image stuff
            dvisvgm dvipng
            # math
            amsmath amssymb mathtools
            # plotting
            tikz pgfplots asymptote
            # graphics
            graphicx xcolor geometry wrapfig float caption
            # text candy
            ulem hyperref
            # lua
            luatex lualatex luaotfload;

            #(setq org-latex-compiler "lualatex")
            #(setq org-preview-latex-default-process 'dvisvgm)
        })
      ];
    })
  ];
}
