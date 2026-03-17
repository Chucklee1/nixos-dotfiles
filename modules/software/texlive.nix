{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = [
        # can be used in org/latex docs
        pkgs.gnuplot
        (pkgs.texlive.combine {
          inherit
            (pkgs.texlive)
            # core
            scheme-small
            latex
            latexmk
            preview
            # font
            capt-of
            fontawesome5
            fontspec
            unicode-math
            microtype
            titlesec
            setspace
            parskip
            paracol
            # bibliography
            biblatex
            biber
            csquotes
            # image stuff
            dvisvgm
            dvipng
            # math
            amsmath
            amsfonts
            mathtools
            # plotting
            pgfplots
            asymptote
            # graphics
            graphics
            xcolor
            geometry
            wrapfig
            float
            caption
            # text candy
            ulem
            hyperref
            # lua
            luatex
            luacode
            lualatex-math
            luaotfload
            ;
        })
      ];
    })
  ];
}
