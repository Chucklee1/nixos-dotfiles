{
  nix = [
    {
      nixpkgs.overlays = [
        (_: prev: {
          lilypond = prev.lilypond.overrideAttrs (old: {
            # add lexical binding to safely shut emacs up
            postInstall =
              (old.postInstall or "")
              + ''
                for f in ${prev.lib.escapeShellArgs [
                  "lilypond-font-lock.el"
                  "lilypond-indent.el"
                  "lilypond-init.el"
                  "lilypond-mode.el"
                  "lilypond-what-beat.el"
                ]}; do
                  file="$out/share/emacs/site-lisp/$f"
                  grep -q 'lexical-binding' "$file" || \
                    sed -i '1s/$/ -*- lexical-binding: t; -*-/' "$file"
                done
              '';
          });
        })
      ];
    }
  ];

  home = [
    ({pkgs, ...}: {
      home.packages = [
        pkgs.gnuplot
        pkgs.lilypond-with-fonts
      ];
      programs.texlive = {
        enable = true;
        packageSet = pkgs.texlive.schemes.texliveFull;
      };
    })
  ];
}
