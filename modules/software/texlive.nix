{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = [
        (pkgs.texlive.schemes.texliveFull.withPackages
          (ps:
            with ps; [
              # for fonts like mathscr
              mathalpha
            ]))
        pkgs.gnuplot # can be used in org/latex docs
        pkgs.lilypond # latex but sheet music
      ];
    })
  ];
}
