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
        # can be used in org/latex docs
        pkgs.gnuplot
      ];
    })
  ];
}
