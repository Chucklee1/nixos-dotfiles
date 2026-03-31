{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = [
        # can be used in org/latex docs
        pkgs.gnuplot
        pkgs.texlive.combined.scheme-full
      ];
    })
  ];
}
