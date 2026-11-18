{
  home = [
    ({pkgs, ...}: {
      programs.texlive = {
        enable = true;
        packageSet = pkgs.texlive.schemes.texliveFull;
      };
    })
  ];
}
