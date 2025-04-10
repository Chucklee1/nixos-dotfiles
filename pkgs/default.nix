pkgs: {
  onetagger = pkgs.callPackage ./onetagger.nix {inherit pkgs;};
}
