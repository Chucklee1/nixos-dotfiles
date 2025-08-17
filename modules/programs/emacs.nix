{
  emacs = {
    nix = [{services.emacs.enable = true;}];
    home = [{programs.emacs.enable = true;}];
  };
}
