{
  emacs = {
    nix = [{services.emacs.enable = true;}];
    home = [
      {
        programs.emacs = {
          enable = true;
          extraPackages = epkgs: [epkgs.evil];
          extraConfig =
            #lisp
            ''
              ;; Enable Evil
              (require 'evil)
              (evil-mode 1)
            '';
        };
      }
    ];
  };
}
