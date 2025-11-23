final: prev:
# function from emacs-overlay, make sure to import the overlay
builtins.mapAttrs (_: package:
  prev.emacsWithPackagesFromUsePackage {
    inherit package;
    config = ./config.el;
    defaultInitFile = false;
    # make sure to include `(setq use-package-always-ensure t)` in config
    alwaysEnsure = true;
    # alwaysTangle = true;

    extraEmacsPackages = epkgs: [
      epkgs.tree-sitter
      epkgs.treesit-grammars.with-all-grammars
    ];
  })
  {
    emacs = prev.emacs;
    emacs-macport = prev.emacs-macport;
    emacs-pgtk = prev.emacs-pgtk;
  }
