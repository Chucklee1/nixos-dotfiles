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
      epkgs.treesit-auto
      epkgs.nov
      (epkgs.trivialBuild {
        pname = "qml-ts-mode";
        version = "master";
        src = prev.fetchFromGitHub {
          owner = "xhcoding";
          repo = "qml-ts-mode";
          rev = "b80c6663521b4d0083e416e6712ebc02d37b7aec";
          sha256 = "WXK/CdFF9E2kG+uIios4HtKcEMhILS9MddJfVDeRLh0=";
        };
      })
    ];
  })
  {
    emacs = prev.emacs;
    emacs-macport = prev.emacs-macport;
    emacs-pgtk = prev.emacs-pgtk;
  }
