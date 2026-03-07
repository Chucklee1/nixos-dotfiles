final: prev:
# function from emacs-overlay, make sure to import the overlay
builtins.mapAttrs (name: package:
  prev.emacsWithPackagesFromUsePackage {
    inherit package;
    config = ",${builtins.readFile ./config.el}";
    defaultInitFile = false;
    # make sure to include `(setq use-package-always-ensure t)` in config
    alwaysEnsure = true;
    # alwaysTangle = true;

    extraEmacsPackages = epkgs:
      [
        epkgs.treesit-grammars.with-all-grammars
        prev.tree-sitter-grammars.tree-sitter-kdl
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
        (prev.tree-sitter.buildGrammar {
          language = "qmljs";
          version = "master";
          src = prev.fetchFromGitHub {
            owner = "yuja";
            repo = "tree-sitter-qmljs";
            rev = "0bec4359a7eb2f6c9220cd57372d87d236f66d59";
            sha256 = "tV4lipey+OAQwygRFp9lQAzgCNiZzSu7p3Mr6CCBH1g=";
          };
        })
      ]
      ++ prev.lib.optionals (name == "emacs-exwm") [epkgs.exwm];
  })
  {
    emacs = prev.emacs;
    emacs-exwm = prev.emacs-gtk;
    emacs-macport = prev.emacs-macport;
    emacs-pgtk = prev.emacs-pgtk;
  }
