final: prev: with final; {
  emacs = emacsWithPackagesFromUsePackage {
    package = emacs;
    config = init.el;
    defaultInitFile = false;
    # make sure to include `(setq use-package-always-ensure t)` in config
    alwaysEnsure = true;
    # alwaysTangle = true;

    extraEmacsPackages = ep: with ep; [
      (trivialBuild {
        pname = "org-modern-indent";
        version = "main";
        src = inputs.org-modern-indent;
      })
      tree-sitter
      treesit-grammars.with-all-grammars
      eat
    ];
  };
}
