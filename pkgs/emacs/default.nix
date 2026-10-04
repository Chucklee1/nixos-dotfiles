{
  emacsWithPackagesFromUsePackage,
  emacs31-pwayl,
  ewm,
  tree-sitter-grammars,
  ...
}:
emacsWithPackagesFromUsePackage {
  package = emacs31-pwayl;
  config = ''
    ,${builtins.readFile ./config.el}
  '';
  defaultInitFile = false;
  # make sure to include `(setq use-package-always-ensure t)` in config
  alwaysEnsure = true;
  # alwaysTangle = true;

  extraEmacsPackages = epkgs: [
    ewm
    epkgs.treesit-grammars.with-all-grammars
    tree-sitter-grammars.tree-sitter-kdl
  ];
}
