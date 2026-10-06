{
  emacsWithPackagesFromUsePackage,
  emacs31-pwayl,
  ewmPackage ? null,
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

  extraEmacsPackages = epkgs:
    [
      epkgs.treesit-grammars.with-all-grammars
      tree-sitter-grammars.tree-sitter-kdl
    ]
    ++ (
      if (ewmPackage != null)
      then [ewmPackage]
      else []
    );
}
