{
  emacsWithPackagesFromUsePackage,
  emacsPackage ? null,
  ewmPackage ? null,
  tree-sitter-grammars,
  ...
}: let
  # when not null
  whenn = attr: list:
    if attr != null
    then list
    else [];
in
  emacsWithPackagesFromUsePackage {
    package = emacsPackage;
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
      ++ (whenn ewmPackage [ewmPackage]);
  }
