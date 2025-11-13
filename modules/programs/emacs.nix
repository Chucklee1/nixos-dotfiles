{inputs, self, ...}: {
  nix = [
    # overlay
    {nixpkgs.overlays = [(import self.inputs.emacs-overlay)];}
    ({pkgs, machine, ...}: let
          emacsWithUsePackage = pkgs.emacsWithPackagesFromUsePackage {
            package =
              if (machine == "inspiron")
                then pkgs.emacs
              else if (machine == "macbook")
                then pkgs.emacs-macport
              else pkgs.emacs-pgtk;

            config = "${self}/assets/emacs/init.el";
            defaultInitFile = true;
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
    in {
      environment.systemPackages = [emacsWithUsePackage];
      services.emacs.enable = true;
      services.emacs.package = emacsWithUsePackage;
    })
  ];
}
