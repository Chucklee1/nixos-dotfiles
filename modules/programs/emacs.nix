{inputs, self, ...}: {
  nix = [
    # overlay
    ({machine, ...}: {
      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        (final: prev: {
          emacs-final = final.emacsWithPackagesFromUsePackage {
            package =
              if (machine != "inspiron")
              then final.emacs-pgtk
              else final.emacs;

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
            ];
          };
        })
      ];
    })
    ({pkgs, ...}: {
      services.emacs.enable = true;
      services.emacs.package = pkgs.emacs-final;
    })
  ];
}
