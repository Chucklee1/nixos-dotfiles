{self, ...}: {
  nix = [
    # overlay
    ({pkgs, machine, ...}: let
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
      emacs-pkg =
        if pkgs.stdenv.isDarwin then pkgs.emacs-macport
        else if machine == "umbra" then pkgs.emacs
        else pkgs.emacs-pgtk;
    in {
      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        self.overlays.emacs
      ];
      # got to restate for mac to actually install emacs...
      environment.systemPackages = [emacs-pkg];
      services.emacs.enable = true;
      services.emacs.package = emacs-pkg;
    })
  ];
}
