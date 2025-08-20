{self, ...}: {
  emacs = {
    nix = [
      # overlay
      ({pkgs, ...}: {
        nixpkgs.overlays = [
          (import self.inputs.emacs-overlay)
          (self: super: {
            emacs-plus = pkgs.emacs-pgtk.overrideAttrs (old: {
              #patches =
              #(old.patches or [])
              #++ [
              ## Fix OS window role (yabai support)
              #(super.fetchpatch {
              #url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
              #sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
              #})
              #
              ## Rounded undecorated frame
              #(super.fetchpatch {
              #url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
              #sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
              #})
              #
              ## Dark/light mode awareness
              #(super.fetchpatch {
              #url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
              #sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
              #})
              #];
            });
          })
        ];
      })
      {services.emacs.enable = true;}
    ];
    home = [
      ({pkgs, ...}: {
        programs.emacs = {
          enable = true;
          package = pkgs.emacs-plus;
          extraPackages = epkgs:
            with pkgs; [
              # vim clone
              epkgs.evil
              epkgs.auctex
              # lsps/other-related-ish
              nixd
              alejandra
              haskell-language-server
              lemminx # xml lsp
              stylua
              vscode-langservers-extracted # soyjack lsps
            ];
          extraConfig =
            #lisp
            ''
              ;; Enable Evil
              (require 'evil)
              (evil-mode 1)

              ;; setup latex
              (setq TeX-source-correlate-method 'synctex)
              (setq TeX-source-correlate-start-server t)

              (use-package company
                :config
                (global-company-mode 1))
            '';
        };
      })
    ];
  };
}
