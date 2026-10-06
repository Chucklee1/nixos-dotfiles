{
  self,
  inputs,
  ...
}: {
  nix = [
    inputs.ewm.nixosModules.default
    ({
      config,
      pkgs,
      ...
    }: {
      nixpkgs.overlays = [
        (import self.inputs.emacs-overlay)
        inputs.ewm.overlays.default # for emacs31-pwayl
      ];
      programs.ewm = {
        enable = true;
        emacsPackage = pkgs.callPackage ../../pkgs/emacs {
          ewmPackage = config.programs.ewm.ewmPackage;
        };
      };
    })
    # shell integration, just fish for now...
    ({config, ...}: {
      programs.fish.shellInit = ''
        if test -z "$SSH_CLIENT" -a "$XDG_CURRENT_DESKTOP" = "ewm"
          source ${config.programs.ewm.ewmPackage}/etc/emacs-ewm.fish
        end
      '';
    })
  ];
}
