{inputs, ...}: let
  base = {nixpkgs.overlays = [inputs.nix-vim.overlays.default];};
in {
  umbra.nix = [base ({pkgs, ...}: {environment.systemPackages = [pkgs.nixvim.core];})];
  additions.full.nix = [base ({pkgs, ...}: {environment.systemPackages = [pkgs.nixvim.full];})];
}
