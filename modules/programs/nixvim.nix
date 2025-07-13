{inputs, ...}: {
  nix.global = [{nixpkgs.overlays = [inputs.nix-vim.overlays.default];}];
  home.global = [({pkgs, ...}: {home.packages = [pkgs.nixvim.full];})];
}
