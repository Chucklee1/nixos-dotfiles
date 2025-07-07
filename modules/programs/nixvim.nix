{inputs, ...}: {
  nix.desktop = [{nixpkgs.overlays = [inputs.nix-vim.overlays.default];}];
  home.desktop = [({pkgs, ...}: {home.packages = [pkgs.nixvim.full];})];
}
