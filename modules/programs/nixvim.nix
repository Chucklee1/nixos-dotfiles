{inputs, ...}: {
  global.nix = [{nixpkgs.overlays = [inputs.nix-vim.overlays.default];}];
  global.home = [({pkgs, ...}: {home.packages = [pkgs.nixvim.full];})];
}
