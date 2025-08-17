
{inputs, ...}: 

   let
    base = {nixpkgs.overlays = [inputs.nix-vim.overlays.default];};
  in {
  umbra.nix = [base ({pkgs, ...}: {environment.systemPackages = [pkgs.nixvim.core];})];
  metal.nix = [base ({pkgs, ...}: {environment.systemPackages = [pkgs.nixvim.full];})];
  macbook.nix = [base ({pkgs, ...}: {environment.systemPackages = [pkgs.nixvim.darwin];})];

}
