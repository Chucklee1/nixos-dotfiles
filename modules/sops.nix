{inputs, ...}: {
  nix.global = [
    inputs.sops-nix.nixosModules.sops
  ];
}
