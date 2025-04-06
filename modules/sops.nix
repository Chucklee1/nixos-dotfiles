{inputs, ...}: {
  nix.laptop = [
    inputs.sops-nix.nixosModules.sops
    {
      sops.defaultSopsFile = ./secrets/secrets.yaml;
      sops.defaultSopsFormat = "yaml";

      sops.age.keyFile = "/home/goat/.config/sops/age/keys.txt";
    }
  ];
}
