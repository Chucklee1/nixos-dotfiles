{inputs, ...}: {
  nix.global = [
    inputs.sops-nix.nixosModules.sops
    {
      sops = {
        defaultSopsFile = ../secrets.yaml;
        defaultSopsFormat = "yaml";
        age.keyFile = "home/goat/.config/age/keys.txt";
      };
    }
  ];
}
