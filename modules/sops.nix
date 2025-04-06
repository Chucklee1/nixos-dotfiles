{inputs, ...}: {
  nix.laptop = [
    inputs.sops-nix.nixosModules.sops
    {
      sops = {
        defaultSopsFile = ../secrets.yaml;
        defaultSopsFormat = "yaml";

        age.keyFile = "/nix/persist/var/lib/sops-nix/key.txt";
        secrets = {
          laptop.neededForUsers = true;
        };
      };
    }
  ];
}
