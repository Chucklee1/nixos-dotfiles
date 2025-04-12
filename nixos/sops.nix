{inputs, ...}: {
  nix.laptop = [
    inputs.sops-nix.nixosModules.sops
    {
      sops = {
        defaultSopsFile = ../secrets.yaml;
        defaultSopsFormat = "yaml";

        age.keyFile = "/persist/sops/age/keys.txt";
        secrets = {
          super-secret-password.neededForUsers = true;
        };
      };
    }
    ({config, ...}: {
      users.users.main = {
        hashedPasswordFile = config.sops.secrets.super-secret-password.path;
      };
      networking.hostName = "${config.users.users.main.name}-macbook";
    })
  ];
}
