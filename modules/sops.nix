{inputs, ...}: {
  nix.macbook = [
    inputs.sops-nix.nixosModules.sops
    {
      sops = {
        defaultSopsFile = ../secrets.yaml;
        defaultSopsFormat = "yaml";

        age.keyFile = "/home/goat/.ssh/sops.age.key.txt";
        secrets = {
          super-secret-password.neededForUsers = true;
        };
      };
    }
    ({config, ...}: {
      users.users.main = {
        initialPassword = "1";
        hashedPasswordFile = config.sops.secrets.super-secret-password.path;
      };
      networking.hostName = "${config.users.users.main.name}-macbook";
    })
  ];
}
