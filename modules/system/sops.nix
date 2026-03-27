{inputs, ...}: {
  nix = [
    inputs.sops-nix.nixosModules.sops
    ({config, pkgs, user, ...}: {
      environment.systemPackages = [pkgs.sops];
      environment.variables.SOPS_AGE_KEY_FILE = config.sops.age.keyFile;
      sops.age.generateKeyFile = false;
      # note: must rebuild system for secrets.yaml changes to take affect
      sops.defaultSopsFile = ../../secrets.yaml;
      sops.defaultSopsFormat = "yaml";
      # must set option in hosts file
      # sops.age.keyFile = string;
      sops.secrets = {
        "gregtrain/goat".neededForUsers = true;
        "tokens/listenbrainz" = {
          owner = user;
        };
      };
    })
  ];
}
