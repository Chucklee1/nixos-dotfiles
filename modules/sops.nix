{inputs, ...}: {
  nix.global = [
    inputs.sops-nix.nixosModules.sops
    ({
      config,
      pkgs,
      ...
    }: {
      environment.systemPackages = with pkgs; [
        sops
        age
      ];

      sops = let
        secretFile = ../secrets.yaml;
        defUser = "${config.users.users."goat".name}";
      in {
        defaultSopsFile = secretFile;
        defaultSopsFormat = "yaml";
        age.keyFile = "home/${defUser}/.config/age/keys.txt";
        secrets = {
          last_fm_navidrome = {
            sopsFile = secretFile;
            owner = defUser;
          };
          spotify_navidrome = {
            sopsFile = secretFile;
            owner = defUser;
          };
        };
      };
    })
  ];
}
