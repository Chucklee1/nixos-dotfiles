{inputs, ...}: let
  mkKey = secret: {
    sops.secrets."${secret}" = {owner = "goat";};
  };
in {
  nix.global = [
    inputs.sops-nix.nixosModules.sops
    {
      sops = {
        defaultSopsFile = ../secrets.yaml;
        defaultSopsFormat = "yaml";
        age.keyFile = "home/goat/.config/sops/age/keys.txt";
      };
    }
    (mkKey "tailscale-auth-key")
    (mkKey "navi-lastfm-api-key")
    (mkKey "navi-lastfm-shared-secret")
    (mkKey "navi-spot-client-id")
    (mkKey "navi-spot-client-secret")
  ];
}
