{inputs, ...}: {
  nix.global = [
    inputs.sops-nix.nixosModules.sops
    ({config, ...}: {
      sops = {
        defaultSopsFile = ../secrets.yaml;
        defaultSopsFormat = "yaml";
        age.keyFile = "home/goat/.config/sops/age/keys.txt";
        secrets = {
          ip = {};
          navi-lastfm-api-key = {owner = "goat";};
          navi-lastfm-shared-secret = {owner = "goat";};
          navi-spot-client-id = {owner = "goat";};
          navi-spot-client-secret = {owner = "goat";};
        };
        templates = {
          "ip".content = ''${config.sops.placeholder.ip}'';
          "navi-lastfm-api-key".content = ''${config.sops.placeholder.navi-lastfm-api-key}'';
          "navi-lastfm-shared-secret".content = ''${config.sops.placeholder.navi-lastfm-shared-secret}'';
          "navi-spot-client-id".content = ''${config.sops.placeholder.navi-spot-client-id}'';
          "navi-spot-client-secret".content = ''${config.sops.placeholder.navi-spot-client-secret}'';
        };
      };
    })
  ];
}
