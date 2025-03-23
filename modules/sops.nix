{inputs, ...}: {
  nix.global = [
    inputs.sops-nix.nixosModules.sops
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
        sops
        age
      ];

      sops = {
        defaultSopsFile = ../secrets.yaml;
        defaultSopsFormat = "yaml";
        age.keyFile = "home/goat/.config/age/keys.txt";
      };
    })
  ];
}
