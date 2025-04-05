{inputs, ...}: {
  nix.global = [
    # system
    ({lib, ...}: {
      system.stateVersion = "24.05";
      networking = {
        useDHCP = lib.mkDefault true;
        networkmanager.enable = true;
        hostName = "goat";
      };
      i18n.defaultLocale = "en_CA.UTF-8";

      # nix
      nixpkgs = {
        hostPlatform = lib.mkDefault "x86_64-linux";
        config.allowUnfree = true;
      };
      nix.settings = {
        auto-optimise-store = true;
        experimental-features = ["nix-command" "flakes"];
      };
    })

    # user
    inputs.home-manager.nixosModules.home-manager
    ({config, ...}: {
      users.users.main = {
        name = "goat";
        isNormalUser = true;
        extraGroups = [
          "wheel"
          "networkmanager"
          "audio"
          "video"
        ];
      };

      home-manager.users.main = {
        home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = "${config.users.users.main.name}";
          homeDirectory = "/home/goat";
        };
        imports = config._module.args.homeMods;
      };
    })
  ];
}
