{inputs, ...}: {
  nix.global = [
    # system
    ({lib, ...}: {
      system.stateVersion = "24.05";
      networking = {
        useDHCP = lib.mkDefault true;
        networkmanager.enable = true;
      };
      i18n.defaultLocale = "en_CA.UTF-8";
      # auto-timezone:
      services.automatic-timezoned.enable = true;

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
          "libvirtd"
        ];
      };

      home-manager.users.main = {
        home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = "${config.users.users.main.name}";
          homeDirectory = "/home/${config.users.users.main.name}";
        };
        imports = config._module.args.homeMods;
      };
    })
  ];
  nix.desktop = [({config, ...}: {networking.hostName = "${config.users.users.main.name}-desktop";})];
  nix.laptop = [
    ({config, ...}: {
      users.users.main.initialPassword = "1";
      networking.hostName = "${config.users.users.main.name}-laptop";
    })
  ];
}
