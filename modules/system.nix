{inputs, user, machine, ...}: {
  nix.global = [
    # system options
    ({config, ...}: {
      # the rest
      system.stateVersion = "24.05";
      networking.hostName = "${user}-${machine}";
      i18n.defaultLocale = "en_CA.UTF-8";
      time.timeZone = "America/Vancouver";

      # nix
      nixpkgs.config.allowUnfree = true;
      nix.settings = {
        auto-optimise-store = true;
        experimental-features = ["nix-command" "flakes"];
      };
    })

    # user
    inputs.home-manager.nixosModules.home-manager
    ({config, ...}: {
      users.users.main = {
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
          username = "${user}";
        };
        imports = config._module.args.homeMods;
      };
    })
  ];
  nix.nixos = [
    ({
      lib,
      config,
      ...
    }: {
      nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
      networking = {
        useDHCP = lib.mkDefault true;
        networkmanager.enable = true;
      };
      home-manager.users.main.home.homeDirectory = "/home/${users.users.main.name}";
    })
  ];
}
