{inputs, ...}: {
  nix.global = [
    # host option
    ({lib, ...}: {
      options.host = {
        machine = lib.mkoption {type = lib.types.string;};
        user = lib.mkoption {type = lib.types.string;};
      };
    })
    ({
      lib,
      config,
      ...
    }: {
      config =
        (lib.mkIf config.host.machine != "darwin")
        {
          nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
          networking = {
            useDHCP = lib.mkDefault true;
            networkmanager.enable = true;
          };
          home-manager.users.main.home.homeDirectory = "/home/${config.host.user}";
        }
        // (lib.mkIf config.host.machine == "darwin")
        {
          nixpkgs.hostPlatform = lib.mkDefault "x86_64-darwin";
          home-manager.users.main.home.homeDirectory = "/users/${config.host.user}";
        };
    })
    # system options
    ({
      lib,
      config,
      ...
    }: {
      # custom option
      host.user = "goat";
      # the rest
      system.stateVersion = "24.05";
      networking.hostName = "${config.host.user}-${config.host.machine}";
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
        name = config.host.user;
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
          username = "${config.host.user}";
        };
        imports = config._module.args.homeMods;
      };
    })
  ];
}
