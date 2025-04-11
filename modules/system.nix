{inputs, host, ...}: {
  nix.global = [
    # system options
    ({config, ...}: {
      # the rest
      system.stateVersion = "24.05";
      networking.hostName = "${host.user}-${host.machine}";
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
        name = host.user;
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
          username = "${host.user}";
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
      home-manager.users.main.home.homeDirectory = "/home/${host.user}";
    })
  ];
  nix.darwin = [
    ({
      lib,
      config,
      ...
    }: {
      nixpkgs.hostPlatform = lib.mkDefault "x86_64-darwin";
      home-manager.users.main.home.homeDirectory = "/Users/${host.user}";
    })
  ];
}
