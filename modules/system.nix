{inputs, ...}: {
  nix.global = [
    inputs.home-manager.nixosModules.home-manager
    ({
      lib,
      config,
      ...
    }: {
      # system options
      system.stateVersion = "24.05";
      networking = {
        useDHCP = lib.mkDefault true;
        networkmanager.enable = true;
        hostName = "goat";
      };
      # nix options
      nixpkgs = {
        hostPlatform = lib.mkDefault "x86_64-linux";
        config.allowUnfree = true;
      };
      nix.settings = {
        auto-optimise-store = true;
        experimental-features = ["nix-command" "flakes"];
      };

      # user
      users.users."goat" = {
        name = "goat";
        isNormalUser = true;
        extraGroups = [
          "wheel"
          "networkmanager"
          "audio"
          "video"
        ];
      };

      home-manager.users."goat" = {
        home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = "${config.users.users."goat".name}";
          homeDirectory = "/home/goat";
        };
        imports = config._module.args.homeMods;
      };
    })

    # language/time
    {
      i18n = {
        defaultLocale = "en_CA.UTF-8";
        supportedLocales = ["all"];
        inputMethod = {
          enable = true;
          type = "fcitx5";
          fcitx5.waylandFrontend = true;
        };
      };
      services.automatic-timezoned.enable = true;
    }
  ];
}
