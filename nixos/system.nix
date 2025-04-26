{
  inputs,
  user,
  ...
}: {
  nix.global = [
    inputs.home-manager.nixosModules.home-manager
    # system
    ({
      lib,
      config,
      ...
    }: {
      system.stateVersion = "24.05";
      networking = {
        useDHCP = lib.mkDefault true;
        networkmanager.enable = true;
      };
      i18n.defaultLocale = "en_CA.UTF-8";
      time.timeZone = "America/Vancouver";

      # nix
      nixpkgs = {
        hostPlatform = lib.mkDefault "x86_64-linux";
        config.allowUnfree = true;
      };
      nix.settings = {
        auto-optimise-store = true;
        experimental-features = ["nix-command" "flakes"];
      };

      # user
      users.users.${user} = {
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

      home-manager.users.${user} = {
        nixpkgs.config.allowUnfree = true;
        home = {
          stateVersion = "24.05"; # DO NOT CHANGE
          username = "${config.users.users.${user}.name}";
          homeDirectory = "/home/${config.users.users.${user}.name}";
        };
        imports = config._module.args.homeMods;
      };
    })
  ];
}
