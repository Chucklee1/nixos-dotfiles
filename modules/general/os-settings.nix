{
  lib,
  system,
  inputs,
  defaults,
  ...
}: {
  # -----------------------------------------------------------
  # boot loader
  # -----------------------------------------------------------
  boot.loader = {
    efi.canTouchEfiVariables = true;
    grub = {
      enable = true;
      efiSupport = true;
      device = "nodev";
    };
  };

  # -----------------------------------------------------------
  # system options
  # -----------------------------------------------------------

  networking = {
    hostName = "${defaults.username}";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };
  system.stateVersion = "24.05"; # DO NOT CHANGE

  time.timeZone = "${defaults.timeZone}";
  i18n.defaultLocale = "${defaults.locale}";
  console = {
    earlySetup = true;
    keyMap = "us";
  };

  # -----------------------------------------------------------
  # nix options
  # -----------------------------------------------------------
  nixpkgs = {
    hostPlatform = lib.mkDefault "${system}";
    config.allowUnfree = true;
  };
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
  };

  # -----------------------------------------------------------
  # system user declaration
  # -----------------------------------------------------------
  users.users.${defaults.username} = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };

  # -----------------------------------------------------------
  # home manager
  # -----------------------------------------------------------
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs;};
    users.${defaults.username} = {
      home.stateVersion = "24.05"; # DO NOT CHANGE
      home.username = "${defaults.username}";
      home.homeDirectory = "/home/${defaults.username}";
    };
  };
}
