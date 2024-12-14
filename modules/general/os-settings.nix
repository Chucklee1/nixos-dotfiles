{
  lib,
  system,
  inputs,
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
    hostName = "goat";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };
  system.stateVersion = "24.05"; # DO NOT CHANGE

  time.timeZone = "America/Vancouver";
  i18n.defaultLocale = "en_CA.UTF-8";
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
  users.users.goat = {
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
    users.goat = {
      home.stateVersion = "24.05"; # DO NOT CHANGE
      home.username = "goat";
      home.homeDirectory = "/home/goat";
    };
  };
}
