{
  pkgs,
  lib,
  config,
  ...
}:
# Bootloader.
{
  # D O  N O T  C H A N G E
  system.stateVersion = "24.05";

  # boot loader
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    plymouth.enable = true;
  };

  # tty settings
  console.font = "Lat2-Terminus16";
  console.useXkbConfig = true;

  # nix
  nixpkgs.config.allowUnfree = true;
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
  };

  # idk
  networking.networkmanager.enable = true;
  time.timeZone = "America/Vancouver";
  i18n.defaultLocale = "en_CA.UTF-8";

  # system user config
  users.users = {
    goat = {
      isNormalUser = true;
      extraGroups = ["wheel" "networkmanager"];
    };
    caprine = {
      isNormalUser = true;
      extraGroups = ["wheel" "networkmanager"];
    };
  };
  # home manager user config
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "backup";
    users = {
      goat = {
        imports = [./home/default-home.nix];
        home.username = "goat";
        home.homeDirectory = "/home/goat";
      };
      caprine = {
        imports = [./home/default-home.nix];
        home.username = "caprine";
        home.homeDirectory = "/home/caprine";
      };
    };
  };
}
