{
  pkgs,
  config,
  inputs,
  ...
}: {
  # -----------------------------------------------------------
  # boot loader & boot options
  # -----------------------------------------------------------
  boot.loader = {
    grub = {
      enable = true;
      efiSupport = true;
      device = "nodev";
    };
    grub2-theme = {
      enable = true;
      theme = "stylish";
      footer = true;
    };
  };
  stylix.targets.grub.enable = false;

  # nix
  nixpkgs.config.allowUnfree = true;
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
  };

  # -----------------------------------------------------------
  # system specifics
  # -----------------------------------------------------------
  system.stateVersion = "24.05"; # DO NOT CHANGE
  networking.hostName = "goat";
  networking.networkmanager.enable = true;
  i18n.defaultLocale = "en_CA.UTF-8";
  services.automatic-timezoned.enable = true;
  # timedatectl list-timezones
  # sudo timedatectl set-timezone <timezone> (no "")
  console = {
    earlySetup = true;
    keyMap = "us";
  };

  # -----------------------------------------------------------
  # user
  # -----------------------------------------------------------
  users.users.goat = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };
}
