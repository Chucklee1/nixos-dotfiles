{
  inputs,
  pkgs,
  ...
}: {
  imports = [
    ./nixos/software.nix
    ./nixos/infastructure.nix
    ./nixos/theming.nix
    ./nixos/gamse.nix
    ./nixos/nvidia.nix
    ./niri/niri.nix
  ];
  # -----------------------------------------------------------
  # boot loader
  # -----------------------------------------------------------
  boot.loader = {
    efi.efiSysMountPoint = "/boot/efi";
    grub = {
      enable = true;
      efiSupport = true;
      efiInstallAsRemovable = true;
      device = "nodev";
    };
  };

  # -----------------------------------------------------------
  # nix options
  # -----------------------------------------------------------
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

  # -----------------------------------------------------------
  # home manager
  # -----------------------------------------------------------
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs;};
    users.goat = {
      imports = [
        ./home/software.nix
        ./home/theming.nix
      ];
      home = {
        stateVersion = "24.05"; # DO NOT CHANGE
        username = "goat";
        homeDirectory = "/home/goat";
      };
    };
  };
}
