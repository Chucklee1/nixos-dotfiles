{
  inputs,
  pkgs,
  ...
}: {
  imports = [
    ./software.nix
    ./infastructure.nix
    ./theming.nix
    ./gamse.nix
    ./nvidia.nix
    ./i3.nix
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
  nixpkgs.overlays = [inputs.niri.overlays.niri];

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
        ./home/niri/config.kdl.nix
        ./home/wlogout/wlogout.nix
      ];
      home = {
        stateVersion = "24.05"; # DO NOT CHANGE
        username = "goat";
        homeDirectory = "/home/goat";
      };
    };
  };
}
