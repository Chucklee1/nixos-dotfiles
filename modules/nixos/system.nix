{
  pkgs,
  config,
  inputs,
  ...
}: {
  # -----------------------------------------------------------
  # boot loader & boot options
  # -----------------------------------------------------------
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot";
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
  };
  stylix.targets.grub.enable = false;

  # -----------------------------------------------------------
  # system specifics
  # -----------------------------------------------------------
  system.stateVersion = "24.05"; # DO NOT CHANGE
  networking.hostName = "goat";
  networking.networkmanager.enable = true;
  time.timeZone = "America/Vancouver";
  i18n.defaultLocale = "en_CA.UTF-8";
  # font for weird 16:10 resolution sacaling
  console = {
    earlySetup = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
    packages = with pkgs; [terminus_font];
    keyMap = "us";
  };
  # home manager
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs;};
  };

  # nix
  nixpkgs.config.allowUnfree = true;
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
  };

  # -----------------------------------------------------------
  # user
  # -----------------------------------------------------------
  users.users.goat = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };
  home-manager.users.goat = {
    home = {
      stateVersion = "24.05"; # DO NOT CHANGE
      username = "goat";
      homeDirectory = "/home/goat";
    };
  };
}
