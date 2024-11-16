{
  lib,
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ./GPU/nvidia.nix # toggle module
    ./infasoftware.nix
    ./theming.nix
    # for my own sanity, niri will have its own nix file
    # related to niri stuff will go in wayland.nix
    ./wayland.nix
    ./niri.nix
    inputs.home-manager.nixosModules.home-manager
    inputs.stylix.nixosModules.stylix
    inputs.niri.nixosModules.niri
  ];

  system.stateVersion = "24.05"; # DO NOT CHANGE

  # boot loader
  boot.loader = {
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot";
    grub.enable = true;
    grub.efiSupport = true;
    grub.device = "nodev";
  };

  # tty settings
  console.font = "Lat2-Terminus16";
  console.useXkbConfig = true;

  # nix
  nixpkgs = {
    config.allowUnfree = true;
    overlays = [inputs.niri.overlays.niri];
  };
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
  };

  # specifics
  networking.hostName = "goat";
  networking.networkmanager.enable = true;
  time.timeZone = "America/Vancouver";
  i18n.defaultLocale = "en_CA.UTF-8";

  # user
  users.users.goat = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs;};
    users.goat = {
      home = {
        stateVersion = "24.05"; # DO NOT CHANGE
        username = "goat";
        homeDirectory = "/home/goat";
      };
      programs.bash = {
        enable = true;
        shellAliases = {
          sv = "sudo nvim";
          v = "nvim";
          kittty = "kitty working-directory $HOME/nixos-dotfiles";
          exec-swww = "swww init && swww img ~/nixos-dotfiles/wallpapers/mono-forest.PNG";
          ozonify = "--enable-features=UseOzonePlatform --ozone-platform=wayland";
          cg = "sudo nix-collect-garbage";
          update-desktop = "sudo nixos-rebuild switch --flake .#desktop --show-trace";
          update-macbook = "sudo nixos-rebuild switch --flake .#macbook --show-trace";
        };
      };
    };
  };
}
