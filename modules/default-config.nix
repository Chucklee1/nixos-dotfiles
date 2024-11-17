{
  pkgs,
  inputs,
  lib,
  config,
  ...
}: {
  imports = [
    # flake inputs
    inputs.home-manager.nixosModules.default
    inputs.stylix.nixosModules.default
    inputs.niri.nixosModules.default
    # toggle modules
    ./GPU/nvidia.nix
    ./niri/niri.nix
    # the rest
    ./infasoftware.nix
    ./theming.nix
  ];

  # -----------------------------------------------------------
  # boot loader & boot options
  # -----------------------------------------------------------
  boot.loader = {
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot";
    grub.enable = true;
    grub.efiSupport = true;
    grub.device = "nodev";
  };

  # -----------------------------------------------------------
  # system specifics
  # -----------------------------------------------------------
  system.stateVersion = "24.05"; # DO NOT CHANGE
  networking.hostName = "goat";
  networking.networkmanager.enable = true;
  time.timeZone = "America/Vancouver";
  i18n.defaultLocale = "en_CA.UTF-8";

  # nix
  nixpkgs.config.allowUnfree = true;
  nix.settings = {
    auto-optimise-store = true;
    experimental-features = ["nix-command" "flakes"];
  };

  # tty settings
  console.font = "Lat2-Terminus16";
  console.useXkbConfig = true;

  # -----------------------------------------------------------
  # user
  # -----------------------------------------------------------
  users.users.goat = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };
  # shared-home-manager-settings
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs;};
  };
  home-manager.users.goat = {
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
        exec-swww = "swww init && swww img ./wallpapers/mono-forest.PNG";
        ozonify = "--enable-features=UseOzonePlatform --ozone-platform=wayland";
        cg = "sudo nix-collect-garbage";
        update-desktop = "sudo nixos-rebuild switch --flake .#desktop --show-trace";
        update-macbook = "sudo nixos-rebuild switch --flake .#macbook --show-trace";
      };
    };
  };
}
