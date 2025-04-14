{
  lib,
  pkgs,
  inputs,
  ...
}: {
  # system - nix
  system.stateVersion = 6;

  nixpkgs = {
    hostPlatform = lib.mkDefault "x86_64-darwin";
    config.allowUnfree = true;
  };

  nix = {
    package = pkgs.nix;
    settings.experimental-features = ["nix-command" "flakes"];
  };
  # system - user
  users.users.goat = {
    name = "goat";
    home = "/Users/goat";
  };

  imports = [inputs.home-manager.darwinModules.home-manager];
  home-manager = {
    extraSpecialArgs = {inherit inputs;};
    useGlobalPkgs = true;
    useUserPackages = true;
    users.goat = {
      imports = [../assets/nixvim.nix];
      home.stateVersion = "24.05";
    };
  };
  # system - macos
  system.defaults = {
    dock = {
      autohide = true;
      show-process-indicators = false;
      show-recents = false;
      static-only = true;
    };
    finder = {
      AppleShowAllExtensions = true;
      AppleShowAllFiles = true;
      ShowPathbar = true;
      FXEnableExtensionChangeWarning = false;
    };
    NSGlobalDomain = {
      AppleKeyboardUIMode = 3;
      "com.apple.keyboard.fnState" = true;
    };
  };
}
