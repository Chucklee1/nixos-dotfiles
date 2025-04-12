{
  lib,
  pkgs,
  ...
}: {
  imports = [./theming.nix];

  system.stateVersion = 6;

  nixpkgs = {
    hostPlatform = lib.mkDefault "x86_64-darwin";
    config.allowUnfree = true;
  };

  nix.settings.experimental-features = ["nix-command" "flakes"];

  system.defaults = {
    dock = {
      autohide = true;
      show-process-indicators = false;
      show-recents = false;
      static-only = true;
    };
    finder = {
      AppleShowAllExtensions = true;
      ShowPathbar = true;
      FXEnableExtensionChangeWarning = false;
    };
    NSGlobalDomain = {
      AppleKeyboardUIMode = 3;
      "com.apple.keyboard.fnState" = true;
    };
  };

  homebrew = {
    enable = true;

    /*
      casks = [];

    masApps = {
      "Drafts" = 1435957248;
      "Reeder" = 1529448980;
      "Things" = 904280696;
      "Timery" = 1425368544;
    };
    */
  };

  #fonts.fonts = [];
}
