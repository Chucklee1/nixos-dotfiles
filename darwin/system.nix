{
  lib,
  pkgs,
  username,
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
    name = "${username}";
    home = "/Users/${username}";
    shell = pkgs.bash;
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

  # programs - nix
  programs.bash.enable = true;

  # programs - homebrew
  homebrew = {
    enable = true;

    casks = [
      "kitty"
    ];

    /*
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
