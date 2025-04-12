{
  lib,
  pkgs,
  inputs,
  ...
}: let
  username = "goat";
in {
  imports = [
    ./theming.nix
    inputs.home-manager.darwinModules.home-manager
  ];

  system.stateVersion = 6;

  nixpkgs = {
    hostPlatform = lib.mkDefault "x86_64-darwin";
    config.allowUnfree = true;
  };

  nix = {
    package = pkgs.nix;
    settings.experimental-features = ["nix-command" "flakes"];
  };

  users.users.goat = {
    name = "${username}";
    home = "/Users/${username}";
    shell = pkgs.bashInteractive;
  };

  home-manager = {
    extraSpecialArgs = {inherit inputs;};
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${username} = {
      imports = [./nixvim.nix];
      home.stateVersion = "24.05";
    };
  };

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

  programs.bash.enable = true;

  homebrew = {
    enable = true;

    /*
      casks = [
    ];
    */

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
