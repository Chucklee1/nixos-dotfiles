{inputs, ...}: {
  nix.macbook = [
    # darwin
    {
      system.stateVersion = 6;
      nix.settings.experimental-features = "nix-command flakes";
      nixpkgs = {
        hostPlatform = "aarch64-darwin";
        config.allowUnfree = true;
      };
      programs.bash.enable = true;
    }
    # stylix
    inputs.stylix.darwinModules.stylix
    ({pkgs, ...}: let
      hideHashtag = set: builtins.mapAttrs (_: v: builtins.replaceStrings ["#"] [""] v) set;
      nordic = hideHashtag {
        name = "nordic";
        author = "AlexvZyl";
        base00 = "#191D24";
        base01 = "#242933";
        base02 = "#3B4252";
        base03 = "#4C566A";
        base04 = "#D8DEE9";
        base05 = "#E5E9F0";
        base06 = "#ECEFF4";
        base07 = "#8FBCBB";
        base08 = "#BF616A";
        base09 = "#D08770";
        base0A = "#EBCB8B";
        base0B = "#A3BE8C";
        base0C = "#88C0D0";
        base0D = "#81A1C1";
        base0E = "#B48EAD";
        base0F = "#C0C8D8";
      };
      cosmic = pkgs.fetchurl {
        url = "https://w.wallhaven.cc/full/5g/wallhaven-5g22q5.png";
        hash = "sha256-snqkeQecU0opsBfIrnkl6aiV71hSCmqnZBAsibNG4w8=";
      };
    in {
      stylix = {
        enable = true;
        autoEnable = true;
        homeManagerIntegration.autoImport = true;
        image = cosmic;
        base16Scheme = nordic;
        polarity = "dark";
        opacity.terminal = 0.95;
        fonts = {
          monospace.package = pkgs.nerd-fonts.jetbrains-mono;
          monospace.name = "JetBrainsMono Nerd Font Mono";
          sansSerif.package = pkgs.noto-fonts-cjk-sans;
          sansSerif.name = "Noto Sans CJK";
          serif.package = pkgs.noto-fonts-cjk-serif;
          serif.name = "Noto Serif CJK";

          sizes = {
            applications = 12;
            terminal = 12;
            desktop = 11;
            popups = 12;
          };
        };
      };
    })
    # home manager
    inputs.home-manager.darwinModules.home-manager
    ({
      pkgs,
      config,
      ...
    }: {
      users.users.goat = {
        name = "goat";
        home = "/Users/goat";
        shell = pkgs.bashInteractive;
        ignoreShellProgramCheck = true;
      };
      home-manager.useGlobalPkgs = true;
      home-manager.users.goat = {
        home.stateVersion = "25.05";
        imports = config._module.args.homeMods;
      };
    })
  ];
}
