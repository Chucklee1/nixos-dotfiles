{
  description = "Never let them know your next move";

  inputs = {
    # main repos
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # disk formatting
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    nix-darwin.url = "github:LnL7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    mac-app-util.url = "github:hraban/mac-app-util";
    # macos
    # theming
    stylix.url = "github:danth/stylix";
    minegrub-theme.url = "github:Lxtharia/minegrub-theme";
    minesddm.url = "github:Davi-S/sddm-theme-minesddm";
    minesddm.inputs.nixpkgs.follows = "nixpkgs";
    # neovim
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    # wayland
    niri.url = "github:sodiboo/niri-flake";
    waybar. url = "github:Alexays/Waybar/master";
  };

  outputs = {
    self,
    nixpkgs,
    nix-darwin,
    ...
  } @ inputs: let
    # ---- pkgs ----
    #allSystems = nixpkgs.lib.genAttrs ["x86_64-linux" "aarch64-darwin"];
    # ---- libs & helpers ----
    extlib = import "${self}/libs.nix" {inherit nixpkgs;};

    # ---- nixvim ----
    nixvim = system: let
      nixvimpkgs = inputs.nixvim.legacyPackages.${system};
    in
      nixvimpkgs.makeNixvimWithModule {
        inherit system;
        module.imports = extlib.simpleMerge "${self}/nixvim";
      };

    # ---- system  ----
    profiles = ["desktop" "laptop" "umbra"];

    mkMod = host: (extlib.mergeProfiles (extlib.mergeModules "${self}/modules" {inherit inputs self;}) "global" host);
  in {
    nixosConfigurations =
      nixpkgs.lib.genAttrs profiles
      (host: let
        mod = mkMod host;
        specialArgs = {
          nixvim = nixvim "x86_64-linux";
          system = "x86_64-linux";
          machine = host;
          user = "goat";
        };
      in
        nixpkgs.lib.nixosSystem {
          inherit specialArgs;
          modules =
            mod.nix
            ++ [{home-manager.extraSpecialArgs = specialArgs;}]
            ++ [{_module.args.homeMods = mod.home;}];
        });
    darwinConfigurations.macbook = nix-darwin.lib.darwinSystem {
      specialArgs = {
        nixvim = nixvim "aarch64-darwin";
        system = "aarch64-darwin";
        user = "goat";
      };
      modules = [
        inputs.home-manager.nixDarwinModules.home-manager
        {
          #environment.systemPackages = [];
          nix.settings.experimental-features = "nix-command flakes";
          programs.bash.enable = true;
          system.stateVersion = 6;

          nixpkgs = {
            hostPlatform = "aarch64-darwin";
            config.allowUnfree = true;
          };
        }
        (let
          user = "goat";
        in {
          users.users.${user} = {
            name = "${user}";
            home = "/Users/${user}";
          };
          home-manager.extraSpecialArgs = {inherit nixvim;};
          home-manager.users.${user} = {
            nixpkgs.config.allowUnfree = true;
            home.packages = [nixvim];
            home.stateVersion = "25.05";
            programs = {
              bash.enable = true;
              git = {
                enable = true;
                userEmail = "kermitthefrog@kakao.com";
                userName = "Chucklee1";
              };
              kitty = {
                enable = true;
                settings = {
                  confirm_os_window_close = 0;
                  tab_bar_edge = "bottom";
                  tab_bar_style = lib.mkForce "powerline";
                  tab_powerline_style = "round";
                };
              };
              lazygit = {
                enable = true;
                settings = {
                  notARepository = "skip";
                  promptToReturnFromSubprocess = false;
                };
              };
              oh-my-posh = {
                enable = true;
                useTheme = "pure";
              };
              yazi = {
                enable = true;
                plugins =
                  lib.genAttrs ["mediainfo" "mount" "restore"] (pn: pkgs.yaziPlugins.${pn});
              };
            };
          };
        })
        stylix.nixosModules.stylix
        ({pkgs, ...}: let
          hideHashtag = set: builtins.mapAttrs (_: v: builtins.replaceStrings ["#"] [""] v) set;
        in {
          stylix = {
            enable = true;
            autoEnable = true;
            homeManagerIntegration.autoImport = true;
            image = pkgs.fetchurl {
              url = "https://w.wallhaven.cc/full/5g/wallhaven-5g22q5.png";
              hash = "sha256-snqkeQecU0opsBfIrnkl6aiV71hSCmqnZBAsibNG4w8=";
            };
            base16Scheme = hideHashtag {
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
            polarity = "dark";
            opacity.terminal = 0.8;
          };
        })
      ];
    };
  };
}
