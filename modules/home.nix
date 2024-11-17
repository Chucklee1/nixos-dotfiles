{pkgs, ...}: {
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {inherit inputs;};
    users.goat = {
      home = {
        imports = [./niri.nix];
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
  # -----------------------------------------------------------
  # niri setup ( wont work in home manager idk why )
  # -----------------------------------------------------------
  nixpkgs.overlays = [inputs.niri.overlays.niri];
  programs.niri = {
    enable = true;
    package = pkgs.niri-unstable; # make niri use overlay poackage
  };
  # -----------------------------------------------------------
  # home manager specifics
  # -----------------------------------------------------------
  home-manager.users.goat = {
    home.sessionVariables = {
      DISPLAY = ":0"; # for xwayland satellite
      XDG_CURRENT_DESKTOP = "niri";
      XDG_SESSION_DESKTOP = "niri";
      XDG_SESSION_TYPE = "wayland";
      GDK_BACKEND = "wayland";
      GTK_CSD = "0";
      CLUTTER_BACKEND = "wayland";
      QT_QPA_PLATFORM = "wayland";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      QT_AUTO_SCREEN_SCALE_FACTOR = "1";
      SDL_VIDEODRIVER = "wayland";
      MOZ_ENABLE_WAYLAND = "1";
      NIXOS_OZONE_WL = "1";
    };
    stylix.targets.niri.enable = true;
  };
}
