{
  pkgs,
  inputs,
  def,
  ...
}: {
  # -----------------------------------------------------------
  # packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # dependancies
    zenity
    libnotify
    libsecret
    # wine
    wine
    wineWowPackages.stagingFull
    samba
    winetricks
    # gcc glory
    gnumake
    gdb
    gcc
    # cli
    ripgrep
    pciutils
    btop
    ncdu
    # web/net
    wget
    git
    curl
    # media/files
    file-roller
    p7zip
    pavucontrol
    v4l-utils
    # apps
    krita
    webcord
    spotify
  ];

  # programs
  programs = {
    dconf.enable = true;
    xfconf.enable = true;
    firefox.enable = true;
    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
      ];
    };
  };

  home-manager.sharedModules = [
    inputs.nixvim.homeManagerModules.nixvim
    {
      programs = {
        # editor
        nixvim = import ./neovim.nix;
        # git
        git = {
          enable = true;
          userEmail = "cooperkang4@gmail.com";
          userName = "Chucklee1 - remote";
        };
        # terminal emulator
        kitty = {
          enable = true;
          settings = {
            confirm_os_window_close = 0;
            hide_window_decorations = true;
          };
        };
        # shell
        bash = {
          enable = true;
          shellAliases = {
            cg = "nix-collect-garbage";
            flake = "$HOME/nixos-dotfiles/flake.nix";
            update-flake = "sudo nixos-rebuild switch --impure --show-trace --flake $HOME/nixos-dotfiles#${def.host}";
          };
        };
        oh-my-posh = {
          enable = true;
          enableBashIntegration = true;
        };
      };
    }
  ];
}
