{
  pkgs,
  config,
  ...
}: {
  # -----------------------------------------------------------
  # enviorment packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # dev tools
    ripgrep
    alejandra
    nixd
    # cli utils
    killall
    pciutils
    # web & net utils
    wget
    git
    curl
    networkmanagerapplet
    # compresssion & archiving
    unrar
    unzip
    file-roller
    tree
    isoimagewriter
  ];

  # -----------------------------------------------------------
  # home packages
  # -----------------------------------------------------------
  home-manager.users.goat.home.packages = with pkgs; [
    # apps
    firefox
    discord
    spotify
    vscode-fhs
    musescore
    zoom-us
    # cli
    btop
    sl
    cowsay
    neofetch
  ];

  # -----------------------------------------------------------
  # home programs
  # -----------------------------------------------------------
  home-manager.users.goat = {
    programs = {
      lazygit.enable = true;
      kitty = {
        enable = true;
        settings = {
          scrollback_lines = 2000;
          wheel_scroll_min_lines = 1;
          window_padding_width = 4;
          confirm_os_window_close = 0;
        };
        extraConfig = ''
          tab_bar_style fade
          tab_fade 1
          active_tab_font_style bold
          inactive_tab_font_style bold
        '';
      };
      neovim = {
        enable = true;
        defaultEditor = true;
        viAlias = true;
        vimAlias = true;
        extraConfig = ''
          set clipboard=unnamedplus
          set number
          set tabstop=2
          set shiftwidth=2
          set expandtab  " Use spaces instead of tabs
        '';
      };
      git = {
        enable = true;
        userEmail = "cooperkang4@gamil.com";
        userName = "Chucklee1";
      };
      oh-my-posh = {
        enable = true;
        enableBashIntegration = true;
        useTheme = "pure";
      };
      bash = {
        enable = true;
        shellAliases = {
          sv = "sudo nvim";
          v = "nvim";
          kittty = "kitty working-directory $HOME/nixos-dotfiles";
          exec-swww = "swww img ./pictures/night-ridgeline.jpg";
          ozonify = "--enable-features=UseOzonePlatform --ozone-platform=wayland";
          cg = "sudo nix-collect-garbage";
          update-desktop = "sudo nixos-rebuild switch --flake .#desktop --show-trace";
          update-macbook = "sudo nixos-rebuild switch --flake .#macbook --show-trace";
        };
      };
    };
    stylix.targets.neovim.enable = true;
  };

  # -----------------------------------------------------------
  # theming
  # -----------------------------------------------------------
  stylix = {
    enable = true;
    homeManagerIntegration.autoImport = true;
    image = ../pictures/night-ridgeline.jpg;
    base16Scheme = "${pkgs.base16-schemes}/share/themes/tokyo-city-terminal-dark.yaml";
    opacity.terminal = 0.6;
    cursor.package = pkgs.bibata-cursors;
    cursor.name = "Bibata-Modern-Classic";
    cursor.size = 24;
    fonts = {
      monospace = {
        package = pkgs.nerdfonts.override {fonts = ["JetBrainsMono"];};
        name = "JetBrainsMono Nerd Font Mono";
      };
      sansSerif = {
        package = pkgs.noto-fonts-cjk-sans;
        name = "Noto Sans CJK";
      };
      serif = {
        package = pkgs.noto-fonts-cjk-serif;
        name = "Noto Serif CJK";
      };
      sizes = {
        applications = 12;
        terminal = 12;
        desktop = 11;
        popups = 12;
      };
    };
  };

  home-manager.users.goat.gtk = {
    iconTheme.name = "Papirus-Dark";
    iconTheme.package = pkgs.papirus-icon-theme;
  };

  # -----------------------------------------------------------
  # hardware
  # -----------------------------------------------------------
  hardware = {
    graphics.enable = true; # renamed opengl to graphics as of 24.11
    graphics.enable32Bit = true;
    bluetooth.enable = true;
    bluetooth.powerOnBoot = true;
  };

  # -----------------------------------------------------------
  # services
  # -----------------------------------------------------------
  services = {
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    displayManager = {
      enable = true;
      ly.enable = true;
    };
    blueman.enable = true;
    printing.enable = true;
    gvfs.enable = true;
    tumbler.enable = true;
    fstrim.enable = true;
    openssh.enable = true;
  };
}
