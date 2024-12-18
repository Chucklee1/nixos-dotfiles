{pkgs, ...}: {
  programs = {
    xfconf.enable = true;
    thunar = {
      enable = true;
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
        thunar-volman
      ];
    };
  };
  services = {
    tumbler.enable = true;
    gvfs.enable = true;
  };
  home-manager.sharedModules = [
    {
      programs = {
        # git
        lazygit.enable = true;
        git = {
          enable = true;
          userEmail = "cooperkang4@gmail.com";
          userName = "Chucklee1 - remote";
        };
        # shell
        oh-my-posh = {
          enable = true;
          enableBashIntegration = true;
          useTheme = "pure";
        };
        bash = {
          enable = true;
          shellAliases = {
            vim = "nvim ./nixos-dotfiles";
            sv = "sudo nvim";
            cg = "sudo nix-collect-garbage";
            gen-conf-desktop = "sudo nixos-generate-config --show-hardware-config > ./nixos-dotfiles/modules/hosts/desktop/hardware.nix";
            gen-conf-laptop = "sudo nixos-generate-config --show-hardware-config > ./nixos-dotfiles/modules/hosts/laptop/hardware.nix";
            update-laptop = "sudo nixos-rebuild switch --impure --show-trace --flake ./nixos-dotfiles#laptop";
            update-desktop = "sudo nixos-rebuild switch --impure --show-trace --flake ./nixos-dotfiles#desktop";
            remote-update-laptop = "sudo nixos-rebuild switch --impure --show-trace --flake github:Chucklee1/nixos-dotfiles#laptop --no-write-lock-file";
            remote-update-desktop = "sudo nixos-rebuild switch --impure --show-trace --flake github:Chucklee1/nixos-dotfiles#desktop --no-write-lock-file";
          };
        };
        # terminal
        tmux.enable = true;
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
        # editors
        neovim = {
          enable = true;
          defaultEditor = true;
          extraConfig = ''
            set clipboard=unnamedplus
            set number
            set tabstop=2
            set shiftwidth=2
            set expandtab  " Use spaces instead of tabs
          '';
        };
        # soy drinker editor
        vscode = {
          enable = true;
          extensions = with pkgs.vscode-extensions; [
            jnoortheen.nix-ide
            kamadorueda.alejandra
          ];
          userSettings = {
            "update.mode" = "none";
            "update.enableWindowsBackgroundUpdates" = false;
            "editor.tabSize" = 2;
            "editor.detectIndentation" = false;
            "editor.minimap.enabled" = false;
            "editor.autoClosingBrackets" = "never";
            "editor.autoClosingQuotes" = "never";
            "editor.autoClosingParentheses" = "never";
            "files.autoSave" = "off";
            "files.confirmDelete" = false;
            "explorer.confirmDragAndDrop" = false;
            "explorer.confirmDelete" = false;
            "workbench.statusBar.visible" = true;
            "workbench.colorTheme" = "Stylix";
          };
        };
      };
    }
  ];
}
