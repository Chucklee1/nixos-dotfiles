{
  pkgs,
  lib,
  config,
  ...
}: {
  imports = [
    ./waybar.nix
  ];
  # sym linking
  home.file.".config/niri/config.kdl".source = ../../home/niri.kdl;
  # user theming
  gtk.iconTheme.name = "Papirus-Dark";
  gtk.iconTheme.package = pkgs.papirus-icon-theme;

  # packages
  home.packages = with pkgs; [
    firefox
    vscode-fhs
    musescore
    wineWowPackages.waylandFull
    # wm stuff
    libnotify
    dunst
    swaylock-effects
    swayidle
    swww
  ];

  # smaller dotfiles
  programs = {
    lazygit.enable = true;
    fuzzel.enable = true;
    wlogout.enable = true;
    git = {
      enable = true;
      userEmail = "cooperkang4@gamil.com";
      userName = "Chucklee1";
    };
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
        active_tab_font_style   bold
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
    bash = {
      enable = true;
      shellAliases = {
        sv = "sudo nvim";
        v = "nvim";
        exec-waybar = "pkill waybar && waybar &";
        exec-swww = "pkill swww && swww init && swww img ~/nixos-dotfiles/home-folder/pictures/wallpapers/mono-forest.PNG";
        wayland-code = "code --enable-features=UseOzonePlatform --ozone-platform=wayland";
        cg = "sudo nix-collect-garbage";
        update-caprine = "sudo nixos-rebuild switch --flake ~/nixos-dotfiles#caprine --show-trace";
        update-goat = "sudo nixos-rebuild switch --flake ~/nixos-dotfiles#goat --show-trace";
      };
    };
  };
  wayland.windowManager.hyprland = {
    # Primary settings for Hyprland, converted from default example configuration
    settings = {
      # Monitor configuration
      monitor = {
        name = "";
        preferred = true;
        resolution = "auto";
        refresh_rate = "auto";
      };

      # Autostart applications
      autostart = [
        # "exec-once = ${terminal}"  # Uncomment to launch terminal on start
        "nm-applet &"
        "waybar"
      ];

      # Environment variables
      environment = {
        XCURSOR_SIZE = 24;
        HYPRCURSOR_SIZE = 24;
      };

      # General look and feel
      general = {
        gaps_in = 5;
        gaps_out = 20;
        border_size = 2;
        col = {
          active_border = "rgba(33ccffee) rgba(00ff99ee) 45deg";
          inactive_border = "rgba(595959aa)";
        };
        resize_on_border = false;
        allow_tearing = false;
        layout = "dwindle";
      };

      # Window decoration
      decoration = {
        rounding = 10;
        active_opacity = 1.0;
        inactive_opacity = 1.0;
        drop_shadow = true;
        shadow_range = 4;
        shadow_render_power = 3;
        col = {
          shadow = "rgba(1a1a1aee)";
        };
        blur = {
          enabled = true;
          size = 3;
          passes = 1;
          vibrancy = 0.1696;
        };
      };

      # Animations
      animations = {
        enabled = true;
        bezier = {
          myBezier = "0.05,0.9,0.1,1.05";
        };
        animations = [
          {
            type = "windows";
            speed = 1;
            strength = 7;
            easing = "myBezier";
          }
          {
            type = "windowsOut";
            speed = 1;
            strength = 7;
            easing = "default";
            effect = "popin 80%";
          }
          {
            type = "border";
            speed = 1;
            strength = 10;
            easing = "default";
          }
          {
            type = "borderangle";
            speed = 1;
            strength = 8;
            easing = "default";
          }
          {
            type = "fade";
            speed = 1;
            strength = 7;
            easing = "default";
          }
          {
            type = "workspaces";
            speed = 1;
            strength = 6;
            easing = "default";
          }
        ];
      };

      # Dwindle and master layout settings
      dwindle = {
        pseudotile = true;
        preserve_split = true;
      };
      master = {
        new_status = "master";
      };

      # Miscellaneous settings
      misc = {
        force_default_wallpaper = -1;
        disable_hyprland_logo = false;
      };

      # Input configuration
      input = {
        kb_layout = "us";
        follow_mouse = 1;
        sensitivity = 0;
        touchpad = {
          natural_scroll = false;
        };
      };
      gestures = {
        workspace_swipe = false;
      };

      # Device-specific input settings
      device = {
        name = "epic-mouse-v1";
        sensitivity = -0.5;
      };

      # Keybindings
      binds = [
        {
          mod = "SUPER";
          key = "Q";
          action = "exec kitty";
        }
        {
          mod = "SUPER";
          key = "C";
          action = "killactive";
        }
        {
          mod = "SUPER";
          key = "M";
          action = "exit";
        }
        {
          mod = "SUPER";
          key = "E";
          action = "exec dolphin";
        }
        {
          mod = "SUPER";
          key = "V";
          action = "togglefloating";
        }
        {
          mod = "SUPER";
          key = "R";
          action = "exec wofi --show drun";
        }
        {
          mod = "SUPER";
          key = "P";
          action = "pseudo";
        }
        {
          mod = "SUPER";
          key = "J";
          action = "togglesplit";
        }

        # Workspace navigation
        {
          mod = "SUPER";
          key = "left";
          action = "movefocus l";
        }
        {
          mod = "SUPER";
          key = "right";
          action = "movefocus r";
        }
        {
          mod = "SUPER";
          key = "up";
          action = "movefocus u";
        }
        {
          mod = "SUPER";
          key = "down";
          action = "movefocus d";
        }

        # Switch workspaces with SUPER + number keys
        {
          mod = "SUPER";
          key = "1";
          action = "workspace 1";
        }
        {
          mod = "SUPER";
          key = "2";
          action = "workspace 2";
        }
        {
          mod = "SUPER";
          key = "3";
          action = "workspace 3";
        }
        {
          mod = "SUPER";
          key = "4";
          action = "workspace 4";
        }
        {
          mod = "SUPER";
          key = "5";
          action = "workspace 5";
        }
        {
          mod = "SUPER";
          key = "6";
          action = "workspace 6";
        }
        {
          mod = "SUPER";
          key = "7";
          action = "workspace 7";
        }
        {
          mod = "SUPER";
          key = "8";
          action = "workspace 8";
        }
        {
          mod = "SUPER";
          key = "9";
          action = "workspace 9";
        }
        {
          mod = "SUPER";
          key = "0";
          action = "workspace 10";
        }

        # Move windows to workspaces
        {
          mod = "SUPER SHIFT";
          key = "1";
          action = "movetoworkspace 1";
        }
        {
          mod = "SUPER SHIFT";
          key = "2";
          action = "movetoworkspace 2";
        }
        {
          mod = "SUPER SHIFT";
          key = "3";
          action = "movetoworkspace 3";
        }
        {
          mod = "SUPER SHIFT";
          key = "4";
          action = "movetoworkspace 4";
        }
        {
          mod = "SUPER SHIFT";
          key = "5";
          action = "movetoworkspace 5";
        }
        {
          mod = "SUPER SHIFT";
          key = "6";
          action = "movetoworkspace 6";
        }
        {
          mod = "SUPER SHIFT";
          key = "7";
          action = "movetoworkspace 7";
        }
        {
          mod = "SUPER SHIFT";
          key = "8";
          action = "movetoworkspace 8";
        }
        {
          mod = "SUPER SHIFT";
          key = "9";
          action = "movetoworkspace 9";
        }
        {
          mod = "SUPER SHIFT";
          key = "0";
          action = "movetoworkspace 10";
        }

        # Multimedia keys
        {
          key = "XF86AudioRaiseVolume";
          action = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+";
        }
        {
          key = "XF86AudioLowerVolume";
          action = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
        }
        {
          key = "XF86AudioMute";
          action = "exec wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
        }
        {
          key = "XF86AudioMicMute";
          action = "exec wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle";
        }
        {
          key = "XF86MonBrightnessUp";
          action = "exec brightnessctl s 10%+";
        }
        {
          key = "XF86MonBrightnessDown";
          action = "exec brightnessctl s 10%-";
        }

        # Media keys (requires playerctl)
        {
          key = "XF86AudioNext";
          action = "exec playerctl next";
        }
        {
          key = "XF86AudioPause";
          action = "exec playerctl play-pause";
        }
        {
          key = "XF86AudioPlay";
          action = "exec playerctl play-pause";
        }
        {
          key = "XF86AudioPrev";
          action = "exec playerctl previous";
        }
      ];

      # Window and workspace rules
      window_rules = [
        {
          rule = "suppressevent maximize";
          class = ".*";
        }
        {
          rule = "nofocus";
          class = "^$";
          title = "^$";
          xwayland = 1;
          floating = 1;
          fullscreen = 0;
          pinned = 0;
        }
      ];
    };
  };
}
