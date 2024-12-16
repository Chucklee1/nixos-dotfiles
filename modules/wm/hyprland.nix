{
  config,
  lib,
  pkgs,
  defaults,
  ...
}: {
  options.hyprland.enable = lib.mkEnableOption "enable hyprland window manager";

  config = lib.mkIf config.hyprland.enable {
    programs.hyprland = {
      enable = true;
      xwayland.enable = true;
    };
    home-manager.sharedModules = [
      {
        wayland.windowManager.hyprland.extraConfig = ''
          monitor=,1920x1080@165,auto,auto

            exec-once = swww-daemon && swww img ${defaults.wallpaper}
            exec-once = ${pkgs.networkmanagerapplet}
            exec-once = ${pkgs.lxqt.lxqt-policykit}
            exec-once =  ${lib.getExe pkgs.wlsunset} -T 5200


          #############################
          ### ENVIRONMENT VARIABLES ###
          #############################

          ENV = NIXOS_OZONE_WL,1
          ENV = XDG_SESSION_TYPE,wayland
          ENV = DISPLAY,:0
          ENV = GDK_BACKEND,wayland
          ENV = GTK_CSD,0
          ENV = CLUTTER_BACKEND,wayland
          ENV = QT_QPA_PLATFORM,wayland;xcb
          ENV = QT_WAYLAND_DISABLE_WINDOWDECORATION,1
          ENV = QT_AUTO_SCREEN_SCALE_FACTOR,1
          ENV = SDL_VIDEODRIVER,x11
          ENV = MOZ_ENABLE_WAYLAND,1

          ENV = GBM_BACKEND,nvidia_drm
          ENV =  __GLX_VENDOR_LIBRARY_NAME,nvidia
          ENV =  LIBVA_DRIVER_NAME,nvidia

          cursor {
            no_hardware_cursors = true
          }

          #####################
          ### LOOK AND FEEL ###
          #####################

          general {
              gaps_in = 5
              gaps_out = 20
              border_size = 2
              col.active_border = ${defaults.colors.base0D}
              col.inactive_border = ${defaults.colors.base03}
              resize_on_border = false
              allow_tearing = false
              layout = dwindle
          }

          decoration {
              rounding = 0

              active_opacity = 1.0
              inactive_opacity = 1.0

              shadow {
                  enabled = true
                  range = 4
                  render_power = 3
                  color = rgba(1a1a1aee)
              }

              blur {
                  enabled = true
                  size = 5
                  passes = 2
                  vibrancy = 0.1696
              }
          }

          animations {
              enabled = true

              bezier = easeOutQuint,0.23,1,0.32,1
              bezier = easeInOutCubic,0.65,0.05,0.36,1
              bezier = linear,0,0,1,1
              bezier = almostLinear,0.5,0.5,0.75,1.0
              bezier = quick,0.15,0,0.1,1

              animation = global, 1, 10, default
              animation = border, 1, 5.39, easeOutQuint
              animation = windows, 1, 4.79, easeOutQuint
              animation = windowsIn, 1, 4.1, easeOutQuint, popin 87%
              animation = windowsOut, 1, 1.49, linear, popin 87%
              animation = fadeIn, 1, 1.73, almostLinear
              animation = fadeOut, 1, 1.46, almostLinear
              animation = fade, 1, 3.03, quick
              animation = layers, 1, 3.81, easeOutQuint
              animation = layersIn, 1, 4, easeOutQuint, fade
              animation = layersOut, 1, 1.5, linear, fade
              animation = fadeLayersIn, 1, 1.79, almostLinear
              animation = fadeLayersOut, 1, 1.39, almostLinear
              animation = workspaces, 1, 1.94, almostLinear, fade
              animation = workspacesIn, 1, 1.21, almostLinear, fade
              animation = workspacesOut, 1, 1.94, almostLinear, fade
          }

          dwindle {
              pseudotile = true
              preserve_split = true
          }

          master {
            new_status = master
          }

          misc {
              force_default_wallpaper = 0
              disable_hyprland_logo = true
          }


          #############
          ### INPUT ###
          #############

          input {
              kb_layout = us
              kb_variant =
              kb_model =
              kb_options =
              kb_rules =

              follow_mouse = 1
              sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
              touchpad {
                  natural_scroll = false
              }
          }

          gestures {
            workspace_swipe = false
          }

          ###################
          ### KEYBINDINGS ###
          ###################

          $mainMod = SUPER # Sets "Windows" key as main modifier

          bind = $mainMod, return, exec, ${defaults.terminal}
          bind = $mainMod, Q, killactive,
          bind = $mainMod, M, exit,
          bind = $mainMod, E, exec, ${defaults.file-manager}
          bind = $mainMod, V, togglefloating,
          bind = $mainMod, space, exec, fuzzel
          bind = $mainMod, P, pseudo, # dwindle
          bind = $mainMod, J, togglesplit, # dwindle

          # Move focus with mainMod + arrow keys
          bind = $mainMod, left, movefocus, l
          bind = $mainMod, right, movefocus, r
          bind = $mainMod, up, movefocus, u
          bind = $mainMod, down, movefocus, d

          # Switch workspaces with mainMod + [0-9]
          bind = $mainMod, 1, workspace, 1
          bind = $mainMod, 2, workspace, 2
          bind = $mainMod, 3, workspace, 3
          bind = $mainMod, 4, workspace, 4

          # Move active window to a workspace with mainMod + SHIFT + [0-9]
          bind = $mainMod SHIFT, 1, movetoworkspace, 1
          bind = $mainMod SHIFT, 2, movetoworkspace, 2
          bind = $mainMod SHIFT, 3, movetoworkspace, 3
          bind = $mainMod SHIFT, 4, movetoworkspace, 4

          # Scroll through existing workspaces with mainMod + scroll
          bind = $mainMod, mouse_down, workspace, e+1
          bind = $mainMod, mouse_up, workspace, e-1

          # Move/resize windows with mainMod + LMB/RMB and dragging
          bindm = $mainMod, mouse:272, movewindow
          bindm = $mainMod, mouse:273, resizewindow

          # Laptop multimedia keys for volume and LCD brightness
          bindel = ,XF86AudioRaiseVolume, exec, wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+
          bindel = ,XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-
          bindel = ,XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
          bindel = ,XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle
          bindel = ,XF86MonBrightnessUp, exec, brightnessctl s 10%+
          bindel = ,XF86MonBrightnessDown, exec, brightnessctl s 10%-

          # Requires playerctl
          bindl = , XF86AudioNext, exec, playerctl next
          bindl = , XF86AudioPause, exec, playerctl play-pause
          bindl = , XF86AudioPlay, exec, playerctl play-pause
          bindl = , XF86AudioPrev, exec, playerctl previous

          ##############################
          ### WINDOWS AND WORKSPACES ###
          ##############################


          # Example windowrule v1
          # windowrule = float, ^(kitty)$

          # Example windowrule v2
          # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$

          # Ignore maximize requests from apps. You'll probably like this.
          windowrulev2 = suppressevent maximize, class:.*

          # Fix some dragging issues with XWayland
          windowrulev2 = nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0
        '';
      }
    ];
  };
}
