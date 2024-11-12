{...}: {
  programs.hyprland = {
    enable = true;
    extraConfig = ''
      ################
      ### MONITORS ###
      ################

      monitor=,preferred,auto,auto

      #################
      ### AUTOSTART ###
      #################

      exec = dunst
      exec = swww init
      exec = waybar
      exec = lxqt-policykit-agent
      exec = wlsunset

      #############################
      ### ENVIRONMENT VARIABLES ###
      #############################

      env = XDG_CURRENT_DESKTOP, Hyprland
      env = XDG_SESSION_DESKTOP, Hyprland


      #####################
      ### LOOK AND FEEL ###
      #####################

      general {
      gaps_in = 5
      gaps_out = 10

      border_size = 1
      col.active_border = #8fa1b3
      col.inactive_border = #4f5b66

      # Set to true enable resizing windows by clicking and dragging on borders and gaps
      resize_on_border = false

      allow_tearing = false

      layout = dwindle
      }

      decoration {
      rounding = 5

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
          passes = 3

          vibrancy = 0.1696
      }
      }

      animations {
      enabled = yes

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
      pseudotile = true # Master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
      preserve_split = true # You probably want this
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

      follow_mouse = 1

      sensitivity = 0 # -1.0 - 1.0, 0 means no modification.

      touchpad {
          natural_scroll = false
      }
      }

      gestures {
      workspace_swipe = false;
      }

      ###################
      ### KEYBINDINGS ###
      ###################

      $mainMod = SUPER

      bind = $mainMod, return, exec, kitty
      bind = $mainMod, Q, killactive,
      bind = Control ,Alt, Delete exit,
      bind = $mainMod, E, exec, thunar
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

      # Ignore maximize requests from apps. You'll probably like this.
      windowrulev2 = suppressevent maximize, class:.*

      # Fix some dragging issues with XWayland
      windowrulev2 = nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0
    '';
  };
}
