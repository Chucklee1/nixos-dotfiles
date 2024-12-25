{
  lib,
  config,
  pkgs,
  def,
  ...
}: {
  imports = [./hardware.nix];
  # -----------------------------------------------------------
  # system
  # -----------------------------------------------------------
  boot = {
    initrd.availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod"];
    kernelModules = ["kvm-amd"];
    supportedFilesystems = ["ntfs"];
  };

  system.stateVersion = "24.05"; # DO NOT CHANGE
  networking = {
    interfaces.enp7s0.useDHCP = lib.mkDefault true;
    interfaces.wlp6s0.useDHCP = lib.mkDefault true;
  };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia = {
    powerManagement.enable = false;
    powerManagement.finegrained = false;
    open = false;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  # -----------------------------------------------------------
  # packages
  # -----------------------------------------------------------
  environment.systemPackages = with pkgs; [
    # tools/deps
    nvidia-vaapi-driver
    zenity
    wineWowPackages.stagingFull
    samba
    winetricks
    protonup-qt
    protontricks
    # hyprland
    hyprland-protocols
    hyprshot
    hypridle
    hyprlock
    hyprsunset
    # apps/games
    webcord
    osu-lazer-bin
    prismlauncher
  ];

  # -----------------------------------------------------------
  # programs
  # -----------------------------------------------------------
  programs = {
    hyprland.enable = true;
    virt-manager.enable = true;
    dconf.enable = true;
    gamemode.enable = true;
    steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      localNetworkGameTransfers.openFirewall = true;
    };
  };

  # -----------------------------------------------------------
  # misc
  # -----------------------------------------------------------
  environment.variables = {STEAM_EXTRA_COMPAT_TOOLS_PATHS = "~/.steam/root/compatibilitytools.d";};
  virtualisation = {
    spiceUSBRedirection.enable = true;
    libvirtd = {
      enable = true;
      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;
        swtpm.enable = true;
        ovmf = {
          enable = true;
          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            })
            .fd
          ];
        };
      };
    };
  };

  # -----------------------------------------------------------
  # home manager
  # -----------------------------------------------------------
  home-manager.sharedModules = [
    {
      dconf.settings = {
        "org/virt-manager/virt-manager/connections" = {
          autoconnect = ["qemu:///system"];
          uris = ["qemu:///system"];
        };
      };
      wayland.windowManager.hyprland = {
        enable = true;
        plugins = [pkgs.hyprlandPlugins.hyprscroller];
        settings = {
          # general
          monitor = ", 1920x1080@165, auto, auto";
          xwayland.force_zero_scaling = true;
          input = {
            kb_layout = "${def.layout}";
            follow_mouse = 0;
            sensitivity = 0;
          };
          gestures = {
            workspace_swipe = false;
            workspace_swipe_forever = false;
          };
          misc = {
            disable_autoreload = true;
            force_default_wallpaper = 0;
            animate_mouse_windowdragging = false;
          };
          # window rules windowrule = <rule>,<info>
          # windowrulev2 = [];
          # startup
          exec-once = [
            "waybar"
            "nm-applet"
            "hyprsunset -t 5200"
            "swaybg -i $HOME/nixos-dotfiles/assets/wallpaper.png -m fill"
          ];
          # theming & layoutr
          general = {
            layout = "scroller";
            gaps_in = 2;
            gaps_out = 4;
            border_size = 2;
            allow_tearing = true;
            resize_on_border = true;
          };
          decoration = {
            rounding = 4;
            animations = {
              # bezier = NAME, X0, Y0, X1, Y1
              bezier = [
                "ease-out-quad, 0.76, 0, 0.24, 1"
                "ease-out-cubic, 0.33, 1, 0.68, 1"
                "ease-out-expo, 0.16, 1, 0.3, 1"
                "spring, 0.25, 0.75, 0.50, 1.0"
              ];
              # animation: NAME, ONOFF, SPEED, CURVE [,STYLE]
              animation = [
                "workspaces, 1, 1.5, spring"
                "windows, 1, 1.5, spring"
                "windowsIn, 1, 4, ease-out-expo" # open
                "windowsOut, 1, 4, ease-out-quad" # close`
                "windowsMove, 1, 2.9, spring"
                "fade, 0"
              ];
            };
          };
          # bindings: MODS, key, dispatcher, params
          "$mod" = "SUPER";
          bind = [
            # programs
            "$mod, return, exec, kitty"
            "$mod shift, return, exec, kitty -e tmux"
            "$mod, space, exec, fuzzel"
            "$mod, e, exec, thunar"
            "$mod shift, p, exec, wlogout"
            "$mod shift, l, exec, swaylock"
            # hyprland
            "$mod, v, togglefloating"
            "$mod shift, m, fullscreen"
            ", ctrl+alt+del, exit"
            # scroller - overview windows
            "$mod shift, space, scroller:toggleoverview"
            # scroller - align windows
            "$mod, h, scroller:alignwindow, l"
            "$mod, l, scroller:alignwindow, r"
            # scroller - set columns
            "$mod shift, j, scroller:admitwindow"
            "$mod shift, k, scroller:expelwindow"
            # scroller - semi fullscreen
            "$mod, m, scroller:fitsize, active"
            # scroller - set window size
            "$mod, equal, scroller:cyclewidth, next"
            "$mod, minus, scroller:cyclewidth, prev"
            "$mod SHIFT, equal, scroller:cycleheight, next"
            "$mod SHIFT, minus, scroller:cycleheight, prev"
            # windows
            "$mod, Q, killactive"
            "$mod, left, movefocus, l"
            "$mod, right, movefocus, r"
            "$mod, up, movefocus, u"
            "$mod, down, movefocus, d"
            "$mod shift, left, movewindow, l"
            "$mod shift, right, movewindow, r"
            "$mod shift, up, movewindow, u"
            "$mod shift, down, movewindow, d"
            # workspaces
            "$mod ctrl, up, workspace, -1"
            "$mod ctrl, down, workspace, +1"
            "$mod shift ctrl, up, movetoworkspace, -1"
            "$mod shift ctrl, down, movetoworkspace, +1"
          ];
          bindm = [
            # mouse related
            "$mod, mouse:272, movewindow"
            "$mod, mouse:273, resizewindow"
            "$mod ALT, mouse:272, resizewindow"
          ];
          bindl = [
            # media keys
            ", XF86AudioPlay, exec, playerctl play-pause"
            ", XF86AudioPrev, exec, playerctl previous"
            ", XF86AudioNext, exec, playerctl next"
            ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
            ", XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
          ];
          bindle = [
            # why hyprland, must you have diffrent names for these
            ", XF86AudioRaiseVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 5%+"
            ", XF86AudioLowerVolume, exec, wpctl set-volume -l '1.0' @DEFAULT_AUDIO_SINK@ 5%-"
            ", XF86MonBrightnessUp, exec, brightnessctl s 5%+"
            ", XF86MonBrightnessDown, exec, brightnessctl s 5%-"
          ];
        };
      };
    }
  ];
}
