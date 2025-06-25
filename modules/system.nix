{inputs, ...}: let
  linuxNix = [
    # ---- system ----
    inputs.home-manager.nixosModules.home-manager
    ({
      lib,
      user,
      machine,
      ...
    }: {
      # boot
      boot.loader = {
        efi.canTouchEfiVariables = true;
        grub = {
          enable = true;
          efiSupport = true;
          device = "nodev";
        };
      };

      # general
      system.stateVersion = "24.05";
      networking = {
        useDHCP = lib.mkDefault true;
        hostName = "${user}-${machine}";
        networkmanager.enable = true;
      };
      i18n.defaultLocale = "en_CA.UTF-8";
      time.timeZone = "America/Vancouver";

      # user
      users.users.${user} = {
        name = "${user}";
        isNormalUser = true;
        extraGroups = [
          "wheel"
          "networkmanager"
          "audio"
          "video"
          "libvirtd"
        ];
      };
    })
    # ---- higher-level drivers ----
    ({pkgs, ...}: {
      # gpu
      hardware.graphics = {
        enable = true;
        enable32Bit = true;
        extraPackages = with pkgs; [
          vulkan-tools
          vulkan-loader
          libvdpau-va-gl
        ];
      };

      # audio
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
      };

      # bluetooth
      hardware.bluetooth.enable = true;
      services.blueman.enable = true;

      # ssh
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "prohibit-password";
        };
      };

      # misc
      services = {
        printing.enable = true;
        fstrim.enable = true;
        tumbler.enable = true;
        gvfs.enable = true;
      };
    })
  ];
in {
  nix.global = [
    ({
      lib,
      config,
      system,
      user,
      ...
    }: {
      # pkg conf
      nix.settings.experimental-features = "nix-command flakes";
      nix.gc = {
        automatic = lib.mkDefault true;
        options = lib.mkDefault "--delete-older-than 7d";
      };
      nixpkgs = {
        hostPlatform = "${system}";
        config.allowUnfree = true;
      };
      # home manager
      home-manager.useGlobalPkgs = true;
      home-manager.users.${user}.imports = config._module.args.homeMods;
    })
  ];
  nix.desktop =
    linuxNix
    ++ [
      # gpu
      ({
        lib,
        config,
        ...
      }: {
        nixpkgs.config.nvidia.acceptLicense = true;
        services.xserver.videoDrivers = ["nvidia"];
        hardware.nvidia = {
          modesetting.enable = true;
          package = config.boot.kernelPackages.nvidiaPackages.beta;
          videoAcceleration = true;
          open = false;
        };
        environment.variables =
          {
            LIBVA_DRIVER_NAME = "nvidia";
            NVD_BACKEND = "direct";
          }
          // lib.mkIf config.programs.niri.enable {
            GBM_BACKEND = "nvidia-drm";
            __GLX_VENDOR_LIBRARY_NAME = "nvidia";
          };
      })
      # tablet support
      {
        hardware.uinput.enable = true;
        programs.weylus.enable = true;
        services.udev.extraRules = ''
          KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
        '';
      }
    ];

  nix.macbook = [
    inputs.home-manager.darwinModules.home-manager
    ({
      pkgs,
      user,
      ...
    }: {
      # nix issue fix
      nix.settings = {auto-optimise-store = false;};
      system = {
        # general
        stateVersion = 6;
        primaryUser = "${user}";
      };
      system.defaults = {
        WindowManager.StandardHideDesktopIcons = true;

        dock = {
          autohide = true;
          dashboard-in-overlay = true; # Don't show dashboard as a space
          mru-spaces = false; # Don't rearrange spaces based on most recently used
          show-recents = false; # don't show recent apps
          static-only = false; # show only running apps
          # Disable all hot corners
          wvous-tl-corner = 1;
          wvous-tr-corner = 1;
          wvous-bl-corner = 1;
          wvous-br-corner = 1;
        };

        finder = {
          AppleShowAllExtensions = true;
          AppleShowAllFiles = true;
          ShowPathbar = true;
        };

        NSGlobalDomain = {
          AppleInterfaceStyle = "Dark";
          AppleInterfaceStyleSwitchesAutomatically = false;
          # expanded save panel by default
          NSNavPanelExpandedStateForSaveMode = true;
          NSNavPanelExpandedStateForSaveMode2 = true;
          NSDocumentSaveNewDocumentsToCloud = false; # do not save to icloud by default
          "com.apple.swipescrolldirection" = false; # Disable "natural" scrolling
          AppleKeyboardUIMode = 3; # full keyboard access
        };

        # misc
        CustomUserPreferences = {
          "com.apple.desktopservices".DSDontWriteNetworkStores = true;
          "com.apple.desktopservices".DSDontWriteUSBStores = true;
          "com.apple.AdLib".allowApplePersonalizedAdvertising = false;
        };
      };

      # Add ability to used TouchID for sudo authentication
      security.pam.services.sudo_local.touchIdAuth = true;

      # user
      users.knownUsers = ["goat"];
      users.users.${user} = {
        uid = 501;
        name = "${user}";
        home = "/Users/${user}";
        shell = pkgs.bash;
        ignoreShellProgramCheck = true;
      };
    })
    # nix managed services
    {
      services = {
        tailscale.enable = true;
        aerospace.enable = true;
        sketchybar.enable = true;
        sketchybar.config =
          # bash
          ''
            # This is a demo config to showcase some of the most important commands.
            # It is meant to be changed and configured, as it is intentionally kept sparse.
            # For a (much) more advanced configuration example see my dotfiles:
            # https://github.com/FelixKratz/dotfiles

            PLUGIN_DIR="$CONFIG_DIR/plugins"

            ##### Bar Appearance #####
            # Configuring the general appearance of the bar.
            # These are only some of the options available. For all options see:
            # https://felixkratz.github.io/SketchyBar/config/bar
            # If you are looking for other colors, see the color picker:
            # https://felixkratz.github.io/SketchyBar/config/tricks#color-picker

            sketchybar --bar position=top height=40 blur_radius=30 color=0x40000000

            ##### Changing Defaults #####
            # We now change some default values, which are applied to all further items.
            # For a full list of all available item properties see:
            # https://felixkratz.github.io/SketchyBar/config/items

            default=(
              padding_left=5
              padding_right=5
              icon.font="Hack Nerd Font:Bold:17.0"
              label.font="Hack Nerd Font:Bold:14.0"
              icon.color=0xffffffff
              label.color=0xffffffff
              icon.padding_left=4
              icon.padding_right=4
              label.padding_left=4
              label.padding_right=4
            )
            sketchybar --default "$(default[@])"

            ##### Adding Mission Control Space Indicators #####
            # Let's add some mission control spaces:
            # https://felixkratz.github.io/SketchyBar/config/components#space----associate-mission-control-spaces-with-an-item
            # to indicate active and available mission control spaces.

            SPACE_ICONS=("1" "2" "3" "4" "5" "6" "7" "8" "9" "10")
            for i in "$(!SPACE_ICONS[@])"
            do
              sid="$(($i+1))"
              space=(
                space="$sid"
                icon="$(SPACE_ICONS[i])"
                icon.padding_left=7
                icon.padding_right=7
                background.color=0x40ffffff
                background.corner_radius=5
                background.height=25
                label.drawing=off
                script="$PLUGIN_DIR/space.sh"
                click_script="yabai -m space --focus $sid"
              )
              sketchybar --add space space."$sid" left --set space."$sid" "$(space[@])"
            done

            ##### Adding Left Items #####
            # We add some regular items to the left side of the bar, where
            # only the properties deviating from the current defaults need to be set

            sketchybar --add item chevron left \
                       --set chevron icon= label.drawing=off \
                       --add item front_app left \
                       --set front_app icon.drawing=off script="$PLUGIN_DIR/front_app.sh" \
                       --subscribe front_app front_app_switched

            ##### Adding Right Items #####
            # In the same way as the left items we can add items to the right side.
            # Additional position (e.g. center) are available, see:
            # https://felixkratz.github.io/SketchyBar/config/items#adding-items-to-sketchybar

            # Some items refresh on a fixed cycle, e.g. the clock runs its script once
            # every 10s. Other items respond to events they subscribe to, e.g. the
            # volume.sh script is only executed once an actual change in system audio
            # volume is registered. More info about the event system can be found here:
            # https://felixkratz.github.io/SketchyBar/config/events

            sketchybar --add item clock right \
                       --set clock update_freq=10 icon=  script="$PLUGIN_DIR/clock.sh" \
                       --add item volume right \
                       --set volume script="$PLUGIN_DIR/volume.sh" \
                       --subscribe volume volume_change \
                       --add item battery right \
                       --set battery update_freq=120 script="$PLUGIN_DIR/battery.sh" \
                       --subscribe battery system_woke power_source_change

            ##### Force all scripts to run the first time (never do this in a script) #####
            sketchybar --update
          '';
      };
    }
  ];

  home.global = [
    ({
      user,
      homeDir,
      ...
    }: {
      home = {
        stateVersion = "24.05"; # DO NOT CHANGE
        username = user;
        homeDirectory = "${homeDir}/${user}";
      };
    })
  ];
}
