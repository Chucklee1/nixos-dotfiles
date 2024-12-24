{inputs, ...}: {
  universal.modules = [inputs.niri.nixosModules.niri];
  personal.modules = [
    ({pkgs, ...}: {
      programs.niri.enable = true;
      nixpkgs.overlays = [niri.overlays.niri];
      programs.niri.package = pkgs.niri-unstable;
      environment.variables.NIXOS_OZONE_WL = "1";
      environment.systemPackages = with pkgs; [
        wl-clipboard
        wayland-utils
        libsecret
        cage
        gamescope
        xwayland-satellite-unstable
        swaybg
      ];
    })
  ];

  personal.home_modules = [
    ({
      lib,
      nixosConfig,
      config,
      pkgs,
      ...
    }:
      with lib; let
        binds = {
          suffixes,
          prefixes,
          substitutions ? {},
        }: let
          replacer = replaceStrings (attrNames substitutions) (attrValues substitutions);
          format = prefix: suffix: let
            actual-suffix =
              if isList suffix.action
              then {
                action = head suffix.action;
                args = tail suffix.action;
              }
              else {
                inherit (suffix) action;
                args = [];
              };

            action = replacer "${prefix.action}-${actual-suffix.action}";
          in {
            name = "${prefix.key}+${suffix.key}";
            value.action.${action} = actual-suffix.args;
          };
          pairs = attrs: fn:
            concatMap (key:
              fn {
                inherit key;
                action = attrs.${key};
              }) (attrNames attrs);
        in
          listToAttrs (pairs prefixes (prefix: pairs suffixes (suffix: [(format prefix suffix)])));
      in {
        programs.niri.settings = {
          input.keyboard.xkb.layout = "no";
          input.mouse.accel-speed = 1.0;
          input.touchpad = {
            tap = true;
            dwt = true;
            natural-scroll = true;
            click-method = "clickfinger";
          };

          input.tablet.map-to-output = "eDP-1";
          input.touch.map-to-output = "eDP-1";

          prefer-no-csd = true;

          layout = {
            gaps = 16;
            struts.left = 64;
            struts.right = 64;
            border.width = 4;
            always-center-single-column = true;
          };

          hotkey-overlay.skip-at-startup = true;
          screenshot-path = "~/Pictures/Screenshots/%Y-%m-%dT%H:%M:%S.png";

          switch-events = with config.lib.niri.actions; let
            sh = spawn "sh" "-c";
          in {
            tablet-mode-on.action = sh "notify-send tablet-mode-on";
            tablet-mode-off.action = sh "notify-send tablet-mode-off";
            lid-open.action = sh "notify-send lid-open";
            lid-close.action = sh "notify-send lid-close";
          };

          binds = with config.lib.niri.actions; let
            sh = spawn "sh" "-c";
            Mod = "alt";
          in
            lib.attrsets.mergeAttrsList [
              {
                "${Mod}+Return".action = spawn "kitty";
                "${Mod}+Space".action = spawn "fuzzel";
                "${Mod}+W".action = sh (builtins.concatStringsSep "; " [
                  "systemctl --user restart waybar.service"
                  "systemctl --user restart swaybg.service"
                ]);

                "${Mod}+L".action = spawn "blurred-locker";

                "${Mod}+Shift+S".action = screenshot;
                "Print".action = screenshot-screen;
                "${Mod}+Print".action = screenshot-window;

                "XF86AudioRaiseVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+";
                "XF86AudioLowerVolume".action = sh "wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-";
                "XF86AudioMute".action = sh "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";

                "XF86MonBrightnessUp".action = sh "brightnessctl set 10%+";
                "XF86MonBrightnessDown".action = sh "brightnessctl set 10%-";

                "${Mod}+Q".action = close-window;

                "XF86AudioNext".action = focus-column-right;
                "XF86AudioPrev".action = focus-column-left;

                "${Mod}+Tab".action = focus-window-down-or-column-right;
                "${Mod}+Shift+Tab".action = focus-window-up-or-column-left;
              }
              (binds {
                suffixes."Left" = "column-left";
                suffixes."Down" = "window-down";
                suffixes."Up" = "window-up";
                suffixes."Right" = "column-right";
                prefixes."${Mod}" = "focus";
                prefixes."${Mod}+Ctrl" = "move";
                prefixes."${Mod}+Shift" = "focus-monitor";
                prefixes."${Mod}+Shift+Ctrl" = "move-window-to-monitor";
                substitutions."monitor-column" = "monitor";
                substitutions."monitor-window" = "monitor";
              })
              (binds {
                suffixes."Home" = "first";
                suffixes."End" = "last";
                prefixes."${Mod}" = "focus-column";
                prefixes."${Mod}+Ctrl" = "move-column-to";
              })
              (binds {
                suffixes."U" = "workspace-down";
                suffixes."I" = "workspace-up";
                prefixes."${Mod}" = "focus";
                prefixes."${Mod}+Ctrl" = "move-window-to";
                prefixes."${Mod}+Shift" = "move";
              })
              (binds {
                suffixes = builtins.listToAttrs (map (n: {
                  name = toString n;
                  value = ["workspace" n];
                }) (range 1 9));
                prefixes."${Mod}" = "focus";
                prefixes."${Mod}+Ctrl" = "move-window-to";
              })
              {
                "${Mod}+Comma".action = consume-window-into-column;
                "${Mod}+Period".action = expel-window-from-column;

                "${Mod}+R".action = switch-preset-column-width;
                "${Mod}+F".action = maximize-column;
                "${Mod}+Shift+F".action = fullscreen-window;
                "${Mod}+C".action = center-column;

                "${Mod}+Minus".action = set-column-width "-10%";
                "${Mod}+Plus".action = set-column-width "+10%";
                "${Mod}+Shift+Minus".action = set-window-height "-10%";
                "${Mod}+Shift+Plus".action = set-window-height "+10%";

                "${Mod}+Shift+E".action = quit;
                "${Mod}+Shift+P".action = power-off-monitors;

                "${Mod}+Shift+Ctrl+T".action = toggle-debug-tint;
              }
            ];

          spawn-at-startup = let
            modulated-wallpaper = pkgs.runCommand "modulated-wallpaper.png" {} ''
              ${lib.getExe pkgs.imagemagick} ${config.stylix.image} -modulate 100,100,14 $out
            '';
          in [
            {
              command = [
                "${lib.getExe pkgs.wlsunset}"
                "-T"
                "5200"
              ];
            }
            {
              command = [
                "${lib.getExe pkgs.waybar}"
              ];
            }
            {
              command = [
                "${lib.getExe pkgs.swaybg}"
                "-m"
                "fill"
                "-i"
                "${modulated-wallpaper}"
              ];
            }
            {
              command = let
                units = [
                  "niri"
                  "graphical-session.target"
                  "xdg-desktop-portal"
                  "xdg-desktop-portal-gtk"
                  "waybar"
                ];
                commands = builtins.concatStringsSep ";" (map (unit: "systemctl --user status ${unit}") units);
              in ["kitty" "--" "sh" "-c" "env SYSTEMD_COLORS=1 watch -n 1 -d --color '${commands}'"];
            }
          ];

          animations.shaders.window-resize = ''
            vec4 resize_color(vec3 coords_curr_geo, vec3 size_curr_geo) {
                vec3 coords_next_geo = niri_curr_geo_to_next_geo * coords_curr_geo;

                vec3 coords_stretch = niri_geo_to_tex_next * coords_curr_geo;
                vec3 coords_crop = niri_geo_to_tex_next * coords_next_geo;

                // We can crop if the current window size is smaller than the next window
                // size. One way to tell is by comparing to 1.0 the X and Y scaling
                // coefficients in the current-to-next transformation matrix.
                bool can_crop_by_x = niri_curr_geo_to_next_geo[0][0] <= 1.0;
                bool can_crop_by_y = niri_curr_geo_to_next_geo[1][1] <= 1.0;

                vec3 coords = coords_stretch;
                if (can_crop_by_x)
                    coords.x = coords_crop.x;
                if (can_crop_by_y)
                    coords.y = coords_crop.y;

                vec4 color = texture2D(niri_tex_next, coords.st);

                // However, when we crop, we also want to crop out anything outside the
                // current geometry. This is because the area of the shader is unspecified
                // and usually bigger than the current geometry, so if we don't fill pixels
                // outside with transparency, the texture will leak out.
                //
                // When stretching, this is not an issue because the area outside will
                // correspond to client-side decoration shadows, which are already supposed
                // to be outside.
                if (can_crop_by_x && (coords_curr_geo.x < 0.0 || 1.0 < coords_curr_geo.x))
                    color = vec4(0.0);
                if (can_crop_by_y && (coords_curr_geo.y < 0.0 || 1.0 < coords_curr_geo.y))
                    color = vec4(0.0);

                return color;
            }
          '';

          window-rules = let
            colors = config.lib.stylix.colors.withHashtag;
          in [
            {
              draw-border-with-background = false;
              geometry-corner-radius = let
                r = 2.0;
              in {
                top-left = r;
                top-right = r;
                bottom-left = r;
                bottom-right = r;
              };
              clip-to-geometry = true;
            }
            {
              matches = [
                {
                  app-id = "^signal$";
                }
              ];
              block-out-from = "screencast";
            }
          ];
        };
      })
  ];
}
