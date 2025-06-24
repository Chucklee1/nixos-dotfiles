{
  nix.global = [
    (
      {
        machine,
        user,
        buildType,
        homeDir,
        ...
      }: {
        environment = let
          root = "${homeDir}/${user}/nixos-dotfiles";
          buildFlags = "--show-trace --impure";
        in {
          variables = {
            BASH_SILENCE_DEPRECATION_WARNING = "1";
            TERMINAL = "kitty";
            EDITOR = "nvim";
          };
          shellAliases = {
            update-flake = "nix flake update --flake ${root}";
            rebuild-flake = "sudo ${buildType}-rebuild switch --flake ${root}#${machine} ${buildFlags}";
          };
        };
      }
    )
  ];
  home.global = [
    ({
      lib,
      pkgs,
      ...
    }: {
      programs = {
        git.userEmail = "kermitthefrog@kakao.com";
        git.userName = "Chucklee1";
        lazygit.settings.notARepository = "skip";
        lazygit.settings.promptToReturnFromSubprocess = false;
        oh-my-posh.useTheme = "pure";
        yazi.plugins = lib.genAttrs ["mediainfo" "mount" "restore"] (pn: pkgs.yaziPlugins.${pn});
      };
    })
  ];
  home.macbook = [
    ({pkgs, ...}: {
      xdg.configFile = let
        launchApp = app: "/Applications/${app}.app/Contents/MacOS/${app}";
        kill = pkgs.writeText "kill.sh" ''
          window_pid=$(yabai -m query --windows --window | jq -r '.pid')
          count_pid=$(yabai -m query --windows | jq "[.[] | select(.pid == ''${window_pid})] | length")
          if [ "$count_pid" -gt 1 ]; then
            yabai -m window --close
          else
            kill "''${window_pid}"
          fi
        '';
      in {
        "skhd/sxhdrc".text = ''
          cmd - return : ${launchApp "kitty"} --single-instance -d ~
          cmd - q : ${kill}
          cmd - space : ${launchApp "dmenu-mac"}
          cmd - shift - f : ${launchApp "librewolf"}
        '';
        "yabai/yabairc".text = ''
          sudo yabai --load-sa
          yabai -m config auto_balance off
          yabai -m config bottom_padding 10
          yabai -m config layout bsp
          yabai -m config left_padding 10
          yabai -m config mouse_follows_focus off
          yabai -m config right_padding 10
          yabai -m config split_ratio 0.50
          yabai -m config top_padding 10
          yabai -m config window_border on
          yabai -m config window_border_width 2
          yabai -m config window_gap 10
          yabai -m config window_placement second_child


          yabai -m rule --add title='^(Opening)' manage=off
          yabai -m rule --add app="^System Settings$" manage=off
          yabai -m rule --add app="^Calculator$" manage=off
          yabai -m rule --add app="^App Store$" manage=off
          yabai -m rule --add app="^Disk Utility$" manage=off
          yabai -m rule --add app="^Calendar$" manage=off
          yabai -m rule --add app="^Activity Monitor$" manage=off
          yabai -m rule --add app="^Weather$" manage=off
          # yabai -m rule --add app="^Finder$" manage=off
          yabai -m rule --add app="^Garmin Express$" manage=off
          yabai -m rule --add app="^Creative Cloud$" manage=off
        '';
      };

      # relaunch issues
      home.activation = {
        skhd-reloader = ''
          run skhd -r
        '';
        yabai-reloader = ''
          run yabai --restart-service
          run sudo yabai --load-sa
        '';
      };
    })
  ];
}
