{self, ...}: {
  nix.global = [
    ({
      machine,
      ifSys,
      ...
    }: {
      environment = let
        root = "$HOME/nixos-dotfiles";
        buildFlags = "--show-trace --impure";
        buildType = ifSys.darwin "darwin" "nixos";
      in {
        variables = {
          BASH_SILENCE_DEPRECATION_WARNING = "1";
          TERMINAL = "kitty";
          EDITOR = "nvim";
        };
        shellAliases = {
          y = "yazi";
          ny = "cd ${root} && yazi";
          update-flake = "nix flake update --flake ${root}";
          rebuild-flake = "sudo ${buildType}-rebuild switch --flake ${root}#${machine} ${buildFlags}";
        };
      };
    })
  ];
  nix.macbook = [
    ({config, ...}:
      with config.lib.stylix.colors; let
        battery =
          # sh
          ''
            PERCENTAGE="$(pmset -g batt | grep -Eo "\d+%" | cut -d% -f1)"
            CHARGING="$(pmset -g batt | grep 'AC Power')"

            if [ "$PERCENTAGE" = "" ]; then
              exit 0
            fi

            case "$(PERCENTAGE)" in
              9[0-9]|100) ICON=""
              ;;
              [6-8][0-9]) ICON=""
              ;;
              [3-5][0-9]) ICON=""
              ;;
              [1-2][0-9]) ICON=""
              ;;
              *) ICON=""
            esac

            if [[ "$CHARGING" != "" ]]; then
              ICON=""
            fi

            # The item invoking this script (name $NAME) will get its icon and label
            # updated with the current battery status
            sketchybar --set "$NAME" icon="$ICON" label="$(PERCENTAGE)%"
          '';
        clock = ''sketchybar --set "$NAME" label="$(date '+%d/%m %H:%M')" '';
        front_app =
          # sh
          ''
            if [ "$SENDER" = "front_app_switched" ]; then
              sketchybar --set "$NAME" label="$INFO"
            fi
          '';
        space = ''sketchybar --set "$NAME" background.drawing="$SELECTED" '';
        volume =
          # sh
          ''
            if [ "$SENDER" = "volume_change" ]; then
              VOLUME="$INFO"

              case "$VOLUME" in
                [6-9][0-9]|100) ICON="󰕾"
                ;;
                [3-5][0-9]) ICON="󰖀"
                ;;
                [1-9]|[1-2][0-9]) ICON="󰕿"
                ;;
                *) ICON="󰖁"
              esac

              sketchybar --set "$NAME" icon="$ICON" label="$VOLUME%"
            fi
          '';
      in {
        services.jankyborders = {
          active_color = ''0xFF${base0D}'';
          inactive_color = ''0x00${base0D}'';
          style = "round";
          width = 4.0;
        };
        services.sketchybar.config =
          # sh
          ''
            sketchybar --bar position=top height=40 blur_radius=30 color=0x40${base00}

            default=(
              padding_left=5
              padding_right=5
              icon.font="JetBrainsMono Nerd Font:Bold:17.0"
              label.font="JetBrainsMono Nerd Font:Bold:14.0"
              icon.color=0xffffffff
              label.color=0xffffffff
              icon.padding_left=4
              icon.padding_right=4
              label.padding_left=4
              label.padding_right=4
            )
            sketchybar --default "$(default[@])"


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
                script="${space}"
                click_script="yabai -m space --focus $sid"
              )
              sketchybar --add space space."$sid" left --set space."$sid" "$(space[@])"
            done


            sketchybar --add item chevron left \
                       --set chevron icon= label.drawing=off \
                       --add item front_app left \
                       --set front_app icon.drawing=off script="${front_app}" \
                       --subscribe front_app front_app_switched


            sketchybar --add item clock right \
                       --set clock update_freq=10 icon=  script="${clock}" \
                       --add item volume right \
                       --set volume script="${volume}" \
                       --subscribe volume volume_change \
                       --add item battery right \
                       --set battery update_freq=120 script="${battery}" \
                       --subscribe battery system_woke power_source_change

            sketchybar --update
          '';
      })
  ];

  home.global = [
    {
      home.file.".config/rmpc".source = "${self}/assets/rmpc";
      home.file.".hammerspoon".source = "${self}/assets/hammerspoon";
      programs = {
        git.userEmail = "kermitthefrog@kakao.com";
        git.userName = "Chucklee1";
        lazygit.settings.notARepository = "skip";
        lazygit.settings.promptToReturnFromSubprocess = false;
        oh-my-posh.useTheme = "pure";
      };
    }
  ];
}
