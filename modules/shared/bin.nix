self: super: {
  get-volume = super.writeShellApplication rec {
    name = "get-volume";
    text = ''
      wpctl() {
          arg="$2"

          set -- "$1"
          if [ "$1" = set-volume ]; then
              set -- "$@" --limit=1.0
          fi

          set -- "$@" @DEFAULT_AUDIO_SINK@ "$arg"

          command wpctl "$@"
      }

      if wpctl get-volume | grep -q "[MUTED]"; then
          display " " 9
          exit
      fi

      volume_frac="$(wpctl get-volume | cut -d' ' -f2)"
      volume="$(echo "$volume_frac * 100 / 1" | bc)"

      if [ -z "$volume" ]; then
          exit 1
      fi

      if [ "$volume" -gt 40 ]; then
          icon=" "
      elif [ "$volume" -gt 15 ]; then
          icon=" "
      else
          icon=" "
      fi

      display "$icon$volume%"
    '';
  };

  get-brightness = super.writeShellApplication rec {
    name = "get-brightness";
    text = ''
      curr_brightness=$(cat /sys/class/backlight/*/brightness)
      max_brightness=$(cat /sys/class/backlight/*/max_brightness)
      brightness_per=$((100 * curr_brightness / max_brightness))
      echo "󰃞 $brightness_per %"
    '';
  };

  get-battery = super.writeShellApplicaiton rec {
    name = "get-battery";
    text = ''
      for battery in /sys/class/power_supply/BAT?*; do
          [ -n "$capacity + x" ] && printf " "

          capacity="$(cat "$battery/capacity" 2>&1)"
          if [ "$capacity" -gt 90 ]; then
              status=" "
          elif [ "$capacity" -gt 60 ]; then
              status=" "
          elif [ "$capacity" -gt 40 ]; then
              status=" "
          elif [ "$capacity" -gt 10 ]; then
              status=" "
          else
              status=" "
          fi

          case "$(cat "$battery/status" 2>&1)" in
              Full) status=" " ;;
              Discharging)
                  if [ "$capacity" -le 20 ]; then
                      status="$status"
                      color=1
                  fi
                  ;;
              Charging) status="󰚥$status" ;;
              "Not charging") status=" " ;;
              Unknown) status="? $status" ;;
              *) exit 1 ;;
          esac

          display "$status$capacity%"
      done
    '';
  };

  get-net = super.writeShellApplication rec {
    name = "get-net";
    text = ''
      if [ "$(cat /sys/class/net/w*/operstate 2>/dev/null)" = 'up' ] ; then
	      wifiicon="$(awk '/^\s*w/ { print "󰖩", int($3 * 100 / 70) "% " }' /proc/net/wireless)"
      elif [ "$(cat /sys/class/net/w*/operstate 2>/dev/null)" = 'down' ] ; then
	      [ "$(cat /sys/class/net/w*/flags 2>/dev/null)" = '0x1003' ] && wifiicon="󱛄 " || wifiicon="󰖪 "
      fi
      [ "$(cat /sys/class/net/e*/operstate 2>/dev/null)" = 'up' ] && ethericon="󰈁" || ethericon="󰈂"
      printf "%s%s%s\n" "$wifiicon" "$ethericon" 
    '';
  };

  get-date = super.writeshellApplication rec {
    name = "get-date";
    text = ''
      display "󰥔 $(date '+%H:%M:%S')"
    '';
  };
}
