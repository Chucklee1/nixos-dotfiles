{lib, pkgs, def, ...}:
{
  programs = {
    fuzzel.enable = true;
    wlogout = {
      enable = true;
    };
    swaylock = {
      enable = true;
      package = pkgs.swaylock-effects;
    };
  };

  services = {
    dunst.enable = true;
    gnome-keyring.enable = true;
    swayidle = {
      enable = true;
    };
  };

  systemd.user.services.wlstart = {
    Unit.Description = "wayland startup service";
    Install.WantedBy = ["default.target"];
    Service = {
      ExecCondition = ''
        if [ "$XDG_SESSION_TYPE" == "wayland" ]; then
          exit 0
        else
          exit 1
        fi
      '';
      ExecStart = ''
        if systemctl --user list-units --type=service --all | grep -q 'waybar.service'; then
          systemctl --user restart waybar.service
        fi
        ${lib.getExe pkgs.networkmanagerapplet} &
        ${lib.getExe pkgs.wlsunset} -T 5200 &
        ${lib.getExe pkgs.swaybg} -i ${def.wallpaper} -m fill &
      '';
      Restart = "always";
      RestartSec = "2";
    };
  };
}
