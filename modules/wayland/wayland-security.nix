{pkgs, ...}: {
  services.gnome.gnome-keyring.enable = true;
  security = {
    rtkit.enable = true; # enable rtkit for sound
    polkit.enable = true; # enable policykit
    pam.services.swaylock = {
      text = ''
        auth include login
      '';
    };
  };

  systemd = {
    user.services.lxqt-policykit-agent = {
      description = "LXQt PolicyKit Agent";
      serviceConfig.ExecStart = "${pkgs.lxqt.lxqt-policykit}/bin/lxqt-policykit-agent";
      wantedBy = ["default.target"];
    };
  };
}
