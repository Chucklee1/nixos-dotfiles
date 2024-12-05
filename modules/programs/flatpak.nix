{pkgs, ...}: {
  services.flatpak.enable = true;
  systemd.services.flatpak-repo = {
    wantedBy = ["multi-user.target"];
    path = [pkgs.flatpak];
    script = ''
      flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
    '';
  };
  environment.systemPackages = [pkgs.gnome.gnomeSoftware];
  environment.variables = {
    XDG_DATA_DIRS = "${XDG_DATA_DIRS}:/usr/share:/var/lib/flatpak/exports/share:${config.home-manager.users.goat.homeDirectory}/.local/share/flatpak/exports/share";
  };
}
