{
  pkgs,
  config,
  ...
}: {
  services.flatpak = {
    enable = true;
    packages = ["com.valvesoftware.Steam"];
    removeUnmanagedPackages = true;
    remote = {
      name = "flathub";
      url = "https://flathub.org/repo/flathub.flatpakrepo";
    };
  };
  systemd.services.flatpak-var = {
    wantedBy = ["multi-user.target"];
    path = [pkgs.flatpak];
    script = ''
      export XDG_DATA_DIRS="$XDG_DATA_DIRS:/usr/share:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share";
    '';
  };
}
