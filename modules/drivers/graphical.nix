{
  nix = [
    ({
      lib,
      pkgs,
      ...
    }: {
      # gpu
      hardware.graphics =
        {
          enable = true;
          extraPackages = with pkgs; [
            libvdpau-va-gl
            vulkan-tools
          ];
        }
        // (lib.mkIf pkgs.stdenv.isx86_64 {enable32Bit = true;});

      # audio
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
      };

      # polkit
      security.polkit.enable = true;

      # portals
      xdg.portal.extraPortals = [
        pkgs.xdg-desktop-portal-gnome
        pkgs.xdg-desktop-portal-gtk
      ];
      xdg.portal.config.common.default = "gnome";

      # specific apps require a gui file manager for xdg-open
      # (cough cough steam)
      programs.thunar.enable = true;
      programs.thunar.plugins = [pkgs.thunar-volman];
      xdg.mime.defaultApplications = {
        "inode/directory" = "thunar.desktop";
      };


      # bluetooth
      hardware.bluetooth.enable = true;
      services.blueman.enable = true;

      # misc
      environment.systemPackages = with pkgs; [
        xdg-utils
        udisks
        mpv
        pavucontrol
      ];
      programs.dconf.enable = true;
      services = {
        printing.enable = true;
        gvfs.enable = true;
      };
    })
  ];
}
