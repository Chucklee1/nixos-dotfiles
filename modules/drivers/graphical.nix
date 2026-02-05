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

      # bluetooth
      hardware.bluetooth.enable = true;
      services.blueman.enable = true;

      # misc
      environment.systemPackages = with pkgs; [udisks mpv pavucontrol];
      programs.dconf.enable = true;
      services = {
        printing.enable = true;
        gvfs.enable = true;
      };
    })
  ];
}
