{
  nix = [
    ({lib, pkgs, ...}: {
      # gpu
      hardware.graphics = {
        enable = true;
        extraPackages = with pkgs; [
          libvdpau-va-gl
          vulkan-tools
        ];
      } // (lib.mkIf pkgs.stdenv.isx86_64 {enable32Bit = true;});

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
      services = {
        printing.enable = true;
        gvfs.enable = true;
      };
    })
  ];
}
