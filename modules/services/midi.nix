{self, ...}: {
  home = [
    ({pkgs, ...}: {
      home.packages = [
        pkgs.alsa-utils
        (pkgs.writeShellScriptBin "vmpk-wrapped" ''
          ${pkgs.vmpk}/bin/vmpk -f ${self}/assets/config/vmpk/launchkey.conf
        '')
      ];
      services.fluidsynth = {
        enable = true;
        soundService = "pipewire-pulse";
      };
    })
  ];
}
