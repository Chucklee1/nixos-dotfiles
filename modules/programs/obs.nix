{
  nix = [
    # virtual camera support
    ({lib, config, pkgs, ...}: {
      boot.extraModulePackages = with config.boot.kernelPackages; lib.mkAfter [
        v4l2loopback
      ];
      boot.extraModprobeConfig = lib.mkAfter ''
          options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
        '';

      # obs with needed plugins
      environment.systemPackages = [
        (pkgs.wrapOBS {
          plugins = with pkgs.obs-studio-plugins; [
            wlrobs
            obs-vaapi
            obs-pipewire-audio-capture
          ];
        })
      ];
    })
  ];
}
