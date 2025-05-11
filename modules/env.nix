{
  nix.global = [
    ({
      lib,
      config,
      ...
    }: let
      cfg = opt: config.${opt}.enable == true;
    in
      with lib; {
        environment.variables =
          # niri wm
          mkIf cfg "niri" {
            XDG_CURRENT_DESKTOP = "niri";
            XDG_SESSION_DESKTOP = "niri";
            NIXOS_OZONE_WL = "1";
            MOZ_ENABLE_WAYLAND = "1";
            DISPLAY = ":0";
            _JAVA_AWT_WM_NONREPARENTING = "1";
            SDL_VIDEODRIVER = "x11";
            GDK_BACKEND = "wayland,x11";
            QT_QPA_PLATFORM = "wayland;xcb";
            QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
            QT_AUTO_SCREEN_SCALE_FACTOR = "1";
          }
          # nvidia
          // mkIf (cfg "nvidia" && cfg "niri") {
            GBM_BACKEND = "nvidia-drm";
            __GLX_VENDOR_LIBRARY_NAME = "nvidia";
          }
          // mkIf (cfg "nvidia") {
            LIBVA_DRIVER_NAME = "nvidia";
            NVD_BACKEND = "direct";
          };
      })
  ];
}
