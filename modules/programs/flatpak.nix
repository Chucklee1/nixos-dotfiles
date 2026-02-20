{inputs, ...}: {
  nix = [
    inputs.nix-flatpak.nixosModules.nix-flatpak
    ({pkgs, ...}: {
      services.flatpak = {
        enable = true;
        packages = [
          rec {
            appId = "com.hypixel.HytaleLauncher";
            sha256 = "sha256-9lKXjb05cM/sOucUPbFmSLIsh8kItLl8V8Rou8ccJew=";
            bundle = "${pkgs.fetchurl {
              url = "https://launcher.hytale.com/builds/release/linux/amd64/hytale-launcher-latest.flatpak";
              inherit sha256;
            }}";
          }
          "us.zoom.Zoom"
          "io.qt.QtCreator"
        ];
        overrides = {
          global = {
            # Force Wayland by default
            Context.sockets = ["wayland" "!x11" "!fallback-x11"];

            Environment = {
              # Fix un-themed cursor in some Wayland apps
              XCURSOR_PATH = "/run/host/user-share/icons:/run/host/share/icons";

              # Force correct theme for some GTK apps
              GTK_THEME = "Adwaita:dark";
            };
          };
          # No Wayland support
          "com.hypixel.HytaleLauncher".Context.sockets = ["x11"];
          "us.zoom.Zoom".Context.sockets = ["x11"];
        };
      };

      # bin scripts for dmemu/wmenu to access
      environment.systemPackages = [
        (pkgs.writeShellScriptBin "hytale" "flatpak run com.hypixel.HytaleLauncher")
        (pkgs.writeShellScriptBin "zoom" "flatpak run us.zoom.Zoom")
        (pkgs.writeShellScriptBin "qtcreator" "flatpak run io.qt.qtCreator")
      ];
    })
  ];
}
