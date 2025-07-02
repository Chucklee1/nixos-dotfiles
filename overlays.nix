{
  inputs,
  self,
  extlib,
  ...
}: {
  x86_64-linux = [
    # small patch for sddm
    (final: _: {
      minesddm = inputs.minesddm.packages.${final.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
        patches = (old.patches or []) ++ ["${self}/assets/patches/minesddm.patch"];
      });
    })
    # wayland
    inputs.niri.overlays.niri
    (final: _: {waybar_git = inputs.waybar.packages.${final.stdenv.hostPlatform.system}.waybar;})
  ];
  global = [
    # custom libs
    (final: _: {
      extlib =
        extlib
        # so I do not need to pass systeme every time
        // {
          ifLinux = A: B:
            extlib.withSystem.isLinux final.stdenv.hostPlatform.system A B;
          ifDarwin = A: B:
            extlib.withSystem.isDarwin final.stdenv.hostPlatform.system A B;
        };
    })
  ];
}
