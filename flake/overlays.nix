{
  inputs,
  self,
  ...
}: {
  x86_64-linux = [
    # small patch for sddm
    (final: _: {
      minesddm = inputs.minesddm.packages.${final.system}.default.overrideAttrs (old: {
        patches = (old.patches or []) ++ ["${self}/assets/patches/minesddm.patch"];
      });
    })
    # wayland
    inputs.niri.overlays.niri
    (final: _: {waybar_git = inputs.waybar.packages.${final.system}.waybar;})
  ];
}
