{
  self,
  nixpkgs,
  ...
} @ inputs: let
  # ---- additionals ----
  extlib = import ./libs.nix {inherit inputs self;};
  overlays = {
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
  };

  # ---- system  ----
  profiles = {
    desktop = {
      system = "x86_64-linux";
      user = "goat";
    };
    macbook = {
      system = "aarch64-darwin";
      user = "goat";
    };
  };

  mkSystem = machine: cfg: let
    builder = extlib.withSystem.ifDarwin cfg.system inputs.nix-darwin.lib.darwinSystem inputs.nixpkgs.lib.nixosSystem;
    mod = extlib.mergeProfiles "global" machine;
    specialArgs = {
      inherit machine;
      inherit (cfg) system user;
    };
  in
    builder {
      inherit (cfg) system;
      inherit specialArgs;
      overlays = [overlays.global overlays.${cfg.system}];
      modules =
        mod.nix
        ++ [{home-manager.extraSpecialArgs = specialArgs;}]
        ++ [{_module.args.homeMods = mod.home;}];
    };

  mkSystems = nixpkgs.lib.mapAttrs mkSystem profiles;
in {
  nixosConfigurations = mkSystems;
  darwinConfigurations = mkSystems;
}
