{
  self,
  inputs,
  extlib,
  ...
}: {
  # ---- system  ----
  profiles = let
    mod = extlib.loadModules "${self}/modules" {inherit inputs self;};
  in {
    nixos = {
      desktop = {
        system = "x86_64-linux";
        modules = with mod; [desktop global gaming linux metal wayland drivers.nvidia];
        user = "goat";
      };
      laptop = {
        system = "x86_64-linux";
        modules = with mod; [laptop global linux metal wayland];
        user = "goat";
      };
      umbra = {
        system = "x86_64-linux";
        modules = with mod; [umbra global linux wayland];
        user = "nixos";
      };
    };
    darwin.macbook = {
      system = "aarch64-darwin";
      modules = with mod; [global macbook];
      user = "goat";
    };
  };

  mkSystems = cfgs:
    inputs.nixpkgs.lib.mapAttrs (machine: cfg: let
      builder =
        if cfgs ? darwin
        then inputs.nix-darwin.lib.darwinSystem
        else inputs.nixpkgs.lib.nixosSystem;
      mod = {
        nix = builtins.concatLists (map (m: m.nix or []) cfg.modules);
        home = builtins.concatLists (map (m: m.home or []) cfg.modules);
      };
      specialArgs = {
        inherit machine;
        inherit (cfg) system user;
        MOL = A: B: extlib.withSystem.ifDarwinElseLinux cfg.system A B;
      };
    in
      builder {
        inherit (cfg) system;
        inherit specialArgs;
        modules =
          builtins.concatLists
          [
            mod.nix
            [{_module.args.homeMods = mod.home;}]
            [{home-manager.extraSpecialArgs = specialArgs;}]
          ];
      })
    cfgs;
}
