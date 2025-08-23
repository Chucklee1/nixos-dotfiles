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
    desktop = {
      system = "x86_64-linux";
      modules = with mod; [
        global
        desktop
        linux
        gaming
        metal
        wayland
        virt
        drivers.nvidia
        additions.full
        editor.emacs
      ];
      user = "goat";
    };
    laptop = {
      system = "x86_64-linux";
      modules = with mod; [
        global
        laptop
        linux
        metal
        wayland
        additions.full
        editor.emacs
      ];
      user = "goat";
    };
    umbra = {
      system = "x86_64-linux";
      modules = with mod; [
        global
        umbra
        linux
        wayland
      ];
      user = "nixos";
    };
    macbook = {
      system = "aarch64-darwin";
      modules = with mod; [
        global
        macbook
        additions.full
        editor.emacs
      ];
      user = "goat";
    };
  };

  mkSystems = cfgs:
    inputs.nixpkgs.lib.mapAttrs (machine: cfg: let
      builder =
        if machine == "macbook"
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
