{
  self,
  inputs,
  extlib,
  ...
}: {
  # ---- system  ----
  profiles = let
    mod = extlib.loadModules "${self}/modules" {inherit inputs self;};
    new_mod = extlib.readDirRecursiveToAttrset "${self}/new_modules";
  in {
    desktop = {
      system = "x86_64-linux";
      modules = with mod; [
        global desktop linux metal
        virt.qemu drivers.nvidia
        gaming wayland additions.full
        editor.nixvim editor.emacs
        # testing new function
        (extlib.new_loadModules [
          new_mod.niri
          new_mod.waybar
          new_mod.yazi new_mod.librewolf
        ] {inherit self inputs;})
      ];
      user = "goat";
    };
    laptop = {
      system = "x86_64-linux";
      modules = with mod; [
        global laptop linux metal
        wayland additions.full
        niri waybar
        editor.emacs
      ];
      user = "goat";
    };
    inspiron = {
      system = "x86_64-linux";
      modules = with mod; [
        global inspiron linux metal
        additions.full
        dwm
        editor.emacs
      ];
      user = "goat";
    };
    umbra = {
      system = "x86_64-linux";
      modules = with mod; [
        global umbra linux
        wayland
        niri waybar
      ];
      user = "nixos";
    };
    macbook = {
      system = "aarch64-darwin";
      modules = with mod; [
        global macbook
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
        inherit self inputs extlib machine;
        spkgs = import inputs.nixpkgs-stable {inherit (cfg) system;};
        inherit (cfg) system user;
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
