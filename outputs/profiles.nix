{
  self,
  inputs,
  extlib,
  ...
}: {
  # ---- system  ----
  profiles = let
    extArgs = {inherit self inputs extlib;};
    mod = extlib.readDirRecursiveToAttrset "${self}/new_modules";
    load_mod_wrapper = mods: extlib.new_loadModules mods extArgs;
  in {
    desktop = {
      system = "x86_64-linux";
      modules = with mod; [ (load_mod_wrapper [ hosts.desktop

          net.mpd net.syncthing net.tailscale

          programs.editor.emacs programs.editor.nixvim programs.git
          programs.kitty programs.librewolf
          programs.niri
          programs.waybar
          programs.yazi

          software.apps
          software.dev
          software.flatpak
          software.gaming
          software.qol
          software.wayland

          system.boot
          system.home
          system.pkgconfig
          system.sys-specs
          system.users

          system.drivers.linux
          system.drivers.nvidia

          system.shell.variables
          system.shell.zsh

          system.theming.blockgame
          system.theming.stylix

          virt.qemu
      ]) ];
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
