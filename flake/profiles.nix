{self, extlib, ...}: let
  inherit (self) inputs;
in {
  # ---- system  ----
  profiles = let
    mod = extlib.readDirRecursiveToAttrset "${self}/modules";
  in {
    desktop = {
      system = "x86_64-linux";
      modules = with mod; [
        hosts.desktop
        net.syncthing net.tailscale

        programs.librewolf programs.emacs programs.nixvim
        programs.git programs.kitty programs.yazi
        programs.niri programs.waybar

        software.apps software.dev
        software.texlive software.java
        software.gaming software.qol
        software.wayland

        system.boot system.home system.users
        system.pkgconfig system.sys-specs

        drivers.graphical drivers.ssh

        shell.variables shell.zsh

        theming.blockgame theming.stylix

        virt.qemu
      ];
      user = "goat";
    };
    laptop = {
      system = "x86_64-linux";
      modules = with mod; [
        hosts.laptop
        net.syncthing net.tailscale

        programs.librewolf programs.emacs
        programs.git programs.kitty programs.yazi
        programs.niri programs.waybar

        software.apps software.dev software.texlive
        software.qol
        software.wayland

        system.boot system.home system.users
        system.pkgconfig system.sys-specs

        drivers.graphical drivers.ssh

        shell.variables shell.zsh

        theming.blockgame theming.stylix
      ];
      user = "goat";
    };
    inspiron = {
      system = "x86_64-linux";
      modules = with mod; [
        hosts.inspiron

        net.syncthing net.tailscale net.the-server

        programs.librewolf
        programs.nixvim
        programs.git programs.kitty programs.yazi
        programs.dwm

        software.apps software.dev software.qol

        system.boot system.home system.users
        system.pkgconfig system.sys-specs

        drivers.graphical drivers.ssh drivers.ext4

        shell.variables shell.zsh

        theming.blockgame theming.stylix
      ];
      user = "goat";
    };
    umbra = {
      system = "x86_64-linux";
      modules = with mod; [
        hosts.umbra
        system.boot system.home
        system.pkgconfig system.sys-specs
        drivers.ssh
        shell.zsh programs.nixvim
      ];
      user = "nixos";
    };
    macbook = {
      system = "aarch64-darwin";
      modules = with mod; [
        hosts.macbook

        net.tailscale

        programs.emacs
        programs.git programs.kitty programs.yazi

        software.dev software.qol software.texlive software.java

        system.home system.pkgconfig

        shell.variables shell.zsh

        theming.stylix
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
      # modules
      inj_mods = [(extlib.loadModulesFromAttrset
        cfg.modules
        {inherit self inputs extlib; inherit (cfg) system;}
      )];
      mod = {
        nix = builtins.concatLists (map (m: m.nix or []) inj_mods);
        home = builtins.concatLists (map (m: m.home or []) inj_mods);
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
