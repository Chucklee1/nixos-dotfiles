{self, extlib, ...}: let
  inherit (self) inputs;
in {
  # ---- system  ----
  profiles = let
    mod = extlib.readDirRecursiveToAttrset "${self}/modules";
  in {
    desktop = {
      system = "x86_64-linux";
      builder = inputs.nixpkgs.lib.nixosSystem;
      user = "goat";
      modules = with mod; [
        hosts.desktop
        net.syncthing net.tailscale

        programs.zen-browser programs.emacs programs.nixvim
        programs.git programs.kitty programs.yazi
        software.wayland programs.niri programs.quickshell
        programs.prismLauncher programs.flatpak

        software.apps software.dev
        software.texlive software.java software.rust
        software.gaming software.qol

        system.boot system.home system.users
        system.pkgconfig system.sys-specs

        drivers.graphical drivers.ssh

        shell.variables shell.zsh shell.nushell

        theming.blockgame theming.stylix theming.themes.nord

        virt.qemu
      ];
    };
    laptop = {
      system = "x86_64-linux";
      builder = inputs.nixpkgs.lib.nixosSystem;
      user = "goat";
      modules = with mod; [
        hosts.laptop
        net.syncthing net.tailscale

        programs.zen-browser programs.emacs
        programs.git programs.kitty programs.yazi
	      software.wayland programs.niri programs.quickshell

        software.apps software.dev software.texlive
        software.qol

        system.boot system.home system.users
        system.pkgconfig system.sys-specs
        disko.disko disko.ext4

        drivers.graphical drivers.ssh

        shell.variables shell.zsh

        theming.blockgame theming.stylix theming.themes.nord
      ];
    };
    inspiron = {
      system = "x86_64-linux";
      builder = inputs.nixpkgs.lib.nixosSystem;
      user = "goat";
      modules = with mod; [
        hosts.inspiron

        net.syncthing net.tailscale net.the-server

        programs.librewolf
        programs.nixvim
        programs.git programs.kitty programs.yazi
        programs.xserver programs.dwm

        software.apps software.dev software.qol

        system.boot system.home system.users
        system.pkgconfig system.sys-specs

        drivers.graphical drivers.ssh drivers.ext4

        shell.variables shell.zsh

        theming.blockgame theming.stylix theming.themes.nord
      ];
    };
    umbra = {
      system = "x86_64-linux";
      builder = inputs.nixpkgs.lib.nixosSystem;
      user = "nixos";
      modules = with mod; [
        hosts.umbra
        system.boot system.home
        system.pkgconfig system.sys-specs
        drivers.ssh
        shell.zsh programs.nixvim
      ];
    };
    arm-vmware = {
      system = "aarch64-linux";
      builder = inputs.nixpkgs.lib.nixosSystem;
      user = "goat";
      modules = with mod; [
        hosts.arm-vmware

        net.syncthing net.tailscale

        programs.zen-browser  programs.emacs
        programs.git programs.kitty programs.yazi
	      software.wayland programs.niri programs.quickshell

        software.dev software.qol
        software.texlive software.java software.rust

        system.boot system.home system.users
        system.pkgconfig system.sys-specs
        disko.disko disko.ext4

        drivers.graphical drivers.ssh drivers.ext4

        shell.variables shell.zsh shell.nushell

        theming.stylix theming.themes.nord
      ];
    };
    macbook = {
      system = "aarch64-darwin";
      builder = inputs.nix-darwin.lib.darwinSystem;
      user = "goat";
      modules = with mod; [
        hosts.macbook

        net.tailscale

        programs.emacs programs.prismLauncher
        programs.git programs.yazi

        software.dev software.qol software.texlive software.java software.rust

        system.home system.pkgconfig

        shell.variables shell.zsh

        theming.stylix theming.themes.nord
      ];
    };
  };

  mkSystems = cfgs:
    inputs.nixpkgs.lib.mapAttrs (machine: cfg: let
      # modules
      loadedMods = [(extlib.loadModulesFromAttrset
        cfg.modules
        {inherit self inputs extlib;}
      )];
      mod = {
        nix = builtins.concatLists (map (m: m.nix or []) loadedMods);
        home = builtins.concatLists (map (m: m.home or []) loadedMods);
      };
      specialArgs = {
        inherit self inputs extlib machine;
        spkgs = import inputs.nixpkgs-stable {inherit (cfg) system;};
        inherit (cfg) system user;
      };
    in
      cfg.builder {
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
