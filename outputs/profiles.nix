{
  self,
  inputs,
  extlib,
  ...
}: {
  # ---- system  ----
  profiles = let
    extArgs = {inherit self inputs extlib;};
    mod = extlib.readDirRecursiveToAttrset "${self}/modules";
    load_mods = mods: [(extlib.loadModulesFromAttrset mods extArgs)];
  in {
    desktop = {
      system = "x86_64-linux";
      modules = with mod; load_mods [
        hosts.desktop
        net.mpd net.syncthing net.tailscale

        programs.librewolf programs.emacs
        programs.git programs.kitty programs.yazi
        programs.niri programs.waybar

        software.apps software.dev software.texlive
        software.gaming software.qol
        software.wayland

        system.boot system.home system.users
        system.pkgconfig system.sys-specs

        drivers.graphical drivers.ssh drivers.nvidia

        shell.variables shell.zsh

        theming.blockgame theming.stylix

        virt.qemu
      ];
      user = "goat";
    };
    # laptop = {
    #   system = "x86_64-linux";
    #   modules = with mod; [
    #     global laptop linux metal
    #     wayland additions.full
    #     niri waybar
    #     editor.emacs
    #   ];
    #   user = "goat";
    # };
    inspiron = {
      system = "x86_64-linux";
      modules = with mod; load_mods [
        hosts.inspiron

        net.mpd net.syncthing net.tailscale net.the-server

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
    # umbra = {
    #   system = "x86_64-linux";
    #   modules = with mod; [
    #     global umbra linux
    #     wayland
    #     niri waybar
    #   ];
    #   user = "nixos";
    # };
    macbook = {
      system = "aarch64-darwin";
      modules = with mod; load_mods [
        hosts.macbook

        net.tailscale

        programs.emacs #programs.zen-browser
        programs.git programs.kitty programs.yazi

        software.dev software.qol software.texlive

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
