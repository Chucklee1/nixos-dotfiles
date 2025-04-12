{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    nix-darwin.url = "github:nix-darwin/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    impermanence.url = "github:nix-community/impermanence";
    stylix.url = "github:danth/stylix";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    niri.url = "github:sodiboo/niri-flake";
    sops-nix.url = "github:Mic92/sops-nix";
  };

  outputs = {
    self,
    nixpkgs,
    nix-darwin,
    ...
  } @ inputs:
    with nixpkgs.lib; let
      systems = ["x86_64-linux" "x86_64-darwin"];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      dir = "${self}/modules";

      mergeAllRecursive = a: b:
        foldl' (
          acc: key: let
            va = a.${key} or null;
            vb = b.${key} or null;
            merged =
              if va == null
              then vb
              else if vb == null
              then va
              else if isList va && isList vb
              then va ++ vb
              else if isAttrs va && isAttrs vb
              then mergeAllRecursive va vb
              else vb;
          in
            acc // {"${key}" = merged;}
        ) {}
        (unique (attrNames a ++ attrNames b));

      raw = pipe (builtins.readDir dir) [
        (filterAttrs (file: type: hasSuffix ".nix" file && type == "regular"))
        attrNames
        (map (file: import "${dir}/${file}"))
        (map (file:
          if isFunction file
          then (file {inherit inputs;})
          else file))
        (builtins.foldl' mergeAllRecursive {})
      ];

      mergeProfiles = profiles: concatLists (map (profile: 
        (getAttr profile raw.nix or []) ++ (getAttr profile raw.home or [])
      ) profiles);

      profiles = {
        yggdrasil = mergeProfiles ["global" "nixos" "yggdrasil"];
        laptop = mergeProfiles ["global" "nixos" "laptop"];
        darwin = "global";
      };

    mkMods = host: (profiles.${host}.nix ++ [({config, ...}: {
        users.users.main.name = "goat";
        networking.hostName = "${config.users.users.main.name}-${host}";
        _module.args.homeMods = profiles.${host}.home;
      })]);
    in {
      packages = forAllSystems (system: import ./pkgs nixpkgs.legacyPackages.${system});
      formatter = forAllSystems (system: nixpkgs.legacyPackages.${system}.alejandra);

      nixosConfigurations =
        genAttrs ["yggdrasil" "nimbus"]
        (host: nixosSystem {modules = mkMods host;});

      darwinConfigurations."darwin" = nix-darwin.lib.darwinSystem {modules = raw.global.nix ++ [
        ({lib, config, ...}: {
        users.users.main.name = "goat";
        networking.hostName = "${config.users.users.main.name}-darwin";
        _module.args.homeMods = raw.global.home;
        nixpkgs.hostPlatform = lib.mkDefault "x86_64-darwin";
        home-manager.users.main.home.homeDirectory = "/Users/goat";
        })
      ];};
    };
}
