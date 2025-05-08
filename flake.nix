{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    niri.url = "github:sodiboo/niri-flake";
    minegrub-theme.url = "github:Lxtharia/minegrub-theme";
    minecraft-plymouth.url = "github:nikp123/minecraft-plymouth-theme";
    minesddm.url = "github:Davi-S/sddm-theme-minesddm";
    minesddm.inputs.nixpkgs.follows = "nixpkgs";
    nordic-nvim.url = "github:AlexvZyl/nordic.nvim/6afe957722fb1b0ec7ca5fbea5a651bcca55f3e1";
    nordic-nvim.flake = false;
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs:
    with nixpkgs.lib; let
      # CUSTOM ARGS HERE
      dir = "${self}/modules";
      user = "goat";
      system = "x86_64-linux";
      #pkgs = import nixpkgs {inherit system;};

      # horrid deepMerge that works and idk why
      mergeAllRecursive' = a: b:
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

      readDirRecursive = dir:
        concatMapAttrs (file:
          flip getAttr {
            directory = mapAttrs' (subpath: nameValuePair "${file}/${subpath}") (readDirRecursive "${dir}/${file}");
            regular = {
              ${file} = "${dir}/${file}";
            };
            symlink = {};
          }) (builtins.readDir dir);

      mergeAllRecursive = a: b: let
        mergeTwo = builtins.zipAttrsWith (
          key: values: let
            item = filter (v: v != null) values;
          in
            if all isAttrs item
            then mergeTwo (head item) (last item)
            else if all isList item
            then concatLists item
            else last item
        ) [a b];
      in
        items: foldl' mergeTwo {} items;

      # main module creation function
      mergeModules = args: (pipe dir [
        readDirRecursive
        (filterAttrs (flip (const (hasSuffix ".nix"))))
        (mapAttrs (const import))
        (mapAttrs (const (flip toFunction args)))
        attrValues
        mergeAllRecursive
      ]);

      # additional step for merging different profiles
      mergeProfiles = mod: prev: next: (genAttrs ["nix" "home"] (type: mod.${type}.${prev} or [] ++ mod.${type}.${next} or []));

      mkSystem = host:
        nixosSystem {
          inherit system;
          modules = let
            # passing args here to inherit host
            mod' = mergeModules {
              inherit inputs self user;
              machine = host;
            };
            mod = mergeProfiles mod' "global" "${host}";
          in
            mod.nix ++ [{_module.args.homeMods = mod.home;}];
        };
    in {
      #packages.${system} = import "${self}/pkgs" {inherit pkgs;};

      nixosConfigurations = genAttrs ["desktop" "macbook"] (host: mkSystem host);
    };
}
