{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    niri.url = "github:sodiboo/niri-flake";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs:
    with nixpkgs.lib; let
      system = "x86_64-linux";

      dir = "${self}/modules";

      raw = let
        # TODO: find a function like mergDeep that is not so crappy
        mergeDeep = a: b:
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
                then mergeDeep va vb
                else vb;
            in
              acc // {"${key}" = merged;}
          ) {}
          (unique (attrNames a ++ attrNames b));
      in
        pipe (attrNames (builtins.readDir dir)) [
          (map (file: replaceStrings [".gen.nix"] [".IgnoreMe"] file))
          (filter (file: hasSuffix ".nix" file))
          (map (file: import "${dir}/${file}"))
          (map (file:
            if isFunction file
            then (file {inherit inputs;})
            else file))
          (builtins.foldl' mergeDeep {})
        ];

      mergeMods = a: b: (genAttrs ["nix" "home"] (type: raw.${type}.${a} or [] ++ raw.${type}.${b} or []));

      mkSystem = host:
        nixosSystem {
          inherit system;
          modules = let
            mod = mergeMods "global" "${host}";
          in
            mod.nix ++ [{_module.args.homeMods = mod.home;}];
        };
    in {
      formatter.${system} = nixpkgs.legacyPackages.${system}.alejandra;
      nixosConfigurations = genAttrs ["laptop" "desktop"] (host: mkSystem host);
    };
}
