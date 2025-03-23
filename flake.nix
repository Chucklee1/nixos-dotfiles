{
  description = "i dont kow what im doing";

  inputs = {
    # niri or libgbm bug :(
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    stylix.url = "github:danth/stylix";
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    niri.url = "github:sodiboo/niri-flake";
    sops-nix.url = "github:Mic92/sops-nix";
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
      in
        pipe (builtins.readDir dir) [
          (filterAttrs (file: type: hasSuffix ".nix" file && type == "regular"))
          attrNames
          (map (file: import "${dir}/${file}"))
          (map (file:
            if isFunction file
            then (file {inherit inputs;})
            else file))
          (builtins.foldl' mergeAllRecursive {})
        ];

      mergeMods = prev: next: (genAttrs ["nix" "home"] (type: raw.${type}.${prev} or [] ++ raw.${type}.${next} or []));

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
