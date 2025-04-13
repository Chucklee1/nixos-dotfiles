{
  description = "i dont kow what im doing";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
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

  outputs = {nixpkgs, ...} @ inputs:
    with nixpkgs.lib; let
      #dir = "${self}/nixos";
      specialArgs = {inherit inputs;};
      /*
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
          system = "x86_64-linux";
          modules = let
            mod = mergeMods "global" "${host}";
          in
            mod.nix ++ [{_module.args.homeMods = mod.home;}];
        };
      */
    in {
      nixosConfigurations = genAttrs ["desktop" "nimbus"] (host: mkSystem host);
      darwinConfigurations.macbookpro = inputs.nix-darwin.lib.darwinSystem {
        inherit specialArgs;
        system = "x86_64-darwin";
        modules = [./darwin];
      };
    };
}
