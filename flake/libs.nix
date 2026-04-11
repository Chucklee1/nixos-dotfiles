{self, ...}:
with self.inputs.nixpkgs.lib; rec {
  readDirRecursive = dir:
    concatMapAttrs (file:
      flip getAttr {
        directory =
          mapAttrs'
          (subpath: nameValuePair "${file}/${subpath}")
          (readDirRecursive "${dir}/${file}");
        regular = {
          ${file} = "${dir}/${file}";
        };
        symlink = {};
      }) (builtins.readDir dir);

  # returns list of file paths
  simpleMerge = dir: (pipe dir [
    readDirRecursive
    (filterAttrs (flip (const (hasSuffix ".nix"))))
    attrValues
  ]);

  # basename clone for nix
  basename = file:
    pipe file [
      (replaceString ".nix" "")
      (splitString "/")
      last
    ];

  /* recursively read directory, then transform
   * paths to nested attribute set */
  readDirRecToAttrset = dir:
    concatMapAttrs (file:
      flip getAttr {
        directory = {
          "${basename file}" = readDirRecToAttrset "${dir}/${file}";
        };
        regular = {
          "${basename file}" = "${dir}/${file}";
        };
        symlink = {};
      }) (builtins.readDir dir);

  /* merge all inner lists of same name
   * assume key is of type list */
  mergeInnerList = sets: (builtins.foldl' (
      acc: m:
        builtins.foldl'
        (
          innerAcc: key:
            innerAcc
            // {
              ${key} =
                (innerAcc.${key} or [])
                ++ (m.${key} or []);
            }
        )
        acc
        (builtins.attrNames m)
    ) {}
    sets);

  loadModulesFromAttrset = mods: args:
    pipe mods [
      (map import)
      (map (flip toFunction args))
      mergeInnerList
    ];

  # system helpers
  darwinOrLinux = A: B:
    if (builtins.match ".*-darwin" builtins.currentSystem != null)
    then A
    else B;

  armOrNot = A: B:
    if (builtins.match "aarch64-*" builtins.currentSystem != null)
    then A
    else B;

  # idea from github:Misterio77/nix-starter-configs
  allSystems = genAttrs [
    "x86_64-linux"
    "aarch64-linux"
    "x86_64-darwin"
    "aarch64-darwin"
  ];

  allSystemsWithPkgs = f:
    allSystems (system: let
      # expose overlays to pkgs
      pkgs = import self.inputs.nixpkgs {
        inherit system;
        overlays = builtins.attrValues self.overlays; # safe if overlays are pure
      };
    in
      f pkgs);
}
