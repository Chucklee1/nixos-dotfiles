{inputs, self, ...}:
with inputs.nixpkgs.lib; rec {
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

  #absolute horrid deepMerge
  mergeAllRecursive = a: b:
    foldl' (acc: key: let
      # sets all missing keys to null
      va = a.${key} or null;
      vb = b.${key} or null;
    in
      acc // {
        "${key}" =
          if va == null
          then vb
          else if vb == null
          then va
          else if isList va && isList vb
          then va ++ vb
          # recurse into function if attr
          else if isAttrs va && isAttrs vb
          then mergeAllRecursive va vb
          # let last primitive-type values override previous
          else vb;
      }) {}
      (unique (attrNames a ++ attrNames b));

  # concats modules from imported files
  loadModules = dir: args: (pipe dir [
    readDirRecursive
    (filterAttrs (flip (const (hasSuffix ".nix"))))
    (mapAttrs (const import))
    (mapAttrs (const (flip toFunction args)))
    attrValues
    (builtins.foldl' mergeAllRecursive {})
  ]);

  # returns list of file paths - it's just mergeModules without wrapping
  simpleMerge = dir: (pipe dir [
    readDirRecursive
    (filterAttrs (flip (const (hasSuffix ".nix"))))
    attrValues
  ]);

  # basename clone for nix
  basename = file: pipe file [
    (replaceString ".nix" "")
    (splitString "/")
    last
  ];

  /*
     - reworked readDirRecursive to return in a
       nested attrset format
     - i like it cause it makes it easy to import
       files like attrsets
     - example: for file `modules/subfolder/file.nix`
       using `readDirRecursiveToAttrset "modules"`
       as mod would allow for accessing `file.nix` as
       `mod.subfolder.file`
  */
  readDirRecursiveToAttrset = dir:
    concatMapAttrs (file:
      flip getAttr {
        directory = {
          "${basename file}" = (readDirRecursiveToAttrset "${dir}/${file}");
        };
        regular = {
          "${basename file}" = "${dir}/${file}";
        };
        symlink = {};
      }) (builtins.readDir dir);

  loadModulesFromAttrset = mods: args: (pipe mods [
    (map import)
    (map (flip toFunction args))
    (builtins.foldl' mergeAllRecursive {})
  ]);

  # system helpers
  darwinOrLinux = system: A: B:
    if (builtins.match ".*-darwin" system != null)
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
      pkgs = import inputs.nixpkgs {inherit system;};
    in
      f pkgs);
}
