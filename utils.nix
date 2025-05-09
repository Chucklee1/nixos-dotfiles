{nixpkgs, ...}:
with nixpkgs.lib; {
  # horrid deepMerge that works
  mergeAllRecursive = a: b:
    foldl' (
      acc: key: let
        va = a.${key} or null;
        vb = b.${key} or null;
      in
        acc
        // {
          "${key}" =
            if va == null
            then vb
            else if vb == null
            then va
            else if isList va && isList vb
            then va ++ vb
            else if isAttrs va && isAttrs vb
            then mergeAllRecursive va vb
            else vb;
        }
    ) {}
    (unique (attrNames a ++ attrNames b));

  # taken from sodiboo's dotfiles
  readDirRecursive = dir:
    concatMapAttrs (file:
      flip getAttr {
        directory = mapAttrs' (subpath: nameValuePair "${file}/${subpath}") (readDirRecursive "${dir}/${file}");
        regular = {
          ${file} = "${dir}/${file}";
        };
        symlink = {};
      }) (builtins.readDir dir);

  # main module creation function
  mergeModules = dir: args: (pipe dir [
    readDirRecursive
    (filterAttrs (flip (const (hasSuffix ".nix"))))
    (mapAttrs (const import))
    (mapAttrs (const (flip toFunction args)))
    attrValues
    (builtins.foldl' mergeAllRecursive {})
  ]);

  # additional step for merging different profiles
  mergeProfiles = mod: prev: next: (genAttrs ["nix" "home"] (type: mod.${type}.${prev} or [] ++ mod.${type}.${next} or []));
}
