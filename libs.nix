{nixpkgs, ...}:
with nixpkgs.lib; rec {
  readDirRecursive = dir:
    concatMapAttrs (file:
      flip getAttr {
        directory = mapAttrs' (subpath: nameValuePair "${file}/${subpath}") (readDirRecursive "${dir}/${file}");
        regular = {
          ${file} = "${dir}/${file}";
        };
        symlink = {};
      }) (builtins.readDir dir);

  #absolute horrid deepMerge
  mergeAllRecursive = a: b:
    foldl' (
      acc: key: let
        # sets all missing keys to null
        va = a.${key} or null;
        vb = b.${key} or null;
      in
        acc
        // {
          "${key}" =
            # remove all nulls
            if va == null
            then vb
            else if vb == null
            then va
            # concat all lists
            else if isList va && isList vb
            then va ++ vb
            # recurse into function if attr
            else if isAttrs va && isAttrs vb
            then mergeAllRecursive va vb
            # let last primitive-type values override previous
            else vb;
        }
    ) {}
    (unique (attrNames a ++ attrNames b));

  # concats modules from imported files
  mergeModules = dir: args: (pipe dir [
    readDirRecursive
    (filterAttrs (flip (const (hasSuffix ".nix"))))
    (mapAttrs (const import))
    (mapAttrs (const (flip toFunction args)))
    attrValues
    (builtins.foldl' mergeAllRecursive {})
  ]);

  # concats profiles from singular module (subattrs under nix & home, eg desktop)
  mergeProfiles = mod: prev: next: (genAttrs ["nix" "home"] (type: mod.${type}.${prev} or [] ++ mod.${type}.${next} or []));
}
