{nixpkgs, ...}:
with nixpkgs.lib; {
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
}
