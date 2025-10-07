{
  nix = [
    ({machine, ...}: {
      environment = {
        variables = {
          BASH_SILENCE_DEPRECATION_WARNING = "1"; # fix for macOS
        };
        shellAliases = let
          rebuild_cmd =
            if machine == "macbook"
            then "darwin-rebuild"
            else "nixos-rebuild";
        in {
          rebuild-flake = "sudo ${rebuild_cmd} switch --flake $HOME/nixos-dotfiles#${machine} --show-trace --impure";
        };
      };
    })
  ];
}
