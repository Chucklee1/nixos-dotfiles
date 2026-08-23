{self, ...}: {
  nix = [
    ({machine, ...}: {
      environment = {
        shellAliases = let
          rebuild_cmd =
            if machine == "macbook"
            then "darwin-rebuild"
            else "nixos-rebuild";
        in {
          rebuild-flake = "sudo ${rebuild_cmd} switch --flake $HOME/nixos-dotfiles#${machine} --show-trace --impure";
          flopts = "nixos-option --flake $HOME/nixos-dotfiles#${machine} --show-trace --impure";
          sysctl = "sudo systemctl";
          usrctl = "systemctl --user";
          # for darwin and non-nixos systmes
          l = "ls -al";
        };
      };
    })
    # shell scripts
    ({pkgs, ...}: {
      environment.systemPackages = [
        (pkgs.writeShellScriptBin "mkSnapshot" (builtins.readFile "${self}/assets/scripts/mkSnapshot"))
      ];
    })
  ];
}
