{
  home = [
    ({config, pkgs, machine, ...}: {
      programs.nushell = {
        enable = true;
        # need to manually set shellAliases with nushell...
        shellAliases = let
          rebuild_cmd =
            if machine == "macbook"
            then "darwin-rebuild"
            else "nixos-rebuild";
        in {
          rebuild-flake = "sudo ${rebuild_cmd} switch --flake ${config.home.homeDirectory}/nixos-dotfiles#${machine} --show-trace --impure";
        };
	      settings = {
          show_banner = false; # remove startup msg prompt
          edit_mode = "vi";
        };
        plugins = with pkgs.nushellPlugins; [
          highlight
          formats
          gstat
        ];
      };
      # once I figure out nushell coding I will make my own prompt...
      programs.oh-my-posh = {
        enable = true;
        enableBashIntegration = false;
        enableZshIntegration = false;
        useTheme = "pure";
	    };
    })
  ];
}
