{
  nix = [
    ({lib, pkgs, user, ...}: {users.users.${user}.shell = lib.mkForce pkgs.nushell;})
  ];
  home = [
    ({pkgs, ...}:{
	    programs.nushell = {
        enable = true;
        settings = {
          show_banner = false; # remove startup msg prompt
          edit_mode = "vi";
        };
        plugins = with pkgs; [
          nu_plugin_highlight
          nu_plugin_formats
          nu_plugin_gstat
        ];
      };
      # once I figure out nushell coding I will make my own prompt...
      programs.oh-my-posh = {
        enable = true;
        useTheme = "pure";
	    };
    })
  ];

}
