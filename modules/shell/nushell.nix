{
    nix = [
      ({lib, pkgs, user, ...}: {users.users.${user}.shell = lib.mkForce pkgs.nushell;})
    ];
  home = [
    {
	    programs.nushell = {
        enable = true;
        settings = {
          edit_mode = "vi";
        };
      };
	  }
  ];

}
