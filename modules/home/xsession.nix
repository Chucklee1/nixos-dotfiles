{pkgs, ...}: {
  xsession = {
    enable = true;
    windowManager = {
      aweseome.enabe = true;
      luaModules = with pkgs.luaPackages; [
        luarocks # is the package manager for Lua modules
        luadbi-mysql # Database abstraction layer
      ];
    };
  };
}
