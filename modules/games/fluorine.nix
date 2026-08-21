{
  nix = [
    # Nessecary stuff fhs-related tools
    ({
      pkgs,
      user,
      ...
    }: {
      services.envfs.enable = true;
      users.users.${user}.extraGroups = ["fuse"];
      programs.fuse.userAllowOther = true;

      programs.nix-ld.enable = true;
      programs.nix-ld.libraries = with pkgs; [
        libGL
        libGLX
        libX11
        libxkbcommon
        stdenv.cc.cc.lib # libstdc++
        wayland
      ];
    })
  ];
  home = [
    # xdg stuff
    ({config, ...}: {
      xdg.desktopEntries.fluorine-nxm-handler = {
        name = "Fluorine Manager NXM Handler";
        exec = "${config.home.homeDirectory}/.local/bin/mo2-nxm-handler %u";
        mimeType = ["x-scheme-handler/nxm" "x-scheme-handler/modl"];
        noDisplay = true;
      };

      # nxm link association
      xdg.mimeApps.defaultApplications = {
        "x-scheme-handler/nxm" = ["fluorine-manager.desktop"];
        "x-scheme-handler/modl" = ["fluorine-manager.desktop"];
      };
    })
  ];
}
