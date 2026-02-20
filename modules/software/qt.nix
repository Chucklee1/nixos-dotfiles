{
  nix = [
    ({pkgs, ...}: let
      qtEnv = with pkgs.qt6;
        env "qt6-simc-${qtbase.version}" [
          qtbase
          qmake
          qt5compat
          qttools
          qtdeclarative
          qtcharts
          qtwayland
          qtmultimedia
          qtlanguageserver
          qtquick3d
          qtshadertools
          qtquicktimeline
        ];
    in {
      enviornment.systemPackages = with pkgs; [
        # qt
        qtEnv
        libglvnd
        # clang works better with qt
        clang
        lldb
      ];
    })
  ];
}
