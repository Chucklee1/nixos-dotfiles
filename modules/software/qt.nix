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
      environment.systemPackages = with pkgs; [
        # qt
        qtEnv
        libglvnd
        qtcreator
        # clang works better with qt
        clang
        lldb
      ];
    })
  ];
}
