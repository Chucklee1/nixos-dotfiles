{
  inputs,
  pkgs,
  ...
}:
with pkgs; let
  msg = str: ''echo -e "\e[32m${str}\e[0m" '';
in {
  haskell = mkShell {
    packages = [ghc cabal-install stack zlib];
    shellHook = std "Loaded Haskell env";
  };
  java = mkShell {
    packages = [jdk gradle];
    shellHook = std "Loaded Java env";
  };
  dotfiles = mkShell {
    packages = [just];
    shellHook = ''
      export make="just"
      ${msg "Loaded dotifles env"}
    '';
  };
  qt = mkShell {
      nativeBuildInputs = [
        (qt6.env "qt6-simc-${qt6.qtbase.version}" [
          qt6.qtbase
          qt6.qmake
          qt6.qt5compat
          qt6.qttools
          qt6.qtdeclarative
          qt6.qtcharts
          qt6.qtwayland
          qt6.qtmultimedia
          qt6.qtlanguageserver
          qt6.qtquick3d
          qt6.qtshadertools
          qt6.qtquicktimeline
        ])
        clang
        cmake
        gnumake
        gdb
        libglvnd
        ninja
        pkg-config
      ];

    shellHook = ''
      # tmp fix for fish, ignore for now
      export IN_NIX_SHELL=1

      export CC=${pkgs.clang}/bin/clang
      export CXX=${pkgs.clang}/bin/clang++

      export CMAKE_C_COMPILER=$CC
      export CMAKE_CXX_COMPILER=$CXX
      export CMAKE_GENERATOR=Ninja

      ${msg "Loaded qt env"}
    '';
  };
  remote = mkShell {
    packages = [just inputs.nix-vim.packages.${pkgs.system}.core];
    shellHook = ''
      export make="just"
      ${msg "Loaded remote env"}
    '';
  };
  crossComp = let
    CROSS_ARCH = "aarch64-multiplatform";
    cpkgs = pkgsCross.${CROSS_ARCH}.buildPackages;
  in
    mkShell {
      nativeBuildInputs = [
        qemu
        cpkgs.gcc
        cpkgs.binutils
      ];
      shellHook = ''
        ${msg "Loaded crossComp env for arch: ${CROSS_ARCH}"}
      '';
    };
}
