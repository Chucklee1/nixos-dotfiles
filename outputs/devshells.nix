{
  inputs,
  pkgs,
  ...
}:
with pkgs; let
  std = msg: ''echo -e "\e[32m${msg}\e[0m" '';
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
      ${std "Loaded dotifles env"}
    '';
  };
  remote = mkShell {
    packages = [just inputs.nix-vim.packages.${pkgs.system}.core];
    shellHook = ''
      export make="just"
      ${std "Loaded remote env"}
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

        ${std "Loaded crossComp env for arch: ${CROSS_ARCH}"}
      '';
    };
}
