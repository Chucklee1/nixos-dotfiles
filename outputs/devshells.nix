{pkgs, ...}:
with pkgs; let
  std = msg: ''echo -e "\e[32m${msg}\e[0m" '';
in {
  haskell = mkShell {
    packages = [ghc cabal-install stack];
    shellHook = std "Entered Haskell Shell";
  };
  java = mkShell {packages = [jdk gradle];};
  shellHook = std "Entered Java Shell";
}
