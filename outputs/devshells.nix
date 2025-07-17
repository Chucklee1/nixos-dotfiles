{pkgs, ...}:
with pkgs; {
  haskell = mkShell {
    packages = [ghc cabal-install stack];
    shellHook = ''echo "entered haskell shell" '';
  };
  java = mkShell {packages = [jdk gradle];};
  shellHook = ''echo "entered java shell" '';
}
