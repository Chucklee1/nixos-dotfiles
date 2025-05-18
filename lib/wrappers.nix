{nixpkgs, ...}: {
  wrapPkgInShell = program: {program = nixpkgs.mkShell {packages = [program];};};
}
