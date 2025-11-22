{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
	      # java
        jdk
        # build system that fabric uses
        gradle
        gradle-completion
        # better java
        kotlin
        kotlin-language-server
        tree-sitter-grammars.tree-sitter-kotlin
      ];
	  })
  ];
}
