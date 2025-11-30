{
  nix = [
    ({pkgs, ...}: {
      environment.systemPackages = with pkgs; [
	      # java
        jdk
        javaPackages.compiler.temurin-bin.jdk-25
        # build system that fabric uses
        gradle-completion
        # better java
        kotlin
        kotlin-language-server
        tree-sitter-grammars.tree-sitter-kotlin
      ];
	  })
  ];
  home = [{programs.gradle.enable = true;}];
}
