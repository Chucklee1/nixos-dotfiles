{pkgs, ...}: {
  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      jnoortheen.nix-ide
      kamadorueda.alejandra
    ];
    userSettings = {
      "update.mode" = "none";
      "update.enableWindowsBackgroundUpdates" = false;
      "git.enableSmartCommit" = true;
      "git.confirmSync" = false;
      "editor.tabSize" = 2;
      "editor.detectIndentation" = false;
      "editor.minimap.enabled" = false;
      "editor.autoClosingBrackets" = "never";
      "editor.autoClosingQuotes" = "never";
      "editor.autoClosingParentheses" = "never";
      "files.autoSave" = "off";
      "files.confirmDelete" = false;
      "explorer.confirmDragAndDrop" = false;
      "explorer.confirmDelete" = false;
      "workbench.statusBar.visible" = true;
      "workbench.colorTheme" = "Stylix";
    };
  };
}
