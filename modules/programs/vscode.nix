{
  pkgs,
  config,
  lib,
  ...
}: {
  options = {
    vscode.enable = lib.mkEnableOption "enable vscode";
  };

  config = lib.mkIf config.vscode.enable {
    home-manager.sharedModules = [
      {
        programs.vscode = {
          enable = true;
          extensions = with pkgs.vscode-extensions; [
            jnoortheen.nix-ide
            eamodio.gitlens
            kamadorueda.alejandra
          ];
          userSettings = {
            "update.mode" = "none";
            "update.enableWindowsBackgroundUpdates" = false'
            "editor.tabSize" = 2;
            "editor.minimap.enabled" = false;
            "editor.autoClosingBrackets" = "never";
            "editor.autoClosingQuotes" = "never";
            "editor.autoClosingParentheses" = "never";
            "files.autoSave" = "off";
            "files.confirmDelete" = false;
            "explorer.confirmDragAndDrop" = false;
            "explorer.confirmDelete" = false;
            "git.confirmSync" = false;
            "git.enableSmartCommit" = true;
            "workbench.statusBar.visible" = false;
            "workbench.colorTheme" = "Stylix";
          };
        };
      }
    ];
  };
}
