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
            "editor.minimap.enabled" = false;
            "explorer.confirmDragAndDrop" = false;
            "editor.autoClosingBrackets" = "never";
            "editor.autoClosingQuotes" = "never";
            "editor.autoClosingParentheses" = "never";
            "files.autoSave" = "off";
            "files.confirmDelete" = false;
            "git.confirmSync" = false;
            "git.enableSmartCommit" = true;
            "workbench.statusBar.visible" = false;
            "workbench.colorTheme" = "Stylix";
            "[nix]"."editor.tabSize" = 2;
          };
        };
      }
    ];
  };
}
