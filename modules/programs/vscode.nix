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
    home-manager.users.goat.programs.vscode = {
      enable = true;
      extensions = with pkgs.vscode-extensions; [
        jnoortheen.nix-ide
        eamodio.gitlens
        kamadorueda.alejandra
      ];
      userSettings = {
        "editor.minimap.enabled" = false;
        "explorer.confirmDragAndDrop" = false;
        "files.autoSave" = "off";
        "files.confirmDelete" = false;
        "git.confirmSync" = false;
        "git.enableSmartCommit" = true;
        "workbench.colorTheme" = "Stylix";
        "[nix]"."editor.tabSize" = 2;
      };
    };
  };
}
