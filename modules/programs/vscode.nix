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
        "files.autoSave" = "off";
        "files.confirmDelete" = false;
        "explorer.confirmDragAndDrop" = false;
        "[nix]"."editor.tabSize" = 2;
        "editor.minimap.enabled" = false;
        "git.confirmSync" = false;
      };
    };
  };
}
