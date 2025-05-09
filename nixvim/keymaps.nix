{lib, ...}: {
  globals.mapleader = " ";
  globals.maplocalleader = " ";
  keymaps = let
    mkNormalKeyMaps = list: let
      Elem = int: lib.elemAt (lib.splitString " : " list) int;
    in {
      mode = "n";
      action = "<cmd>${Elem 0}<cr>";
      key = "${Elem 1}";
    };
  in
    # [ action key ]
    map mkNormalKeyMaps [
      # misc ui
      "Oil : <leader>e"
      "LazyGit : <leader>gg"

      # Telescope bindings
      "Telescope live_grep : <leader>fn"
      "Telescope find_files : <leader>f"
      "Telescope git_commits : <leader>fg"
      "Telescope oldfiles : <leader>fo"

      # Bufferline bindings
      "BufferLineCycleNext : <Tab>"
      "BufferLineCyclePrev : <S-Tab>"
      "BufferLineCycleNext : <S-l>"
      "BufferLineCyclePrev : <S-h>"
      "bd : <leader>c"
      "bd! : <leader-S-c>"
    ];
}
