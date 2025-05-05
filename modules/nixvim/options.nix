{inputs, ...}: {
  home.global = [
    inputs.nixvim.homeManagerModules.nixvim
    ({lib, ...}: {
      programs.nixvim = {
        # ----- OPTIONS -----
        enable = true;
        defaultEditor = true;
        viAlias = true;
        vimAlias = true;
        withPerl = false;
        withRuby = false;
        opts = {
          # visual opts
          number = true;
          relativenumber = true;
          signcolumn = "yes";
          cursorline = true;
          scrolloff = 5;
          splitright = true;
          splitbelow = true;
          termguicolors = true;
          # spacing behavior
          tabstop = 2;
          shiftwidth = 2;
          softtabstop = 0;
          smarttab = true;
          expandtab = true;
          breakindent = true;
          autoindent = true;
          smartindent = true;
          # case sensing
          ignorecase = true;
          smartcase = true;
          # idk
          mouse = "a";
          timeoutlen = 600;

          # history
          clipboard = {
            providers = {
              wl-copy.enable = true; # wayland
              xsel.enable = true; # X11
            };
            register = "unnamedplus";
          };
          backup = false;
          swapfile = false;
          undofile = true;
        };
        # command aliases
        userCommands = {
          Q.command = "q";
          Wq.command = "wq";
          WQ.command = "wq";
          W.command = "w";
        };
        performance.byteCompileLua.enable = true;
      };

      # ----- KEYMAPS -----
      programs.nixvim = {
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
      };
    })
  ];
}
