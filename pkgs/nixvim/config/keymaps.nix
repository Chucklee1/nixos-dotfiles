{
  globals.mapleader = " ";
  globals.maplocalleader = " ";
  extraConfigLua = ''
     local key = vim.keymap.set

     -- General
     key("n", "<leader>gg", ":LazyGit<CR>",             {desc = "Open LazyGit"})

     -- Explorer
     key("n", "<leader>e",  ":Oil<CR>",                 {desc = "Open fil editor"})
     key("n", "<leader>tt", ":Neotree<CR>",             {desc = "Open fil editor"})

     -- Telescope binding
    key("n", "<leader>fb", ":Telescope buffers<CR>",    {desc = "Telescope buffer search"})
    key("n", "<leader>ff", ":Telescope find_files<CR>", {desc = "Telescope file search"})
    key("n", "<leader>fg", ":Telescope live_grep<CR>",  {desc = "Telescope grep search"})
    key("n", "<leader>fn", ":Telescope nerdy<CR>",      {desc = "Telescope nerd-icon search"})

     -- Buffer
     key("n",  "<S-l>",      ":bn<CR>",                {desc = "Goto next buffer"})
     key("n",  "<S-h>",      ":bp<CR>",                {desc = "Goto previous buffer"})
     key("n",  "<leader>bd", ":bd<CR>",                {desc = "Buffer delete"})
     key("n", "<leader>bD",  ":bd!<CR>",               {desc = "Force Buffer delete"})
     -- Telescope find buffer clone to match emacs ibuffer keymap
     key("n",  "<leader>bi", ":Telescope buffers<CR>", {desc = "Buffer Ibuffer"})

     -- folding
    key('n', 'zR', require('ufo').openAllFolds)
    key('n', 'zM', require('ufo').closeAllFolds)
    key('n', 'zr', require('ufo').openFoldsExceptKinds)
    key('n', 'zm', require('ufo').closeFoldsWith) -- closeAllFolds == closeFoldsWith(0)
    key('n', 'zK', function()
        local winid = require('ufo').peekFoldedLinesUnderCursor()
        if not winid then
            vim.lsp.buf.hover()
        end
    end)

     -- use the force
     key("n", "<leader>W",  ":w!<CR>",  {desc = "Force Write"})
     key("n", "<leader>Q",  ":q!<CR>",  {desc = "Force quit"})


     -- tweaks
     key("n", "<esc>",      ":nohlsearch<CR>",  {desc = "Clear search highlights"})

     key("n", "n", "nzzzv",       {desc = "Next search result (centered)"})
     key("n", "N", "Nzzzv",       {desc = "Previous search result (centered)"})
     key("n", "<C-d>", "<C-d>zz", {desc = "Half page down (centered)"})
     key("n", "<C-u>", "<C-u>zz", {desc = "Half page up (centered)"})

     key("n", "<A-j>", ":m .+1<CR>==",     {desc = "Move line down"})
     key("n", "<A-k>", ":m .-2<CR>==",     {desc = "Move line up"})
     key("v", "<A-j>", ":m '>+1<CR>gv=gv", {desc = "Move selection down"})
     key("v", "<A-k>", ":m '<-2<CR>gv=gv", {desc = "Move selection up"})

     key("v", "<", "<gv", {desc = "Indent left and reselect"})
     key("v", ">", ">gv", {desc = "Indent right and reselect"})

     key("n", "J", "mzJ`z", {desc = "Join lines and keep cursor position"})
  '';
}
