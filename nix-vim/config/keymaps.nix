{
  globals.mapleader = " ";
  globals.maplocalleader = " ";
  extraConfigLua = ''
     local key = vim.keymap.set
     -- General
     key("n", "<esc>",      ":nohlsearch<CR>",  {desc = "Clear search highlights"})
     key("n", "<leader>e",  ":Yazi<CR>",        {desc = "Open fil editor"})
     key("n", "<leader>gg", ":LazyGit<CR>",     {desc = "Open LazyGit"})
     key("n", "<leader>gc", ":CopilotChat<CR>", {desc = "Open CopilotChat"})

     -- Telescope binding
    key("n", "<leader>fb", ":Telescope buffers<CR>",    {desc = "Telescope buffer search"})
    key("n", "<leader>ff", ":Telescope find_files<CR>", {desc = "Telescope file search"})
    key("n", "<leader>fg", ":Telescope live_grep<CR>",  {desc = "Telescope grep search"})
    key("n", "<leader>fn", ":Telescope nerdy<CR>",      {desc = "Telescope nerd-icon search"})

     -- Buffer
     key("n",  "<S-l>",      ":bn<CR>",  {desc = "Goto next buffer"})
     key("n",  "<S-h>",      ":bp<CR>",  {desc = "Goto previous buffer"})
     key("n",  "<leader>c",  ":bd<CR>",  {desc = "Delete current buffer"})

     -- use the force
     key("n", "<leader>W",  ":w!<CR>",  {desc = "Force Write"})
     key("n", "<leader>Q",  ":q!<CR>",  {desc = "Force quit"})
     key("n", "<leader>C",  ":bd!<CR>", {desc = "Force Delete Current Buffer"})

     -- tweaks
     key("n", "n", "nzzzv", { desc = "Next search result (centered)" })
     key("n", "N", "Nzzzv", { desc = "Previous search result (centered)" })
     key("n", "<C-d>", "<C-d>zz", { desc = "Half page down (centered)" })
     key("n", "<C-u>", "<C-u>zz", { desc = "Half page up (centered)" })

     key("n", "<A-j>", ":m .+1<CR>==", { desc = "Move line down" })
     key("n", "<A-k>", ":m .-2<CR>==", { desc = "Move line up" })
     key("v", "<A-j>", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
     key("v", "<A-k>", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

     key("v", "<", "<gv", { desc = "Indent left and reselect" })
     key("v", ">", ">gv", { desc = "Indent right and reselect" })

     key("n", "J", "mzJ`z", { desc = "Join lines and keep cursor position" })
  '';
}
