vim.opt.shell = '/bin/bash'

vim.opt.autoindent = true
vim.opt.scrolloff = 2
vim.opt.showmode = false
vim.opt.wrap = false
vim.opt.signcolumn = 'yes'
vim.opt.cursorline = true

vim.opt.undodir = "/tmp/nvimdid"
vim.opt.undofile = true

vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.tabstop = 4
vim.opt.expandtab = true
vim.api.nvim_command "au Filetype c,cpp,haskell set shiftwidth=2 softtabstop=2 tabstop=2 expandtab"

vim.api.nvim_command "autocmd BufRead,BufNewFile *.sage set filetype=python"

-- Proper search
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Number column and color column
-- vim.opt.relativenumber = true
vim.opt.number = true
vim.opt.colorcolumn = "80"
vim.opt.showcmd = true -- Show (partial) command in status line.

-- Mouse
vim.opt.mouse = "a"

-- Theme
vim.opt.termguicolors = true
vim.cmd "colorscheme one"
vim.g.lightline = { colorscheme = 'one' }
vim.cmd "set background=light"
