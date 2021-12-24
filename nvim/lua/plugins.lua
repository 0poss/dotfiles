local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1',
        'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd 'packadd packer.nvim'
end

local use = require('packer').use

return require('packer').startup(
function()
	-- Status line
	use 'itchyny/lightline.vim'

	-- Theme
	use 'rakr/vim-one'

	-- Semantic language support
	use 'neovim/nvim-lspconfig'
	use 'hrsh7th/nvim-cmp' -- Autocompletion
	use 'hrsh7th/cmp-nvim-lsp'
	use 'hrsh7th/cmp-vsnip'
	use 'hrsh7th/cmp-path'
	use 'hrsh7th/cmp-buffer'
	use 'simrat39/rust-tools.nvim' -- Rust

	-- Syntactic language support
	use 'nvim-treesitter/nvim-treesitter'
end
)
