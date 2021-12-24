local ts_config = require("nvim-treesitter.configs")

ts_config.setup {
	ensure_installted = {
		"cpp",
		"c",
	},
	highlight = {
		enable = true,
		use_languagetree = true
	}
}
