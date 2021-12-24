function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend('force', options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

local opt = {}

-- Ctrl+h to stop searching
map('n', '<C-h>', ':nohlsearch<cr>', opt)
map('v', '<C-h>', ':nohlsearch<cr>', opt)

-- Very magic by default
map('n', '?', '?\\v', opt)
map('n', '/', '/\\v', opt)

-- No arrow keys
map('i', '<up>', '', opt)
map('i', '<down>', '', opt)
map('i', '<left>', '', opt)
map('i', '<right>', '', opt)
