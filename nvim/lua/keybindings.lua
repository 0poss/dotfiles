function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend('force', options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Ctrl+c to stop searching
map('n', '<C-c>', ':nohlsearch<cr>', {})

-- Very magic by default
map('n', '?', '?\\v', {})
map('n', '/', '/\\v', {})

-- No arrow keys
map('i', '<up>', '', {})
map('i', '<down>', '', {})
map('i', '<left>', '', {})
map('i', '<right>', '', {})

-- Create a session to the home directory
map('n', '<C-s>', ':mksession! ~/Session.vim', {})

-- Move between viewports
map('n', '<C-k>', '<C-w>k', {})
map('n', '<C-j>', '<C-w>j', {})
map('n', '<C-h>', '<C-w>h', {})
map('n', '<C-l>', '<C-w>l', {})

-- Set Copilot complete key to <C-Tab>
vim.cmd 'imap <silent><script><expr> <C-l> copilot#Accept("\\<CR>")'
vim.cmd 'let g:copilot_no_tab_map = v:true'
