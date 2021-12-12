if status is-interactive
    # Commands to run in interactive sessions can go here
end

alias d=dirh
bind \cn "nextd; echo; dirh; commandline -f repaint"
bind \cp "prevd; echo; dirh; commandline -f repaint"

alias ls="ls --color"
alias l="ls -lh"
alias ll="l -A"
