if status is-interactive
    # Commands to run in interactive sessions can go here
end

alias d=dirh
bind \cn "nextd; echo; dirh; commandline -f repaint"
bind \cp "prevd; echo; dirh; commandline -f repaint"

alias ls="ls --color"
alias l="ls -lh"
alias ll="l -A"

# equery aliases
alias eqb="equery belongs"
alias eqk="equery check"
alias eqd="equery depends"
alias eqg="equery depgraph"
alias eqf="equery files"
alias eqa="equery has"
alias eqh="equery hasuse"
alias eky="equery keywords"
alias eql="equery list"
alias eqm="equery meta"
alias eqs="equery size"
alias equ="equery uses"
alias eqw="equery which"
