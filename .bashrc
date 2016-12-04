# -*- mode: sh -*-

if [ -f /etc/bash.bashrc ]
then
    source /etc/bash.bashrc
fi

alias e='emacsclient -a "" -t'
alias ec='emacsclient'
alias en='emacs -nw'
alias startemacs='emacs --daemon --eval "(load-project-management)"'

alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias ll='ls --color=auto -l'
alias lah='ls --color=auto -lah'
alias nohist='unset HISTFILE'
alias pfg='ps -ef | grep -v grep | grep'
alias sctl='systemctl'
alias sudo='sudo '

export PATH=~/bin:$PATH
export PS1='[\u@\h \W]\$ '
export HISTCONTROL=ignorespace:ignoredups

if [ $(which emacs 2> /dev/null) ]; then
    export EDITOR='emacs -nw'
fi

if [ -f ~/local.sh ]
then
    source ~/local.sh
fi
