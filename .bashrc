# -*- mode: sh -*-

if [ -f /etc/bash.bashrc ]
then
    source /etc/bash.bashrc
fi

if [ -f ~/local.sh ]
then
    source ~/local.sh
fi

alias e="emacsclient -t"
alias en="emacs -nw"
alias ec="emacsclient -c"
alias startemacs="emacs --daemon --eval \"(load-project-management)\""

alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias la='ls --color=auto -a'
alias ll='ls --color=auto -l'
alias lh='ls --color=auto -lh'
alias lah='ls --color=auto -lah'
alias lg='ls --color=auto -lah | grep'
alias pf='ps -ef'
alias pfg='ps -ef | grep -v grep | grep'
alias sctl='systemctl'
alias sudo='sudo '

export PATH=~/bin:$PATH
export PS1='[\u@\h \W]\$ '

if [ `which emacs 2> /dev/null` ]; then
    export EDITOR="emacs -nw"
fi
