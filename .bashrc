# -*- mode: sh -*-

if [ -f /etc/bash.bashrc ]
then
    source /etc/bash.bashrc
fi

emacs_base=emacs
emacsclient_base=emacsclient

if [[ "$(uname -s)" == *"CYGWIN"* ]]
then
    emacs_base=emacs-w32
    emacsclient_base=emacsclient-w32
fi

alias e="$emacsclient_base -t"
alias en="$emacs_base -nw"
alias ec="$emacsclient_base -c"
alias startemacs="$emacs_base --daemon --eval \"(load-project-management)\""

alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias la='ls --color=auto -a'
alias ll='ls --color=auto -l'
alias lh='ls --color=auto -lh'
alias lah='ls --color=auto -lah'
alias pf='ps -ef'
alias pfg='ps -ef | grep -v grep | grep'

export PATH=~/bin:$PATH
export PS1='[\u@\h \W]\$ '

if [ `which $emacs_base 2> /dev/null` ]; then
    export EDITOR="$emacs_base -nw"
fi
