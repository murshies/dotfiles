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

alias ls='ls --color=auto'

export PATH=~/bin:$PATH
export PS1='[\u@\h \W]\$ '
export EDITOR="$emacs_base -nw"
