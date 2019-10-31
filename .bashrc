# -*- mode: sh -*-

if [ -f /etc/bash.bashrc ]
then
    source /etc/bash.bashrc
fi

function ssh-retry()
{
    while [ 1 ]; do
        ssh -o ServerAliveInterval=5 -o ServerAliveCountMax=2 "$@"
        sleep 1
    done
}

function ssh-tmux()
{
    local hostname="$1"
    shift

    if [ "$hostname" == "" ]; then
        >&2 echo 'Usage: ssh-tmux hostname [tmux options...]'
        return
    fi

    ssh-retry -t "$hostname" tmux attach "$@"
}

function datetime()
{
    local start=$(date)
    $@
    local end=$(date)
    printf 'Started: %s\nEnded:   %s\n' "$start" "$end"
}

function emacsval()
{
    emacs --batch -l ~/.emacs --eval "$@"
}

function eclival()
{
    emacsclient --eval "$@"
}

function link_dotfiles()
{
    for f in $(ls -a); do
        if [ -f "$f" ]; then
            ln -s "$(pwd -P)/$f" "$HOME/$f"
        fi
    done
}

alias e='emacsclient -a "" -t'
alias ec='emacsclient'
alias ecn='emacsclient -n'
alias en='emacs -nw'
alias startemacs='emacs --daemon --eval "(load-project-management)"'

alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias lh='ls --color=auto -lh'
alias ll='ls --color=auto -l'
alias lah='ls --color=auto -lah'
alias nohist='unset HISTFILE'
alias pfg='ps -ef | grep -v grep | grep'
alias sctl='systemctl'
alias sudo='sudo '
alias tmux='tmux -2'

export PATH=~/bin:$PATH
export PS1='[\u@\h \W]\$ '
export HISTCONTROL=ignorespace:ignoredups
export HISTTIMEFORMAT="%Y/%m/%d %T "

case "$TERM" in
    *"xterm"*|*"screen"*)
        export PROMPT_COMMAND='printf "\033]0;%s@%s:%s\007" "${USER}" "${HOSTNAME%%.*}" "${PWD/#$HOME/~}"'
        ;;
    *)
        unset PROMPT_COMMAND
        ;;
esac

if [ $(which emacs 2> /dev/null) ]
then
    export EDITOR='emacs -nw'
fi

if [ -f ~/local.sh ]
then
    source ~/local.sh
fi
