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

function link-dotfiles()
{
    for f in $(ls -a); do
        if [ -f "$f" ]; then
            ln -s "$(pwd -P)/$f" "$HOME/$f"
        fi
    done
}

function refresh-git-repos()
{
    local search_root="$1"

    if [ "$search_root" == "" ]; then
        search_root='.'
    fi

    search_root=$(realpath -s "$search_root")

    for dir in $(find "$search_root" -type d -not -path "*/.git*"); do
        local branch=$(cd "$dir" && git rev-parse --abbrev-ref HEAD 2>/dev/null)
        if [ "$branch" != "" ]; then
            # This is a git repo.
            if [ -d "$dir/.git" ]; then
                # This is the top level of a repo. Determine what the default
                # branch is, and if the repo is on it run a git pull.
                default_branch=$(cd "$dir" && git symbolic-ref --short HEAD)
                if [ "$default_branch" == "" ]; then
                    default_branch='master'
                    echo "Could not determine default branch for $dir, defaulting to master"
                fi
                if [ "$branch" == "$default_branch" ]; then
                    # Repo is on the master branch.
                    echo "Updating $dir"
                    (cd "$dir" && git pull)
                else
                    echo "$dir is on branch $branch (default is $default_branch)"
                fi
            fi
        fi
    done
}

function goto-realdir()
{
    local target="$1"
    local real_path=$(readlink "$target")

    if [ "$real_path" == "" ]; then
        echo "$target is not a link"
        return
    fi

    local realdir=$(dirname "$real_path")
    cd "$realdir"
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
