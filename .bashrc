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
    local session="$1"
    shift

    if [ "$hostname" == "" ]; then
        >&2 echo "Usage: ${FUNCNAME[0]} hostname [tmux options...]"
        return
    fi

    if [ "$session" != "" ]; then
        ssh-retry -t "$hostname" tmux -2 $@ new-session -A -s "$session"
    else
        ssh-retry -t "$hostname" tmux -2 $@ attach
    fi
}

function datetime()
{
    local start=$(date)
    time $@
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

function magit()
{
    emacs $EMACS_PARAMS --eval "(progn (magit-status \"$@\") (delete-other-windows))"
}

function tmagit()
{
    EMACS_PARAMS='-nw'
    magit $@
}

function link-dotfiles()
{
    local curr_dir=$(pwd -P)
    grep 'url.*dotfiles' .git/config > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo 'Not in the dotfiles repo, exiting'
        return
    fi
    for f in $(find -maxdepth 1 -type f -name '.*'); do
        local base_f=$(basename "$f")
        local target="$HOME/$base_f"
        if [ -f "$target" ]; then
            echo "Warning: $target exists already, overwriting"
        fi
        ln -sf "$curr_dir/$base_f" "$target"
    done
}

function basic-prompt()
{
    export PS1='[\u@\h \W]\$ '
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

function venv-shim()
{
    if (( $# < 2 )); then
        echo "Usage: ${FUNCNAME[0]} venv_root cmd1 [cmd2...]"
        return
    fi
    local venv_root=$(readlink -f "$1")
    local arg_str='$@' # The literal string $@, not the parameter list
    shift
    for cmd in "$@"; do
        local cmd_loc=~/bin/$cmd
        printf "#!/bin/bash\n\nsource '$venv_root/bin/activate'\n$cmd $arg_str\n" > "$cmd_loc"
        chmod 755 "$cmd_loc"
        echo "Created $cmd_loc"
    done
}

function eww()
{
    if [ "$1" == "-nw" ]; then
        local url="$2"
        local flags="-nw"
    else
        local url="$1"
        local flags=""
    fi
    emacs $flags --eval "(eww \"$url\")"
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
alias sudo='sudo '
alias tmux='tmux -2'
alias timestamp='date -u +%Y%m%dT%H%M%SZ'

export PATH=~/bin:$PATH
export PS1='\n\[\033[01;32m\][\w]\[\033[00m\]\n\[\033[01;34m\]\u@\h\[\033[00m\] $ '
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

if [ -n "$EMACS_DAEMON" ]; then
    # If we're running emacs in a daemon, use emacsclient as the editor.
    export EDITOR='emacsclient'
elif [ -n "$INSIDE_EMACS" ]; then
    # If we're inside emacs but not using a daemon, use vim as the editor. This
    # is so we don't get emacs inside of emacs when running the editor inside
    # of something like vterm.
    export EDITOR='vim'
elif [ $(which emacs 2> /dev/null) ]; then
    # Otherwise, if we're on the command line and emacs is installed, use
    # terminal emacs as the editor.
    export EDITOR='emacs -nw'
fi

if [ -f ~/local.sh ]
then
    source ~/local.sh
fi
