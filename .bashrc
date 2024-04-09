# -*- mode: sh -*-

if [ -f /etc/bash.bashrc ]; then
    source /etc/bash.bashrc
fi

function ssh-retry() {
    while [ 1 ]; do
        ssh -o ServerAliveInterval=5 -o ServerAliveCountMax=2 "$@"
        sleep 1
    done
}

function datetime() {
    local start=$(date)
    time $@
    local end=$(date)
    printf 'Started: %s\nEnded:   %s\n' "$start" "$end"
}

function magit() {
    emacs $EMACS_PARAMS --eval "(progn (magit-status \"$@\") (delete-other-windows))"
}

function tmagit() {
    EMACS_PARAMS='-nw'
    magit $@
}

function link-dotfiles() {
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

function basic-prompt() {
    export PS1='[\u@\h \W]\$ '
}

function normal-prompt() {
    export PS1='$? \[\033[01;32m\][\w]\[\033[00m\]\n\[\033[01;34m\]\u@\h\[\033[00m\] $ '
}

function goto-realdir() {
    local target="$1"
    local real_path=$(readlink "$target")

    if [ "$real_path" == "" ]; then
        echo "$target is not a link"
        return
    fi

    local realdir=$(dirname "$real_path")
    cd "$realdir"
}

function venv-shim() {
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
        chmod 0755 "$cmd_loc"
        echo "Created $cmd_loc"
    done
}

function eww() {
    if [ "$1" == "-nw" ]; then
        local url="$2"
        local flags="-nw"
    else
        local url="$1"
        local flags=""
    fi
    emacs $flags --eval "(eww \"$url\")"
}

function __aupdate() {
    echo "Update at $(date +%T)"
    sudo apt-get update
    sudo NEEDRESTART_MODE=a DEBIAN_FRONTEND=noninteractive apt-get upgrade -y
    echo "Finished at $(date +%T)"
}

function aupdate() {
    if [[ $(which tmux) && "$TERM" = *"xterm"* ]]; then
        LOG_FILE=$HOME/last_update.log
        tmux new -s aupdate "bash -lc '__aupdate | tee $LOG_FILE'"
        cat $LOG_FILE
    else
        LOG_FILE=$HOME/last_update.log ; __aupdate | tee $LOG_FILE
    fi
}

function gpgarchive-open() {
    local archive="$1"
    local dest="$2"
    if [ -z "$archive" ] || [ -z "$dest" ]; then
        echo "Usage: gpgarchive-open <archive file> <dest directory>"
        return 1
    fi
    mkdir -p "$dest"
    gpg -d "$archive" | tar -xf - -C "$dest"
    echo "$archive extracted to $dest"
}

function gpgarchive-create() {
    local source_dir="$1"
    local dest_archive="$2"
    if [ -z "$source_dir" ] || [ -z "$dest_archive" ]; then
        echo "Usage: gpgarchive-create <source dir> <dest archive>"
        return 1
    fi
    tar -cvf - -C "$source_dir" . | gpg -c > "$dest_archive"
    echo "$source_dir saved to $dest_archive"
}

function ramdisk() {
    local size="$1"
    local dest="$2"
    if [ -z "$size" ] || [ -z "$dest" ] || [ "$dest" == "-h" ]; then
        echo "Usage: ramdisk <size> <dest dir>"
        return 1
    fi
    mkdir -p "$dest"
    sudo mount -t tmpfs -o "size=$size,uid=$(id -u),gid=$(id -g)" tmpfs "$dest"
}

alias ec='emacsclient'
alias ecn='emacsclient -n'
alias en='emacs -nw'
alias startemacs='emacs --daemon'

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
export HISTCONTROL=ignorespace:ignoredups
export HISTTIMEFORMAT="%Y/%m/%d %T "

normal-prompt

case "$TERM" in
    *"xterm"*|*"screen"*|*"eterm"*)
        export PROMPT_COMMAND='printf "\n"'
        ;;
    *)
        unset PROMPT_COMMAND
        ;;
esac

if [ -n "$EMACS_DAEMON" ]; then
    export EDITOR='emacsclient'
    alias e='emacsclient -n'
elif [ -n "$INSIDE_EMACS" ]; then
    # The current shell is bring run inside of emacs, but emacs is not in
    # daemon mode. Using an external editor like vim avoids an "emacs inside of
    # emacs" situation.
    export EDITOR='vim'
    alias e='vim'
elif [ $(which emacs 2> /dev/null) ]; then
    export EDITOR='emacs -nw'
    alias e='emacsclient -a "" -t'
fi

if [ -f ~/local.sh ]; then
    source ~/local.sh
fi
