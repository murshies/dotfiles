#!/bin/bash

emacs --batch --eval "(progn (load-file \"$HOME/.emacs\") (install-selected-packages))"
