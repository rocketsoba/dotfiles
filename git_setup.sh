#!/bin/bash

if command -v git > /dev/null 2>&1; then
    if ! git --no-pager  config --get-all url."git@github.com:".pushInsteadOf > /dev/null 2>&1; then
        git config --global       url."git@github.com:".pushInsteadOf "git://github.com/"
        git config --global --add url."git@github.com:".pushInsteadOf "https://github.com/"
    fi
    git config --global       url."git://github.com/".insteadOf "https://github.com/"
    git config --global       merge.ff false
    git config --global       pull.ff only
    git config --global       push.default current
    git config --global       diff.algorithm histogram
    git config --global       alias.tree 'log --graph --all --format="%x09%C(cyan bold)%an%Creset%x09%C(yellow)%h%Creset %C(magenta reverse)%d%Creset %s"'
    git config --global       alias.st 'status -sb'
    git config --global       core.pager 'less -+FX -M'
fi
