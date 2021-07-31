#!/bin/bash

if ! [ -z $TMUX_STATUS_TIMER ]; then
    if [ $(expr $(date +%s) - $TMUX_STATUS_TIMER) -gt 30 ]; then
        tmux setenv -g TMUX_STATUS_TIMER $(date +%s)
        echo -n '' > $HOME/.tmux_panelist2
        for i in $(seq 0 $(expr $(tmux list-pane -a | wc -l) - 1)); do
            tmux pipe-pane -t $i "echo #{pane_current_path} >> $HOME/.tmux_panelist2"
            sleep 0.05
        done
        cat $HOME/.tmux_panelist2 | sort | uniq > $HOME/.tmux_panelist

        git_info=""
        for path in $(cat $HOME/.tmux_panelist); do
            git_result=$(git --no-pager -C $path diff HEAD --shortstat 2> /dev/null)
            if ! [ -z "${git_result}" ]; then
                git_info="["$(basename $path)":#[fg=colour2]+"$(echo -n $(echo $git_result | grep -Po '[0-9]+ (?=insertion)'))"#[fg=colour1]-"$(echo -n $(echo $git_result | grep -Po '[0-9]+ (?=deletion)'))"#[fg=colour115]]"${git_info}
            fi
        done
        tmux setenv -g TMUX_GIT_INFO $git_info
    fi
else
    tmux setenv -g TMUX_STATUS_TIMER $(date +%s)
fi

eval $(tmux showenv -gs TMUX_GIT_INFO)
echo $TMUX_GIT_INFO
