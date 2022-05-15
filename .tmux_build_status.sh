#!/bin/bash

# https://chitoku.jp/programming/bash-getopts-long-options#--foo-bar-%E3%82%92%E5%87%A6%E7%90%86%E3%81%99%E3%82%8B%E6%96%B9%E6%B3%95
while getopts "au-:" OPT; do
    # OPTIND 番目の引数を optarg へ代入
    OPTARG2="${!OPTIND}"
    if [ "$OPT" = - ]; then
       OPT="${OPTARG}"
    fi

    case "$OPT" in
        u)
            BASIC_USER=$OPTARG2
            shift
            ;;
        user)
            BASIC_USER=$OPTARG2
            shift
            ;;
    esac
done
shift $((OPTIND - 1))

if ! tmux showenv -g TMUX_STATUS_TIMER2 > /dev/null 2>&1; then
    tmux setenv -g TMUX_STATUS_TIMER2 $(date +%s)
fi
eval $(tmux showenv -g TMUX_STATUS_TIMER2)

if ! [ -z $1 ] && ! [ -z $BASIC_USER ] && [ $(expr $(date +%s) - $TMUX_STATUS_TIMER2) -gt 30 ]; then
    URL=$1
    tmux setenv -g TMUX_STATUS_TIMER2 $(date +%s)

    JENKINS_INFO=""
    if API_RESULT=$(curl -s -u $BASIC_USER $URL); then
        for FAILED_BUILD in $(echo $API_RESULT | jq -r '.jobs[] | select(.color | contains("red")) | .name'); do
            JENKINS_INFO="["$FAILED_BUILD":#[fg=colour1]failed#[fg=colour115]]"$JENKINS_INFO
        done
    fi

    if [ -z $JENKINS_INFO ]; then
        tmux setenv -g TMUX_JENKINS_INFO ""
    else
        tmux setenv -g TMUX_JENKINS_INFO $JENKINS_INFO
    fi
fi

eval $(tmux showenv -g TMUX_JENKINS_INFO)
echo -n $TMUX_JENKINS_INFO

# echo "ELAPSSED SEC: "$(expr $(date +%s) - $TMUX_STATUS_TIMER2)
