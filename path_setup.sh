#!/bin/bash

PROGNAME=$(basename $0)
BIN_PREFIX=$HOME'/.opt'
BASH_PROFILE=$HOME'/.bash_profile'

# https://chitoku.jp/programming/bash-getopts-long-options#--foo-bar-%E3%82%92%E5%87%A6%E7%90%86%E3%81%99%E3%82%8B%E6%96%B9%E6%B3%95
while getopts "a-:" OPT; do
    # OPTIND 番目の引数を optarg へ代入
    OPTARG2="${!OPTIND}"
    if [ "$OPT" = - ]; then
       OPT="${OPTARG}"
    fi

    case "$OPT" in
        prefix)
            BIN_PREFIX=$OPTARG2
            shift
            ;;
        bashprofile)
            BASH_PROFILE=$OPTARG2
            shift
            ;;
    esac
done
shift $((OPTIND - 1))

binsearch_help () {
    echo "Usage: $PROGNAME [--prefix prefix] [--bashprofile path] [COMMAND]"
    echo
    echo "Commands:"
    echo "    show"
    echo "        show searched result"
    echo "    insert"
    echo "        insert searched result into .bash_profile"
}

binsearch_find () {
    local LOCAL_PATH=($(find $BIN_PREFIX -maxdepth 2 -type d -name 'bin' | sort))

    echo "# ----Automatically generated PATH by rocketsoba/build-scripts----------"
    echo "LOCAL_PATH=("

    for val in "${LOCAL_PATH[@]}";do
        if ! echo $val | grep -E "composer|pyenv" 2>&1 > /dev/null; then
            echo "    "\'$val\'
        fi
    done

    echo ")"
    echo 'LOCAL_PATH_STR="$(IFS=:; echo "${LOCAL_PATH[*]}")"'
    echo 'export PATH=${LOCAL_PATH_STR}:${PATH}'
    echo "# ----------------------------------------------------------------------"
}

binsearch_insert () {
    cat ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.bak"
    if grep "# ----Automatically generated PATH by rocketsoba/build-scripts----------" ${HOME}"/.bash_profile.bak" 2>&1 > /dev/null; then
        sed -e "/^# ----Automatically generated PATH by rocketsoba\/build-scripts----------/,/# ---/d" ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.tmp"
    else
        cat ${HOME}"/.bash_profile.bak" > ${HOME}"/.bash_profile.tmp"
    fi
    cat ${HOME}"/.bash_profile.tmp" <(binsearch_find) > ${HOME}"/.bash_profile"
    rm -f ${HOME}"/.bash_profile.tmp"
}

case "$1" in
    show)
        binsearch_find
        ;;
    insert)
        binsearch_insert
        ;;
    *)
        binsearch_help
        ;;
esac
