#!/bin/bash

PROGNAME=$(basename $0)
BIN_PREFIX=$HOME'/.opt'

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
    esac
done
shift $((OPTIND - 1))

pyenv_bin_copy() {
    local PYENV_PATH=${HOME}"/.pyenv/bin/pyenv"
    local PYENV_BIN_DEST=${HOME}"/.opt/pyenv/bin"
    if [ -f $PYENV_PATH ] && ! [ -d $PYENV_BIN_DEST ]; then
        mkdir -p $PYENV_BIN_DEST
        ln -s $PYENV_PATH ${PYENV_BIN_DEST}"/pyenv"
    fi
}

python_config_print() {
    local PYENV_ROOT=${HOME}"/.pyenv"
    echo "# ----Automatically generated python config by rocketsoba/build-scripts----"
    if [ -d $PYENV_ROOT ]; then
        echo 'export PYENV_ROOT=${HOME}/.pyenv'
        if command -v pyenv > /dev/null 2>&1; then
            echo 'eval "$(pyenv init - --no-rehash)"'
            echo 'eval "$(pyenv init --path --no-rehash)"'
        else
            if [ -d $BIN_PREFIX"/pyenv/bin" ]; then
                echo 'export PATH='$BIN_PREFIX'/pyenv/bin:${PATH}'
                echo 'eval "$(pyenv init - --no-rehash)"'
                echo 'eval "$(pyenv init --path --no-rehash)"'
            fi
        fi
    fi
    echo "# -------------------------------------------------------------------------"
}

python_config_write() {
    cat ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.bak"
    if grep "# ----Automatically generated python config by rocketsoba/build-scripts----" ${HOME}"/.bash_profile.bak" 2>&1 > /dev/null; then
        sed -e "/^# ----Automatically generated python config by rocketsoba\/build-scripts----/,/# ---/d" ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.tmp"
    else
        cat ${HOME}"/.bash_profile.bak" > ${HOME}"/.bash_profile.tmp"
    fi

    cat ${HOME}"/.bash_profile.tmp" <(python_config_print) > ${HOME}"/.bash_profile"
    rm -f ${HOME}"/.bash_profile.tmp"
}

pyenv_bin_copy
python_config_write
