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

nodejs_config_print() {
    local NVM_ROOT=${HOME}"/.nvm"
    echo "# ----Automatically generated node.js config by rocketsoba/build-scripts----"
    if [ -d $NVM_ROOT ] && [ -f ${NVM_ROOT}/nvm.sh ]; then
        echo 'export NVM_DIR=${HOME}/.nvm'
        echo 'source ${NVM_DIR}/nvm.sh'
    fi
    echo "# -------------------------------------------------------------------------"
}

nodejs_config_write() {
    cat ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.bak"
    if grep "# ----Automatically generated node.js config by rocketsoba/build-scripts----" ${HOME}"/.bash_profile.bak" 2>&1 > /dev/null; then
        sed -e "/^# ----Automatically generated node.js config by rocketsoba\/build-scripts----/,/# ---/d" ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.tmp"
    else
        cat ${HOME}"/.bash_profile.bak" > ${HOME}"/.bash_profile.tmp"
    fi

    cat ${HOME}"/.bash_profile.tmp" <(nodejs_config_print) > ${HOME}"/.bash_profile"
    rm -f ${HOME}"/.bash_profile.tmp"
}

nodejs_config_write
