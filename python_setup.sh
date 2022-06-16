#!/bin/bash

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
    if [ -d $PYENV_ROOT ] && command -v pyenv > /dev/null 2>&1; then
        echo 'export PYENV_ROOT=${HOME}/.pyenv'
        echo 'eval "$(pyenv init - --no-rehash)"'
        echo 'eval "$(pyenv init --path --no-rehash)"'
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
