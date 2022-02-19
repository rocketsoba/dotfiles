#!/bin/bash

php_config_print() {
    echo "# ----Automatically generated php config by rocketsoba/build-scripts----"
    if command -v php56 > /dev/null 2>&1; then
        echo 'source "/opt/remi/php56/enable"'
    fi
    if command -v composer > /dev/null 2>&1; then
        echo 'export COMPOSER_HOME=${HOME}/.composer'
    fi
    if [ -d ${HOME}/.composer/vendor/bin ]; then
        echo 'export PATH=${HOME}/.composer/vendor/bin:${PATH}'
    fi
    echo "# ----------------------------------------------------------------------"
}

cat ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.bak"
if grep "# ----Automatically generated php config by rocketsoba/build-scripts----" ${HOME}"/.bash_profile.bak" 2>&1 > /dev/null; then
    sed -e "/^# ----Automatically generated php config by rocketsoba\/build-scripts----/,/# ---/d" ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.tmp"
    cat ${HOME}"/.bash_profile.tmp" <(php_config_print) > ${HOME}"/.bash_profile"
    rm -f ${HOME}"/.bash_profile.tmp"
else
    cat ${HOME}"/.bash_profile.bak" <(php_config_print) > ${HOME}"/.bash_profile"
fi
