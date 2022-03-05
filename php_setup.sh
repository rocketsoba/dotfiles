#!/bin/bash

php_config_print() {
    case $1 in
        php56)
            local COMPOSER_DIR=".composer56"
            local PHP_VERSION="php56"
            ;;
        php74)
            local COMPOSER_DIR=".composer74"
            local PHP_VERSION="php74"
            ;;
        php81)
            local COMPOSER_DIR=".composer81"
            local PHP_VERSION="php81"
            ;;
    esac
    echo "# ----Automatically generated php config by rocketsoba/build-scripts----"
    if command -v $PHP_VERSION > /dev/null 2>&1; then
        echo 'source "/opt/remi/'$PHP_VERSION'/enable"'
    fi
    if command -v composer > /dev/null 2>&1; then
        echo 'export COMPOSER_HOME=${HOME}/'$COMPOSER_DIR
        echo 'export PATH=${COMPOSER_HOME}/vendor/bin:${PATH}'
    fi
    echo "# ----------------------------------------------------------------------"
}

inject_emacs_phpctags() {
    case $1 in
        php56)
            local PHPCTAGS_URL="https://github.com/xcwen/ac-php/raw/362907ca3dac0b5525a6881678e0f07b82f7a77f/phpctags"
            ;;
        *)
            local PHPCTAGS_URL="https://github.com/xcwen/ac-php/raw/master/phpctags"
            ;;
    esac
    local AC_PHP_CORE_PATH=$(find ~/.emacs.d/ -name 'ac-php-core*' -type d | head -n1)
    if [ -d "$AC_PHP_CORE_PATH" ]; then
        if [ -f ${AC_PHP_CORE_PATH}/phpctags ]; then
            rm -f ${AC_PHP_CORE_PATH}/phpctags
        fi
        curl -Lso ${AC_PHP_CORE_PATH}/phpctags $PHPCTAGS_URL
        chmod +x ${AC_PHP_CORE_PATH}/phpctags
        rm -rf ~/.ac-php/
    fi
}

php_config_write() {
    cat ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.bak"
    if grep "# ----Automatically generated php config by rocketsoba/build-scripts----" ${HOME}"/.bash_profile.bak" 2>&1 > /dev/null; then
        sed -e "/^# ----Automatically generated php config by rocketsoba\/build-scripts----/,/# ---/d" ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.tmp"
    else
        cat ${HOME}"/.bash_profile.bak" > ${HOME}"/.bash_profile.tmp"
    fi

    case $1 in
        php56)
            cat ${HOME}"/.bash_profile.tmp" <(php_config_print $1) > ${HOME}"/.bash_profile"
            ;;
        php74)
            cat ${HOME}"/.bash_profile.tmp" <(php_config_print $1) > ${HOME}"/.bash_profile"
            ;;
        php81)
            cat ${HOME}"/.bash_profile.tmp" <(php_config_print $1) > ${HOME}"/.bash_profile"
                ;;
    esac
    rm -f ${HOME}"/.bash_profile.tmp"
}

case "$1" in
    php56)
        php_config_write "php56"
        inject_emacs_phpctags "php56"
        ;;
    php74)
        php_config_write "php74"
        inject_emacs_phpctags "php74"
        ;;
    php81)
        php_config_write "php81"
        inject_emacs_phpctags "php81"
        ;;
    *)
        php_config_write "php56"
        inject_emacs_phpctags "php56"
        ;;
esac
