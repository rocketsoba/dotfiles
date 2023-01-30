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
    else
        if [ -d $BIN_PREFIX"/composer/bin" ]; then
            echo 'export PATH='$BIN_PREFIX'/composer/bin:${PATH}'
            echo 'export COMPOSER_HOME=${HOME}/'$COMPOSER_DIR
            echo 'export PATH=${COMPOSER_HOME}/vendor/bin:${PATH}'
        fi
    fi
    echo "# ----------------------------------------------------------------------"
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
        ;;
    php74)
        php_config_write "php74"
        ;;
    php81)
        php_config_write "php81"
        ;;
    *)
        php_config_write "php56"
        ;;
esac
