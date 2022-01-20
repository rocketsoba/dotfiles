#!/bin/bash

# 正規表現をエスケープせずに複数行の文字列を置換するコマンドが存在しないっぽいのでpythonで書き換えるべき
function_print() {
    sed -i -ze 's/if \[ -f ~\/\.bashrc \]; then\
	\. ~\/\.bashrc\
fi\
\
# User specific environment and startup programs\
\
PATH=$PATH:$HOME\/\.local\/bin:$HOME\/bin\
\
export PATH/if \[ -f ~\/\.bashrc \]; then\
    \. ~\/\.bashrc\
fi\
\
# User specific functions\
function share_history() {\
    history -a\
    history -c\
    history -r\
}\
function peco_search_history() {\
    local l=$(HISTTIMEFORMAT= history | sort -r | sed -e '\''s\/^ *\[0-9\]\\+ \\+\/\/'\'' | peco --query "$READLINE_LINE")\
    READLINE_LINE="$l"\
    READLINE_POINT=${#l}\
}\
\
# User specific environment and startup programs\
\
if \[ -z $TMUX \]; then\
    PATH=$PATH:$HOME\/\.local\/bin:$HOME\/bin\
fi\
\
export PATH/g' ${HOME}/.bash_profile
}

config_print() {
    echo "# ----Automatically generated config by rocketsoba/build-scripts--------"
    if command -v peco > /dev/null 2>&1; then
        echo 'if ! [ -z "${PS1}" ]; then'
        echo 'bind -x '\''"\C-r": peco_search_history'\'
        echo 'fi'
    fi
    if [ -f "/usr/share/git-core/contrib/completion/git-prompt.sh" ]; then
        echo 'source "/usr/share/git-core/contrib/completion/git-prompt.sh"'
        echo
        echo 'export PS1='\''[\[\e[38;5;210m\]\u@\h\[\e[0m\] \[\e[38;5;115m\]\w$(__git_ps1)\[\e[0m\]]\[\e[38;5;223m\]\$\!\[\e[0m\]: '\'
    else
        echo
        echo "export PS1='[\[\e[38;5;210m\]\u@\h\[\e[0m\] \[\e[38;5;115m\]\w\[\e[0m\]]\[\e[38;5;223m\]\$\!\[\e[0m\]: '"
    fi
    echo
    echo '# history settings'
    echo 'export TERM=xterm-256color'
    echo 'export HISTSIZE=100000'
    echo "HISTTIMEFORMAT='%Y-%m-%dT%T '"
    echo "PROMPT_COMMAND='share_history'"
    echo 'shopt -u histappend'
    echo "# ----------------------------------------------------------------------"
}

cat ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.bak"
if grep "# ----Automatically generated config by rocketsoba/build-scripts--------" ${HOME}"/.bash_profile.bak" 2>&1 > /dev/null; then
    sed -e "/^# ----Automatically generated config by rocketsoba\/build-scripts--------/,/# ---/d" ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.tmp"
    cat ${HOME}"/.bash_profile.tmp" <(config_print) > ${HOME}"/.bash_profile"
    rm -f ${HOME}"/.bash_profile.tmp"
else
    cat ${HOME}"/.bash_profile.bak" <(config_print) > ${HOME}"/.bash_profile"
fi

function_print
