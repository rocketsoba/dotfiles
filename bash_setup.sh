#!/bin/bash

# 正規表現をエスケープせずに複数行の文字列を置換するコマンドが存在しないっぽいのでpythonで書き換えるべき
replace_path() {
    sed -i -ze 's/PATH=$PATH:$HOME\/\.local\/bin:$HOME\/bin\
\
export PATH/export PATH=\/usr\/local\/bin:\/usr\/bin:$HOME\/\.local\/bin:$HOME\/bin\
/g' ${HOME}/.bash_profile
}

print_bashrc_function() {
    echo "# ----Automatically generated config by rocketsoba/build-scripts--------"
    echo 'function peco_search_history() {'
    echo '    local l=$(HISTTIMEFORMAT= history | tac | sed -e '\''s/^ *[0-9]\+ \+//'\'' | peco --query "$READLINE_LINE")'
    echo '    READLINE_LINE="$l"'
    echo '    READLINE_POINT=${#l}'
    echo '}'
    echo
    echo 'function diff() {'
    echo '    command git diff --no-index $@'
    echo '}'
    echo "# ----------------------------------------------------------------------"
}

print_config() {
    echo "# ----Automatically generated config by rocketsoba/build-scripts--------"
    if command -v peco > /dev/null 2>&1; then
        echo 'if ! [ -z "$PS1" ]; then'
        echo '    bind -x '\''"\C-r": peco_search_history'\'
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
    echo "PROMPT_COMMAND='history -a && history -c && history -r'"
    echo 'shopt -u histappend'
    echo
    echo '# local bash-completion'
    echo 'if [ -d ~/.bash_completion.d/ ]; then'
    echo '    for i in $(find ~/.bash_completion.d/ -maxdepth 1 -name '\''*.bash'\'' -type f); do'
    echo '        source $i'
    echo '    done'
    echo 'fi'
    echo "# ----------------------------------------------------------------------"
}

write_config() {
    replace_path


    cat ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.bak"
    if grep "# ----Automatically generated config by rocketsoba/build-scripts--------" ${HOME}"/.bash_profile.bak" 2>&1 > /dev/null; then
        sed -e "/^# ----Automatically generated config by rocketsoba\/build-scripts--------/,/# ---/d" ${HOME}"/.bash_profile" > ${HOME}"/.bash_profile.tmp"
    else
        cat ${HOME}"/.bash_profile.bak" > ${HOME}"/.bash_profile.tmp"
    fi
    cat ${HOME}"/.bash_profile.tmp" <(print_config) > ${HOME}"/.bash_profile"
    rm -f ${HOME}"/.bash_profile.tmp"


    cat ${HOME}"/.bashrc" > ${HOME}"/.bashrc.bak"
    if grep "# ----Automatically generated config by rocketsoba/build-scripts--------" ${HOME}"/.bashrc.bak" 2>&1 > /dev/null; then
        sed -e "/^# ----Automatically generated config by rocketsoba\/build-scripts--------/,/# ---/d" ${HOME}"/.bashrc" > ${HOME}"/.bashrc.tmp"
    else
        cat ${HOME}"/.bashrc.bak" > ${HOME}"/.bashrc.tmp"
    fi
    cat ${HOME}"/.bashrc.tmp" <(print_bashrc_function) > ${HOME}"/.bashrc"
    rm -f ${HOME}"/.bashrc.tmp"
}

write_config
