#!/bin/bash

# 正規表現をエスケープせずに複数行の文字列を置換するコマンドが存在しないっぽいのでpythonで書き換えるべき
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
# User specific environment and startup programs\
\
if \[ -z $TMUX \]; then\
    PATH=$PATH:$HOME\/\.local\/bin:$HOME\/bin\
fi\
\
export PATH\
/g' ${HOME}/.bash_profile
