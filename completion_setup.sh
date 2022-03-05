#!/bin/bash

if ! [ -d ~/.bash_completion.d/ ]; then
    mkdir ~/.bash_completion.d/
fi

curl -Lso ~/.bash_completion.d/phpunit.completion.bash https://raw.githubusercontent.com/sjorek/phpunit-bash-completion/master/phpunit-completion.bash
curl -Lso ~/.bash_completion.d/composer.completion.bash https://raw.githubusercontent.com/Bash-it/bash-it/master/completion/available/composer.completion.bash
curl -Lso ~/.bash_completion.d/tmux.completion.bash https://raw.githubusercontent.com/Bash-it/bash-it/master/completion/available/tmux.completion.bash

sed -i -e 's/^about-completion.*//g' ~/.bash_completion.d/composer.completion.bash
sed -i -e 's/^cite.*//g' ~/.bash_completion.d/composer.completion.bash
