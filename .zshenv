export VISUAL=em
export EDITOR=em
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

alias ssh="kitty +kitten ssh"
alias be='bundle exec'
alias em='emacsclient -nw -a ""'
alias rm='rm -rf'
alias ls='ls -oh'
alias psql='psql -h localhost -U eyr'
alias d='cd $HOME/dev/eyr-phoenix; docker-compose --file docker-compose.dev.yml'
alias psqldev='ssh -f -CNL 8090:localhost:5432 eyr-app-dev ; psql --port 8090'
alias psqlstaging='ssh -f -CNL 8091:localhost:5432 eyr-app-staging ; psql --port 8091 -h 10.42.30.4'
alias mobdev='cd $HOME/dev/eyr-mobile ; git co .env ; git co ios/Podfile.lock; ln -s -f /Users/alan/dev/eyr-mobile/.env.dev .env'
alias moblocal='cd $HOME/dev/eyr-mobile ; git co .env ; git co ios/Podfile.lock ; ln -s -f /Users/alan/dev/eyr-mobile/.env.local.alan .env'


