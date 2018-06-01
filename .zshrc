# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="lambda-mod"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.

plugins=(git colored-man colorize github docker brew osx zsh-syntax-highlighting kubectl)

# User configuration

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
export LANG=en_US.UTF-8


# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

export VISUAL=em

alias be='bundle exec'
alias em='emacsclient -nw'
alias rm='rm -rf'
alias ls='ls -oh'

# shh-agent managing
if [ ! -S ~/.ssh/ssh_auth_sock ]; then
  eval `ssh-agent`
  ln -sf "$SSH_AUTH_SOCK" ~/.ssh/ssh_auth_sock
fi
export SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock
ssh-add -l > /dev/null || ssh-add

# fix rbenv
eval "$(rbenv init -)"

# IOT mangOH
# THIS SHIT IS CANCER. IT WILL BREAK YOUR PATH AND PERL AND GIT
# . /opt/swi/y22-ext-SWI9X07Y_02.16.02.00/environment-setup-armv7a-neon-poky-linux-gnueabi

# export LEGATO_ROOT=~/dev/iot/legato_framework_18.04/legato
# export WP76XX_SYSROOT=/opt/swi/y22-ext-wp76xx/sysroots/armv7a-neon-poky-linux-gnueabi
# alias cfglegato="pushd . && cd ${LEGATO_ROOT} && source ./bin/configlegatoenv; popd"

# export PKG_CONFIG_PATH=/opt/swi/y22-ext-SWI9X07Y_02.16.02.00/sysroots/armv7a-neon-poky-linux-gnueabi/usr/lib/pkgconfig:/opt/swi/y22-ext-SWI9X07Y_02.16.02.00/sysroots/armv7a-neon-poky-linux-gnueabi/usr/share/pkgconfig:/usr/local/lib/pkgconfig/

# END

export GCP_CREDENTIALS_FILE=/home/hoarf/docs/secret_staging.json
export GOOGLE_MAPS_API_KEY="AIzaSyBKa-Atv1NyLJ52nd2ckcY-upFK3Mw0Img"
