# ZSH RC File
# If not running interactively, don't do anything
#[[ $- != *i* ]] && return

export PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
zstyle :compinstall filename '/User/david/.zshrc'

# History
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=90000
#setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
#bindkey -e

bindkey "^[[F" end-of-line
bindkey "^[[H" beginning-of-line
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char
source ~/.zprezto/init.zsh

eval $( dircolors -b $HOME/.dir_colors )
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Direnv
eval "$(direnv hook zsh)"

#----------------------------------------------------------------------#
# Autocompletion and prompt
#----------------------------------------------------------------------#

autoload -Uz compinit
compinit

zstyle ':completion:*' menu select

# Jupyter executable
alias jupyter='python3 ~/Library/Python/3.6/bin/jupyter-notebook' 
alias lsmp3='ls -1 --indicator-style=none *.mp3'
alias lsepub='ls -1 -R --indicator-style=none | grep epub'
# export GREP_COLOR="1;31"
# alias grep='grep --color=auto'
export LESS="-R"
alias c='cd'
alias l='ls'

alias vim='nvim'
alias vi='nvim'
# alias tmux="TERM=screen-256color-bce tmux"
# export TERM=screen-256color
# alias python=python3
# export TERM=xterm-256color

#----------------------------------------------------------------------#
# PATH
#----------------------------------------------------------------------#
export PATH="$PATH:$HOME/bin"
export HPC="${HOME}/Projects/ETH/HPC/"
export CQP="${HOME}/Projects/ETH/CQP/"
export VW="${HOME}/Projects/Vorwerk/"
export TMOUT=0
export VISUAL='nvim'
export EDITOR=$VISUAL 

#----------------------------------------------------------------------#
# Alias
#----------------------------------------------------------------------#

alias juams='ssh -Y vsk1045@juams01.fz-juelich.de'
alias cluster='ssh -Y db957752@cluster.rz.rwth-aachen.de'
alias portal='ssh -Y dbach@portal.physik.rwth-aachen.de'

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'


alias h='history'
alias ls='ls --color'
alias v='vi'
alias gv='gvim'
alias j="jobs -l"
alias psl='ps -aux | less'
alias ..='cd ..'
alias 'cd..'='cd ..'

# Git 
alias gia="git add"
alias gcm="git commit -a -m"
alias gp="git push"
alias gs="git status"

# History Search

autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "${key[Up]}"   ]] && bindkey "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-beginning-search

# Python Virtualenvs
export WORKON_HOME=~/Envs
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
source /usr/local/bin/virtualenvwrapper.sh
