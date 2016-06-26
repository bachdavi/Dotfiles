# ZSH RC File
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

zstyle :compinstall filename '/home/david/.zshrc'

# History
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=9000
setopt APPEND_HISTORY
bindkey -e

source ./.zprezto/init.zsh

#----------------------------------------------------------------------#
# Autocompletion and prompt
#----------------------------------------------------------------------#

autoload -Uz compinit
compinit

zstyle ':completion:*' menu select



alias lsmp3='ls -1 --indicator-style=none *.mp3'
alias lsepub='ls -1 -R --indicator-style=none | grep epub'
export GREP_COLOR="1;31"
alias grep='grep --color=auto'
export LESS="-R"

#----------------------------------------------------------------------#
# PATH
#----------------------------------------------------------------------#
export PATH="$PATH:$HOME/bin"
export PYTHIA="/home/david/Bachelorarbeit/pythia8215/bachelor-dbach/"

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
alias ls='ls --color=auto'
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

#autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
#zle -N up-line-or-beginning-search
#zle -N down-line-or-beginning-search

#[[ -n "${key[Up]}"   ]] && bindkey "${key[Up]}"   up-line-or-beginning-search
#[[ -n "${key[Down]}" ]] && bindkey "${key[Down]}" down-line-or-beginning-search

