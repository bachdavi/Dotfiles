# ZSH RC File
# If not running interactively, don't do anything
[[ $- != *i* ]] && return
#----------------------------------------------------------------------#
# Autocompletion and prompt
#----------------------------------------------------------------------#

autoload -Uz compinit promptinit
compinit
promptinit

prompt grml

#Browser

#$BROWSER='google-chrome-stable'

#----------------------------------------------------------------------#
# Colors
#----------------------------------------------------------------------#


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

# Themes
alias col_dark="sh ~/.config/termcolours/dark.sh"
alias col_light="sh ~/.config/termcolours/light.sh"
alias col_default="sh ~/.config/termcolours/default.sh"

