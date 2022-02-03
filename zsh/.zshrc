# ZSH RC File
# If not running interactively, don't do anything
#[[ $- != *i* ]] && return

export FZF_DEFAULT_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
export PATH="$PATH:/opt/local/bin"

# History
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=900000
setopt SHARE_HISTORY

bindkey "^E" end-of-line
bindkey "^A" beginning-of-line
bindkey '^R' history-incremental-pattern-search-backward
bindkey '^N' forward-word
bindkey '^P' backward-word
source ~/.zprezto/init.zsh

#----------------------------------------------------------------------#
# Autocompletion and prompt
#----------------------------------------------------------------------#

autoload -Uz compinit
compinit

zstyle ':completion:*' menu select

alias lsmp3='ls -1 --indicator-style=none *.mp3'
alias lsepub='ls -1 -R --indicator-style=none | grep epub'
export LESS="-R"
alias c='cd'
alias l='ls'

alias vim='nvim'
alias vi='nvim'
alias tmux="TERM=screen-256color-bce tmux"
set term=screen-256color

export JAVA_HOME="/Library/Java/JavaVirtualMachines/Contents/Home/"

alias julia='JULIA_NUM_THREADS=4 /Applications/Julia-1.7.app/Contents/Resources/julia/bin/julia'

# Ruby
# export GEM_HOME=$HOME/gems
export PATH=$HOME/.gem/ruby/2.5.0/bin:$PATH

#----------------------------------------------------------------------#
# PATH
#----------------------------------------------------------------------#
export PATH="$PATH:$HOME/bin"
export TMOUT=0
export VISUAL="emacsclient -c"
export EDITOR="emacsclient -t"

#----------------------------------------------------------------------#
# Alias
#----------------------------------------------------------------------#

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

alias h='history'
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

# FZF

# fh - repeat history
fh() {
  print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed 's/ *[0-9]* *//') }
export PATH="/usr/local/opt/ruby/bin:$PATH"
export PATH="/usr/local/opt/qt/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/david/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/david/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/david/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/david/google-cloud-sdk/completion.zsh.inc'; fi
export PATH="/usr/local/opt/llvm/bin:$PATH"
