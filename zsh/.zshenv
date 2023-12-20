# .zshenv
#
# sourced on all shell invocations. generally things that
# aren't needed for an interactive shell, which belong in
# .zshrc.
#
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
#
# Path to your oh-my-zsh installation.
#export ZSH="$HOME/.oh-my-zsh"
#
# Would you like to use another custom folder than $ZSH/custom?
#export ZSH_CUSTOM="$HOME/.zshcustom"
#
# on ubuntu, do not use manpath variable! see manpath command
# and /etc/manpath.config
#typeset -U MANPATH
#export manpath=(~/.local/share/man /usr/local/man $manpath[@])
#
# PATH="$PATH"
typeset -U PATH
# export path=(~/.local/bin ~/.cargo/bin /usr/local/go/bin $path[@])
export path=(~/.local/bin $path[@])
#
#
#export FZF_DEFAULT_COMMAND='fd --type file'
export FZF_DEFAULT_COMMAND='fd --type file --follow --hidden --exclude .git --color=always'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS='--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4'
#
# Umask
#
# /etc/profile sets 022, removing write perms to group + others.
# Set a more restrictive umask: i.e. no exec perms for others:
# umask 027
# Paranoid: neither group nor others have any perms:
# umask 077
umask 033
#
# export VISUAL="nvim"
# export EDITOR=$VISUAL
# export ALTERNATE_EDITOR=""
#
# Timezone, set TZ$ to /etc/timezone if it exists, or default
# to America/New_York.
if [ ! -f /etc/timezone ]; then
    export TZ="America/New_York"
else
    export TZ=$(</etc/timezone)
fi
