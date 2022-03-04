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
export ZSH="$HOME/.oh-my-zsh"
#
# Would you like to use another custom folder than $ZSH/custom?
export ZSH_CUSTOM="$HOME/.zshcustom"
#
export MANPATH="/usr/local/man:$MANPATH"
#
# PATH="$PATH"
typeset -U PATH
export path=(~/bin ~/.local/bin ~/go/bin $path[@])
#
#export FZF_DEFAULT_COMMAND='fd --type file'
export FZF_DEFAULT_COMMAND='fd --type file --follow --hidden --exclude .git --color=always'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
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
export VISUAL="emacs"
export EDITOR=$VISUAL
