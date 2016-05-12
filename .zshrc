# Emacs key bind
bindkey -e

# history
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# auto complete
autoload -U compinit; compinit

# color
autoload -Uz colors; colors

# prompt
autoload -Uz VCS_INFO_get_data_git; VCS_INFO_get_data_git

function rprompt-git-current-branch {
        local name st color gitdir action
        if [[ "$PWD" =~ '/\.git(/.*)?$' ]]; then
                return
        fi

        name=`git rev-parse --abbrev-ref=loose HEAD 2> /dev/null`
        if [[ -z $name ]]; then
                return
        fi

        gitdir=`git rev-parse --git-dir 2> /dev/null`
        action=`VCS_INFO_git_getaction "$gitdir"` && action="($action)"

	if [[ -e "$gitdir/rprompt-nostatus" ]]; then
		echo "$name$action "
		return
	fi

        st=`git status 2> /dev/null`
	if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
		color=%F{green}
	elif [[ -n `echo "$st" | grep "^nothing added"` ]]; then
		color=%F{yellow}
	elif [[ -n `echo "$st" | grep "^# Untracked"` ]]; then
                color=%B%F{red}
        else
                color=%F{red}
        fi

        echo "$color$name$action%f%b "
}

# プロンプトが表示されるたびにプロンプト文字列を評価、置換する
setopt prompt_subst

PROMPT='%n@%m%F{green}%#%F{white} '
RPROMPT='[`rprompt-git-current-branch`%~]'
