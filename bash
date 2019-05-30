# export PATH="$PATH:/Users/Arvid/ampl201101.cplex122.macosxrtexport ILOG_LICENSE_FILE="/Users/Arvid/ampl201101.cplex122.macosx/access.ilm"
export PATH=$PATH:/usr/local/opt/go/libexec/bin
export PATH=/usr/local/share/python:$PATH
export PYTHONPATH=/usr/local/lib/python:$PYTHONPATH
export LANG='sv_SE.UTF-8'

export ANDROID_HOME=~/Library/Android/sdk/
export ANDROID_SDK_ROOT=$ANDROID_HOME


source ~/git-completion.bash

# export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "
export CLICOLOR='1'
export LSCOLORS='ExFxBxDxCxegedabagacad'
alias ls='ls -GFh'
alias s='sublime .'
alias cd..='cd ..'
alias o='open .'
alias c='clear'
alias ip='curl icanhazip.com'
alias drop='cd ~/dropbox '
alias liu='cd ~/dropbox/UNI/LiU/ '
alias bp='s ~/.bash_profile'
alias dev='cd ~/development'
alias stud='cd ~/Studier'
alias ap='cd ~/Development/Airpelago'
alias catbp='cat ~/.bash_profile'
alias w+='g++ -std=c++14 -pedantic -Wall -Wextra'
alias g++17='g++ -std=c++1z -Wall -Wextra -pedantic'
alias loggs='react-native log-ios'
alias rn='react-native'
alias ios-simulator='open ~/../../Applications/Xcode.app/Contents/Developer/Applications/Simulator.app/'
alias no-lines='cloc $(git ls-files)'
alias venv='source ./venv/bin/activate'

# welcome() {
#     # ------------------------------------------
#     # ------WELCOME MESSAGE---------------------
#     # this will display the username, date, time, a calendar, the amount of users, and the up time.
#     figlet "Welcome, " $USER;
#     echo -e ""; cal ;
#     echo -ne "Today is "; date #date +"Today is %A %D, and it is now %R"
#     echo -e ""
#     echo -ne "Up time:";uptime | awk /'up/'
#     echo -en "Local IP Address : "
#     curl icanhazip.com;
#     echo "";
# }




# ----------------------
# Git Aliases
# ----------------------
alias g='git'
alias ga='git add'
alias gaa='git add .'
alias gaaa='git add --all'
alias gau='git add --update'
alias gb='git branch'
alias gbd='git branch --delete '
alias gc='git commit'
alias gcm='git commit --message'
alias gcf='git commit --fixup'
alias gco='git checkout'
alias gcob='git checkout -b'
alias gcom='git checkout master'
alias gcos='git checkout staging'
alias gcod='git checkout develop'
alias gd='git diff'
alias gda='git diff HEAD'
alias gi='git init'
alias glg='git log --graph --oneline --decorate --all'
alias gld='git log --pretty=format:"%h %ad %s" --date=short --all'
alias gm='git merge --no-ff'
alias gma='git merge --abort'
alias gmc='git merge --continue'
alias gp='git pull'
alias gpr='git pull --rebase'
alias gr='git rebase'
alias gs='git status'
alias gss='git status --short'
alias gst='git stash'
alias gsta='git stash apply'
alias gstd='git stash drop'
alias gstl='git stash list'
alias gstp='git stash pop'
alias gps='git push'
alias gsts='git stash save'
alias cd..='cd ../'                         # Go back 1 directory level (for fast typers)
alias ..='cd ../'                           # Go back 1 directory level
alias ...='cd ../../'                       # Go back 2 directory levels
alias .3='cd ../../../'                     # Go back 3 directory levels
alias .4='cd ../../../../'                  # Go back 4 directory levels
alias .5='cd ../../../../../'               # Go back 5 directory levels
alias .6='cd ../../../../../../'            # Go back 6 directory levels
alias ll='ls -FGlAhp'                       # Preferred 'ls' implementation
alias pages='cd ~/Library/Mobile\ Documents/com~apple~Pages/Documents'
alias icloud='cd ~/Library/Mobile\ Documents/'
alias wkit='cd ~/Development/WKIT'
alias rmf='rm -rf'
cd() { builtin cd "$@"; ls; }               # Always list directory contents upon 'cd'
function gall() { git add .;                 # Not working
git commit -m $1;
git push;
}

function safari() {
    if [ -z "$1" ]; then
            open -a safari ; #https://www.google.se;
    else
        open -a safari 'https://'$1;
    fi
}

function google() {
    for term in $@ ; do
        search="$search%20$term"
    done
    safari "google.com/search?q=$search"
    unset search
}

function parse_git_branch {
    BRANCH=`git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'`
    if [ ! "${BRANCH}" == "" ]
    then
        STAT=`parse_git_dirty`
        echo "[${BRANCH}${STAT}]"
    else
        echo ""
    fi
}

function parse_git_dirty {
    status=`git status 2>&1 | tee`
    dirty=`echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?"`
    untracked=`echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?"`
    ahead=`echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?"`
    newfile=`echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?"`
    renamed=`echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?"`
    deleted=`echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?"`
    bits=''
    if [ "${renamed}" == "0" ]; then
        bits=">${bits}"
    fi
    if [ "${ahead}" == "0" ]; then
        bits="*${bits}"
    fi
    if [ "${newfile}" == "0" ]; then
        bits="${bits}"
    fi
    if [ "${untracked}" == "0" ]; then
        bits="?${bits}"
    fi
    if [ "${deleted}" == "0" ]; then
        bits="x${bits}"
    fi
    if [ "${dirty}" == "0" ]; then
        bits="${bits}"
    fi
    if [ ! "${bits}" == "" ]; then
        echo "${bits}"
    else
        echo ""
    fi
}

mcd () { mkdir -p "$1" && cd "$1"; }        # mcd:          Makes new Dir and jumps inside
trash () { command mv "$@" ~/.Trash ; }
ql () { qlmanage -p "$*" >& /dev/null; }    #Quicklook

PS1="\[\033]0;\w\007\]\[\e[33m\u\[\e[m\] \e[0;37m\]in \e[m\]"
# PS1="\[\033]0;\w\007\]\[\e[1;33m\]\u\[\e[m\] \e[0;37m\]in \e[m\]"
PS1+="\e[0;31m\]\w\e[m\]\e[1;37m\]\$([[ -n \$(git branch 2> /dev/null) ]]"
PS1+="&& echo \" on\")\[${white}\] \e[1;34m\]\$(parse_git_branch)\e[m\] \n$ "
PS2="| => "

export EDITOR=/usr/local/bin/sublime

# Ensure user-installed binaries take precedence
export PATH=/usr/local/bin:$PATH
# Load .bashrc if it exists
test -f ~/.bashrc && source ~/.bashrc

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

alias git='LANG=en_GB git'
