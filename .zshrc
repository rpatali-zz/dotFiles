# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"
#ZSH_THEME="agnoster"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/home/code/build_tools/gradle/bin


# location-labs

export EDITOR=vim 
export CATALINA_HOME=/opt/wm/apache-tomcat-5.5.28/ 
export JDK_HOME=/usr/lib/jvm/java-6-sun

alias ant='$ANT_HOME/bin/ant'

function wm-project() {
    local name=$1

    #local script=/ext/build/build_ctrl/latest/best/src/build_ctrl/bin/wm-project.py

    if [ -n "$name" ]; then
        location=/usr/local/home/code/${name}/project.xml
        eval `/ext/build/build_ctrl/latest/best/src/build_ctrl/bin/wm-project.py -r choosy -c /usr/local/home/code/ -s -p ubuntu-10.04-64 -t 'export \${var}=\${path}' $location`
    fi
}

# complete -F _wm-project wm-project

#virtualenv
if [ `id -u` != '0' ]; then

  # Always use pip/distribute
  export VIRTUALENV_USE_DISTRIBUTE=1

  # All virtualenvs will be stored here
  export WORKON_HOME=$HOME/.virtualenvs

  source /usr/local/bin/virtualenvwrapper.sh
  export PIP_VIRTUALENV_BASE=$WORKON_HOME
  export PIP_RESPECT_VIRTUALENV=true

fi

alias ack=ack-grep

#### GRADLE
export GRADLE_HOME=/usr/local/home/code/build_tools/gradle
export GRADLE_COMMON=/usr/local/home/code/build_tools/gradle_common
export GRADLE_OPTS=-Dorg.gradle.daemon=true
export PATH=$PATH:$GRADLE_HOME/bin

#don't correct
unsetopt correct_all
