# Setup fzf function
# ------------------
unalias fzf 2> /dev/null
fzf() {
  /usr/bin/ruby --disable-gems ~/.fzf/fzf "$@"
}
export -f fzf > /dev/null

# Auto-completion
# ---------------
[[ $- =~ i ]] && source ~/.fzf/fzf-completion.zsh

# Key bindings
# ------------
# CTRL-T - Paste the selected file path(s) into the command line
__fsel() {
  set -o nonomatch
  command find * -path '*/\.*' -prune \
    -o -type f -print \
    -o -type d -print \
    -o -type l -print 2> /dev/null | fzf -m | while read item; do
    printf '%q ' "$item"
  done
  echo
}

if [[ $- =~ i ]]; then

if [ -n "$TMUX_PANE" -a ${FZF_TMUX:-1} -ne 0 -a ${LINES:-40} -gt 15 ]; then
  fzf-file-widget() {
    local height
    height=${FZF_TMUX_HEIGHT:-40%}
    if [[ $height =~ %$ ]]; then
      height="-p ${height%\%}"
    else
      height="-l $height"
    fi
    tmux split-window $height "zsh -c 'source ~/.fzf.zsh; tmux send-keys -t $TMUX_PANE \"\$(__fsel)\"'"
  }
else
  fzf-file-widget() {
    LBUFFER="${LBUFFER}$(__fsel)"
    zle redisplay
  }
fi
zle     -N   fzf-file-widget
bindkey '^T' fzf-file-widget

# ALT-C - cd into the selected directory
fzf-cd-widget() {
  cd "${$(set -o nonomatch; command find * -path '*/\.*' -prune \
          -o -type d -print 2> /dev/null | fzf):-.}"
  zle reset-prompt
}
zle     -N    fzf-cd-widget
bindkey '\ec' fzf-cd-widget

# CTRL-R - Paste the selected command from history into the command line
fzf-history-widget() {
  LBUFFER=$(fc -l 1 | fzf +s +m -n..,1,2.. | sed "s/ *[0-9*]* *//")
  zle redisplay
}
zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget

fi

# Setting ag as the default source for fzf
export FZF_DEFAULT_COMMAND='ag -l -g ""'

# Copy the original fzf function to __fzf
declare -f __fzf > /dev/null ||
   eval "$(echo "__fzf() {"; declare -f fzf | grep -v '^{' | tail -n +2)"

# Use git ls-tree when possible
fzf() {
   if [ -n "$(git rev-parse HEAD 2> /dev/null)"  ]; then
      FZF_DEFAULT_COMMAND="git ls-tree -r --name-only HEAD" __fzf "$@"
   else
      __fzf "$@"
   fi
}
