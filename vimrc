" Setup backup location and enable
let &backupdir="/Users/rohit.patali/.vim/backup"
let &directory="/Users/rohit.patali/.vim/swap"
let &undodir="/Users/rohit.patali/.vim/undo"

""""""""""""""Global Settings"""""""""""""""""""""

set nocompatible                                 " be iMproved

set t_Co=256
set encoding=utf-8                               " Necessary to show unicode glyphs

set autoread                                     " auto reload files that are changed outside of vim
set clipboard=unnamed                            " copy across vim in different terminals

set vb t_vb=                                     " removes annoying beeps when bad command

filetype off                                     " required!
filetype plugin indent on                        " required!
syntax enable                                    " syntax highlighting

set mouse=a                                      " allow mouse control
set cino+=(0                                     " When in unclosed parens, ie args, have them line up

set backup
set undolevels=1000
set undoreload=1000
set history=1000

set autoindent                                   " matches previous indent level,
set smartindent                                  " intelligently guesses indent (code level)
set ignorecase                                   " ignorecase when searching
set smartcase                                    " case only important if use caps
set splitright                                   " split window and open on new one on right

set softtabstop=4                                " interpret tab as an indent command instead of an insert-a-tab command
set shiftwidth=4                                 " set the mod-N indentation used when you hit the tab key
set tabstop=4 ts=4                               " cause the TAB file-character to be displayed as mod-N (emacs = tab-width)
set expandtab                                    " cause TAB characters to not be used in the file (indent-tabs-mode)
set so=5                                         " Keep cursor away from the edge of the screen
set backspace=indent,eol,start                   " allowing backspace to work after indent -> see :help i_backspacing

set nowrap
set ruler                                        " shows status of cursor position
set hidden                                       " hides buffers instead of closing them

set hls                                          " sets search highlighting
set incsearch                                    " also...incremental search!
hi Search ctermfg=white                          " colors for &hlsearch
hi Search ctermbg=lightblue

set wildmenu                                     " wildmode enables better file viewing when opeing new files, like bash
set wildmode=longest,list:longest
set wildignore+=*.swp,*.pyc,*.class,*.idea*

set completeopt=menu                             " insert mode completion, only show menu

set laststatus=2                                 " Always display the statusline in all windows
set noshowmode                                   " Hide the default mode text (e.g. -- INSERT -- below the statusline)

""""""""""""""Mappings""""""""""""""""""""""""""""

let mapleader=";"                                " specialized leader key

inoremap <leader><leader> <ESC>

" easier navigation between split windows
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" removes highlighting from search after space
noremap <silent> <Space> :noh<Bar>echo<CR>

nnoremap <C-c> :close<CR>
nnoremap <leader>bd :bd<CR>

" insert current system timestamp
nnoremap <F5> "=strftime('%c')"<CR>P
inoremap <F5> <C-R>=strftime('%c')<CR>

""""""""""""""Plugins""""""""""""""""""""""""""""

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'ahayman/vim-nodejs-complete'
Plugin 'altercation/vim-colors-solarized'
   " for whatever reason, setting to solarized doesn't work but this does.
   " https://superuser.com/questions/311370/solarized-colors-in-vim-dont-seem-to-be-working-for-me
   colorscheme default
Plugin 'artur-shaik/vim-javacomplete2'
Plugin 'bling/vim-airline'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'ervandew/supertab'
   let g:SuperTabDefaultCompletionType = "context"
   let g:SuperTabClosePreviewOnPopupClose = 1
Plugin 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
  let g:go_highlight_array_whitespace_error = 0
  let g:go_highlight_chan_whitespace_error = 0
  let g:go_highlight_extra_types = 0
  let g:go_highlight_space_tab_error = 0
  let g:go_highlight_trailing_whitespace_error = 0
  let g:go_highlight_operators = 0
  let g:go_highlight_functions = 0
  let g:go_highlight_function_arguments = 0
  let g:go_highlight_function_calls = 0
  let g:go_highlight_types = 0
  let g:go_highlight_fields = 0
  let g:go_highlight_build_constraints = 0
  let g:go_highlight_generate_tags = 0
  let g:go_highlight_string_spellcheck = 1
  let g:go_highlight_format_strings = 1
  let g:go_highlight_variable_declarations = 0
  let g:go_highlight_variable_assignments = 0
Plugin 'jelera/vim-javascript-syntax'
Plugin 'junegunn/fzf.vim'
   set rtp+=/usr/local/opt/fzf
   nmap <c-t> :FZF<CR>
   nmap <c-g> :GFiles?<CR>
   nmap <c-h><c-g> :Commits<CR>
   nmap <c-h><c-f> :BCommits<CR>
   nmap <Leader>b :Buffers<CR>
   nnoremap <silent> <Leader>m :call fzf#run({
     \    'source'  : v:oldfiles,
     \    'sink'    : 'e',
     \    'options' : '-m -x +s',
     \    'down'    : '40%'}
     \  )<CR>
   " Augmenting Ag command using fzf#vim#with_preview function
   "   * fzf#vim#with_preview([[options], preview window, [toggle keys...]])
   "   * For syntax-highlighting, Ruby and any of the following tools are required:
   "       - Highlight: http://www.andre-simon.de/doku/highlight/en/highlight.php
   "       - CodeRay: http://coderay.rubychan.de/
   "       - Rouge: https://github.com/jneen/rouge
   "
   "   :Ag  - Start fzf with hidden preview window that can be enabled with
   "   "?" key
   "   :Ag! - Start fzf in fullscreen and display the preview window above
   command! -bang -nargs=* Ag
     \ call fzf#vim#ag(<q-args>,
     \                 <bang>0 ? fzf#vim#with_preview('up:60%')
     \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
     \                 <bang>0)

Plugin 'kien/rainbow_parentheses.vim'
   au VimEnter * RainbowParenthesesToggle
   au Syntax * RainbowParenthesesLoadRound
   au Syntax * RainbowParenthesesLoadSquare
   au Syntax * RainbowParenthesesLoadBraces
Plugin 'Lokaltog/vim-easymotion'
Plugin 'mhinz/vim-startify'
   let g:startify_files_number = 20
   let g:startify_restore_position = 1
Plugin 'moll/vim-node'
   autocmd User Node if &filetype == "javascript" | setlocal expandtab | endif
   autocmd User Node
     \ if &filetype == "javascript" |
     \   nmap <buffer> <C-w>f <Plug>NodeVSplitGotoFile |
     \   nmap <buffer> <C-w><C-f> <Plug>NodeVSplitGotoFile |
     \ endif
Plugin 'pangloss/vim-javascript'
    let g:javascript_plugin_jsdoc = 1
    let g:javascript_plugin_ngdoc = 1
Plugin 'scrooloose/nerdtree'
   let NERDTreeIgnore=['\.pyc$', '\~$']
   nnoremap <silent> <leader>nt :NERDTreeToggle<CR>
   nnoremap <silent> <leader>nd :NERDTree %:h<CR>
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/syntastic'
   let g:syntastic_error_symbol='✗'
   let g:syntastic_warning_symbol='⚠'
Plugin 'Shougo/neocomplete.vim'
   let g:neocomplete#enable_smart_case = 1
   autocmd FileType java setlocal omnifunc=javacomplete#Complete
   autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
   autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
Plugin 'ternjs/tern_for_vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-rsi'
Plugin 'tpope/vim-surround'
Plugin 'wookiehangover/jshint.vim'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-notes'
Plugin 'YankRing.vim'
   let g:yankring_replace_n_pkey = "yp"
   let g:yankring_replace_n_nkey = "yn"

" All of your Plugins must be added before the following line
call vundle#end()                                " required

