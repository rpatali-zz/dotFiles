" Setup backup location and enable
let &backupdir="/home/rohit/.vim/backup"
let &directory="/home/rohit/.vim/swap"
let &undodir="/home/rohit/.vim/undo"

""""""""""""""Global Settings"""""""""""""""""""""

set nocompatible                                 " be iMproved

set t_Co=256
set encoding=utf-8                               " Necessary to show unicode glyphs

set autoread                                     " auto reload files that are changed outside of vim
set clipboard+=unnamed                           " copy across vim in different terminals

set vb t_vb=                                     " removes annoying beeps when bad command

syntax enable                                    " syntax highlighting
filetype off                                     " required!
filetype plugin indent on                        " required!

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

set softtabstop=3                                " interpret tab as an indent command instead of an insert-a-tab command
set shiftwidth=3                                 " set the mod-N indentation used when you hit the tab key
set tabstop=3 ts=3                               " cause the TAB file-character to be displayed as mod-N (emacs = tab-width)
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

set noshowmode                                   " required by fancy status lines
set laststatus=2                                 " required by fancy status lines

""""""""""""""Mappings""""""""""""""""""""""""""""

let mapleader=";"                                " specialized leader key

inoremap <leader><leader> <ESC>

" easier navigation between split windows
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" D deletes till end
" C changes till end
" Y should yank till end
" By default it yanks whole line
nnoremap Y y$

" Automatically jump to end of text you pasted:
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]`

" removes highlighting from search after space
noremap <silent> <Space> :noh<Bar>echo<CR>

nnoremap <C-c> :close<CR>

""""""""""""""Plugins""""""""""""""""""""""""""""

" fzf - fuzzy finder for your shell
set rtp+=/home/rohit/.fzf
nnoremap <C-t> :FZF<CR>

" set the runtime path to include Vundle and initialize
set rtp+=/home/rohit/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'bronson/vim-trailing-whitespace'
   noremap <leader>ss :FixWhitespace<CR>
Plugin 'altercation/vim-colors-solarized'
   colorscheme solarized
   set background = "light"
Plugin 'ervandew/supertab'
   let g:SuperTabDefaultCompletionType = "context"
   let g:SuperTabClosePreviewOnPopupClose = 1
Plugin 'idanarye/vim-merginal'                   " requires vim-fugitive
Plugin 'jeetsukumaran/vim-buffergator'
Plugin 'kien/rainbow_parentheses.vim'
   " Always on
   au VimEnter * RainbowParenthesesToggle
   au Syntax * RainbowParenthesesLoadRound
   au Syntax * RainbowParenthesesLoadSquare
   au Syntax * RainbowParenthesesLoadBraces
Plugin 'Lokaltog/vim-easymotion'
Plugin 'mhinz/vim-startify'
   let g:startify_files_number = 20
   let g:startify_restore_position = 1
Plugin 'rking/ag.vim'
Plugin 'scrooloose/nerdtree'
   let NERDTreeIgnore=['\.pyc$', '\~$']
   nnoremap <silent> <leader>nt :NERDTreeToggle<CR>
   nnoremap <silent> <leader>nd :NERDTree %:h<CR>
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/syntastic'
   let g:syntastic_mode_map = { 'mode': 'active', 'active_filetypes': ['python'], 'passive_filetypes': ['java'] }
   let g:syntastic_error_symbol='✗'
   let g:syntastic_warning_symbol='⚠'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-rsi'
Plugin 'tpope/vim-surround'
Plugin 'YankRing.vim'
   let g:yankring_replace_n_pkey = "yp"
   let g:yankring_replace_n_nkey = "yn"

" All of your Plugins must be added before the following line
call vundle#end()            " required

