""""""""""""""System resource loc"""""""""""""""""
let hostname = substitute(system('hostname'), '\n', '', '')
if hostname == "rohitp"
   " ll ubuntu
   let home ="/usr/local/home/"
   let vimHome=home . ".vim"
   let tmpDir=home . ".tmpvim"
elseif hostname == "kryptonite"
   " home vbox
   let home="/home/patali/"
   let vimHome=home . ".vim"
   let tmpDir=home . ".tmpvim"
elseif hostname == "Macintosh-2.local"
   " home mac
   let home="/Users/patali/"
   let vimHome =home. ".vim"
   let tmpDir =home. ".tmpvim"
endif

""""""""""""""System Settings"""""""""""""""""""""
set nocompatible                                 " be iMproved
filetype off                                     " required!
filetype plugin indent on                        " required!
syntax on                                        " syntax highlighting

set t_Co=256
set encoding=utf-8                               " Necessary to show unicode glyphs
set mouse=a                                      " allow mouse control

" Setup backup location and enable
let &backupdir=tmpDir . "/backup"
let &directory=tmpDir . "/swap"
let &undodir=tmpDir . "/undo"

set backup
set undolevels=1000
set undoreload=1000

" When in unclosed parens, ie args, have them line up.
set cino+=(0                                     " help cinoptions-values

" wildmode enables better file viewing when opeing new files, like bash
set wildmenu
set wildmode=longest,list:longest
set wildignore+=*.swp,*.pyc,*.class,*.idea*

set autoindent                                   " matches previous indent level,
set smartindent                                  " intelligently guesses indent (code level)

set ignorecase                                   " ignorecase when searching
set smartcase                                    " case only important if use caps

set softtabstop=3                                " interpret tab as an indent command instead of an insert-a-tab command
set shiftwidth=3                                 " set the mod-N indentation used when you hit the tab key
set tabstop=3 ts=3                               " cause the TAB file-character to be displayed as mod-N (emacs = tab-width)
set expandtab                                    " cause TAB characters to not be used in the file (indent-tabs-mode)

set backspace=indent,eol,start                   "allowing backspace to work after indent -> see :help i_backspacing

autocmd VimEnter * set vb t_vb=                  "removes annoying beeps when bad command
set vb t_vb=

set hls                                          " sets search highlighting
set incsearch                                    " also...incremental search!
hi Search ctermfg=white                          " colors for &hlsearch
hi Search ctermbg=lightblue

set nowrap
set ruler                                        " shows status of cursor position
set hidden                                       " hides buffers instead of closing them

set autoread                                     " auto reload files that are changed outside of vim
set completeopt=menu                             " insert mode completion, only show menu

set noshowmode                                   " required by powerline
set laststatus=2                                 " required by powerline
set clipboard=unnamed                            " copy across vim in different terminals

""""""""""""""STRIP TRAILING WHITESPACE"""""""""""
function! StripWhitespace()
   let save_cursor = getpos(".")
   let old_query = getreg('/')
   :%s/\s\+$//e
   call setpos('.', save_cursor)
   call setreg('/', old_query)
endfunction

""""""""""""""PLUGINS"""""""""""""""""""""""""""""
" add local, non git, changes.
exec 'set rtp+='.vimHome."/local_config/after"

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" initialize powerline for vim only, global package not required
"set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

Bundle 'BusyBee'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'Valloric/YouCompleteMe'
Bundle 'altercation/vim-colors-solarized'
Bundle 'bling/vim-airline'

" Although YCM does everything that supertab does, YCM does not provide
" autocomplete in plain text, markdown, etc. YCM will require vim to be
" compiled from sources.
Bundle 'ervandew/supertab'

Bundle 'jiangmiao/auto-pairs'
Bundle 'klen/python-mode'

" powerline is aweseome but i see weird bugs, specifically, throws a
" stacktrace everytime i open vim, making it useless.
" Bundle 'Lokaltog/powerline'

" tagbar requires exuberant-ctags pkg
Bundle 'majutsushi/tagbar'

" ack requires Ack-grep pkg
Bundle 'mileszs/ack.vim'

Bundle 'msanders/snipmate.vim'
Bundle 'rosenfeld/conque-term'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'sjl/gundo.vim'
Bundle 'sorin-ionescu/python.vim'
Bundle 'terryma/vim-multiple-cursors'
Bundle 'tpope/vim-fugitive'

" cscope requires cscope pkg
Bundle 'vim-scripts/cscope.vim'

Bundle 'vim-scripts/netrw.vim'
Bundle 'wincent/Command-T'

" vim-notes requires vim-misc
Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-notes'

Bundle 'xolox/vim-session'

Bundle 'git://repo.or.cz/vcscommand'

""""""""""""""COLOR"""""""""""""""""""""""""""""""
"colorscheme BusyBee

syntax enable                                    " required for solarized
let g:solarized_termcolors=16                    " suggested 256, doesn't work though
set background=dark                              " alternatively, light
colorscheme solarized                            " in the past, it has required me to install ghuntley/terminator-solarized to make this colorscheme work for me.

""""""""""""""NERDTree""""""""""""""""""""""""""""
" let g:NERDTreeDirArrows=0                      " nerd tree will break because of missing arrow keys with out this

"autocmd vimenter * if !argc()|NERDTree|endif    " Opens nerdtree if no file is specified for vim

let NERDTreeIgnore=['\.pyc$', '\~$']             " ignore files

" close vim if only window open is nerdtree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" let NERDTreeShowHidden=1                       " show hidden files

" NERDTree Ctrl-n for nerdtree
nnoremap <silent> <leader>n :NERDTreeToggle<CR>

" change nerdtree directory to directory containing current file Ctr-d goto dir
nnoremap <silent> <C-d> :NERDTree %:h<CR>

""""""""""""""NETRW"""""""""""""""""""""""""""""""
let g:netrw_liststyle = 3
let g:netrw_list_hide = ".git,.svn"

""""""""""""""AIRLINE"""""""""""""""""""""""""""""
let g:airline_powerline_fonts=1
let g:airline_left_sep = '▶'
let g:airline_left_alt_sep = '▶'
let g:airline_right_sep = '◀'
let g:airline_right_alt_sep = '◀'
let g:airline_linecolumn_prefix = '¶ '
let g:airline_paste_symbol = 'ρ'
let g:airline_fugitive_prefix = '⎇ '
let g:airline_readonly_symbol = 'RO'
let g:airline_linecolumn_prefix = '¶ '
" replace the fugitive indicator with the current working directory, followed by the filename.
let g:airline_section_b = '%{getcwd()}'
let g:airline_section_c = '%t'

""""""""""""""SUPERTAB""""""""""""""""""""""""""""
let g:SuperTabDefaultCompletionType = "context"

let g:SuperTabClosePreviewOnPopupClose = 1       " close the scratch window on code completion popup close

set completeopt=longest,menu,preview             " with completions not autofinishing first match

""""""""""""""SYNTASTIC"""""""""""""""""""""""""""
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': ['python', 'javascript'],
                           \ 'passive_filetypes': ['java'] }

" E221 - multiple spaces before operator.  Nice to lineup =.
" E241 - multiple spaces after :.  Nice to lineup dicts.
" E272 - multiple spaces before keyword.  Nice to lineup import.
" W404 - import *, unable to detected undefined names.
" W801 - redefinition of unused import, try/except import fails.
let g:syntastic_python_flake8_args = "--ignore=E221,E241,E272,W404,W801"

""""""""""""""GRADLE""""""""""""""""""""""""""""""
au BufNewFile,BufRead *.gradle set filetype=groovy

""""""""""""""TAGBAR""""""""""""""""""""""""""""""
let g:tagbar_autofocus=1

""""""""""""""ECLIM"""""""""""""""""""""""""""""""
"let g:EclimProjectKeepLocalHistory=1             " keep local history

"let g:EclimLoggingDisabled=1                     " disable logging import when log is typed

"let g:EclimJavaImportPackageSeparationLevel=2    " sort imports together when first two pacakges match

"let g:EclimJavaSearchSingleResult='edit'         " java search will open the file in the same window

""if has("PingEclim") "PingEclim isn't loaded yet, so this always fails
"if 1
    "let g:EclimTaglistEnabled=0                  " disable eclim taglist
"endif

""""""""""""""Easy Motion"""""""""""""""""""""""""
"let g:EasyMotion_keys = '1234567890'

""""""""""""""GUNDO"""""""""""""""""""""""""""""""
let g:gundo_width = 60
let g:gundo_preview_height = 30
let g:gundo_right = 1
let g:gundo_close_on_revert = 1

""""""""""""""MAPPINGS""""""""""""""""""""""""""""
" Make the current buffer at least 100 cols wide and distribute
" equally across others. Press esc then tabs to cycle through.
" Adjust `100` based on your taste.
:set winwidth=80
:nmap <Tab> <c-w><c-w><c-w>=

" LL stuff, don't bother copying this part,
" you are missing markdown2html script.
function! Markdownify()
" markdown current file to html
    let l:urlSpacesRemoved = substitute(expand("%:p"), " ", "\\\\ ", "g")
    execute '!markdown2html -i ' . l:urlSpacesRemoved . ' -g'
endfunction

" actually use the default ctrl-[, that is more meaningful.
let mapleader=";"                                " specialized leader key

" use ';' instead of ':' for command mode
noremap ; :

map! <leader><leader> <Esc>
map <leader><leader> <Esc>

nnoremap <leader><CR> :TagbarToggle<CR>

" change to next buffer
noremap <silent> <C-h> :bp<CR>
" change to previou buffer
noremap <silent> <C-l> :bn<CR>

" D deletes till end
" C changes till end
" Y should yank till end
" By default it yanks whole line
nnoremap Y y$

" break line and move stuff on the right of cursor to next line
:nnoremap <NL> i<CR><ESC>

" Trick if forgot to sudo
cmap w!! %!sudo tee > /dev/null %

" removes highlighting from search after space
noremap <silent> <Space> :noh<Bar>echo<CR>

" force myself yo use hjkl
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

" map in command mode to go back to beginning of line.
" matches normal shell mapping, like emacs.
cmap <c-a> <c-b>


nnoremap <C-c> :close<CR>
""""""""""""""REMOVE TRAILING WHITE SPACES""""""""
noremap <leader>ss :call StripWhitespace()<CR>

""""""""""""""VIM NOTES"""""""""""""""""""""""""""
let g:notes_directories = [tmpDir . "/notes"]

"""""""""""""""VIM SESSION""""""""""""""""""""""""
let g:session_autosave = 'No'
let g:session_autoload = 'No'
let g:session_directory = tmpDir . "/session"

""""""""""""""PYTHON MODE"""""""""""""""""""""""""
" Disable default python folding
let g:pymode_folding = 0

" Key for set/unset breakpoint
let g:pymode_breakpoint_key = 'B'

""""""""""""""ECLIM mapping"""""""""""""""""""""""
"nnoremap <silent> <leader>h :JavaHierarchy<CR>

"nnoremap <silent> <leader>d :JavaDocPreview<CR>

"nmap <silent> <leader>m :JavaImport<CR>

"nmap <silent> <leader>o :JavaImportOrganize<CR>

"nmap <silent> <leader>c :JavaCorrect<CR>

"nmap <silent> <leader>s :JavaSearch<CR>

"nmap <silent> <leader>p :ProjectProblems<CR>

""""""""""""""GUNDO mapping"""""""""""""""""""""""
nnoremap <leader>g :GundoToggle<CR>

""""""""""""""Open file in current buffer in a split screen and scroll bind on
noremap <silent> <Leader>vs ggzR:<C-u>let @z=&so<CR>:set so=0 noscb<CR>:set columns=160<CR>:bo vs<CR>zRLjzt:setl scb<CR><C-w>p:setl scb<CR>:let &so=@z<CR>
