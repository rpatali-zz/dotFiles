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
set so=5                                         " Keep cursor away from the edge of the screen

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

Bundle 'altercation/vim-colors-solarized'

Bundle 'itchyny/lightline.vim'

" Although YCM does everything that supertab does, YCM does not provide
" autocomplete in plain text, markdown, etc. YCM will require vim to be
" compiled from sources.
Bundle 'ervandew/supertab'
Bundle 'Valloric/YouCompleteMe'
Bundle 'jiangmiao/auto-pairs'

" compiling vim with lua support was pain, some pointers here:
" lua-5.1 and supporting packages were installed using dpkg
" https://github.com/Shougo/neocomplete.vim/issues/31
" https://github.com/vim-jp/issues/issues/348#issuecomment-21705259
"
"./configure --with-features=huge \
"            --enable-rubyinterp \
"            --enable-pythoninterp \
"            --with-python-config-dir=/usr/lib/python2.7/config \
"            --enable-perlinterp \
"            --enable-gui=gtk2 \
"            --enable-cscope \
"            --prefix=/usr \  
"            --enable-perlinterp=yes \
"            --enable-luainterp=yes --with-lua-prefix=/usr/local --with-luajit \
"            --enable-fail-if-missing \
"
"make VIMRUNTIMEDIR=/usr/share/vim/vim74
"
"sudo checkinstall
"
"sudo update-alternatives --install /usr/bin/editor editor /usr/bin/vim 1
"sudo update-alternatives --set editor /usr/bin/vim
"sudo update-alternatives --install /usr/bin/vi vi /usr/bin/vim 1
"sudo update-alternatives --set vi /usr/bin/vim

Bundle 'Shougo/neocomplete.vim'
Bundle 'Shougo/neosnippet'

Bundle 'klen/python-mode'

" powerline is aweseome but i see weird bugs, specifically, throws a
" stacktrace everytime i open vim, making it useless.
" Bundle 'Lokaltog/powerline'

" ack requires Ack-grep pkg
Bundle 'mileszs/ack.vim'

Bundle 'rosenfeld/conque-term'

Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'

Bundle 'scrooloose/syntastic'

Bundle 'sjl/gundo.vim'

Bundle 'sorin-ionescu/python.vim'
Bundle 'Lokaltog/vim-easymotion'

Bundle 'terryma/vim-multiple-cursors'

Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'

" cscope requires cscope pkg
Bundle 'vim-scripts/cscope.vim'

Bundle 'vim-scripts/netrw.vim'

Bundle 'vim-scripts/bufkill.vim'

Bundle 'kien/ctrlp.vim'
" requires Ctrlp
Bundle 'tacahiroy/ctrlp-funky'

" vim-notes requires vim-misc
Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-notes'

Bundle 'xolox/vim-session'

Bundle 'mhinz/vim-startify'

Bundle 'git://repo.or.cz/vcscommand'

""""""""""""""COLOR"""""""""""""""""""""""""""""""
"colorscheme BusyBee

syntax enable                                    " required for solarized
set background=dark                              " alternatively, light
let g:solarized_termcolors=16                    " suggested 256, doesn't work though
colorscheme solarized                            " in the past, it has required me to install ghuntley/terminator-solarized to make this colorscheme work for me.

""""""""""""""NETRW"""""""""""""""""""""""""""""""
let g:netrw_liststyle = 3
let g:netrw_list_hide = ".git,.svn"

""""""""""""""Startify""""""""""""""""""""""""""""
let g:startify_bookmarks = ['~/.vimrc', 
         \ '~/code/sparkle',
         \ '~/code/sparkle_docs',
         \ '~/code/sprint_sms_gateway',
         \ '~/code/sprint_sms',
         \ '~/code/elmer',
         \ '~/code/verizon_vmp_gateway',
         \ '~/code/verizon_vmp']
let g:startify_files_number = 20
let g:startify_restore_position = 1

""""""""""""""LIGHTLINE"""""""""""""""""""""""""""
let g:lightline = {
      \ 'colorscheme': 'solarized',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ], ['ctrlpmark'] ],
      \   'right': [[ 'lineinfo', 'syntastic' ], ['percent'], [ 'fileformat', 'fileencoding', 'filetype']]
      \ },
      \ 'component_function': {
      \   'fugitive': 'MyFugitive',
      \   'filename': 'MyFilename',
      \   'fileformat': 'MyFileformat',
      \   'filetype': 'MyFiletype',
      \   'fileencoding': 'MyFileencoding',
      \   'mode': 'MyMode',
      \   'syntastic': 'SyntasticStatuslineFlag',
      \   'ctrlpmark': 'CtrlPMark',
      \ },
      \ 'separator': { 'left': '⮀', 'right': '⮂' },
      \ 'subseparator': { 'left': '⮁', 'right': '⮃' }
      \ }

function! MyModified()
  return &ft =~ 'help' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! MyReadonly()
  return &ft !~? 'help' && &readonly ? '⭤' : ''
endfunction

function! MyFilename()
  let fname = expand('%:t')
  return fname == 'ControlP' ? g:lightline.ctrlp_item :
        \ fname == '__Tagbar__' ? g:lightline.fname :
        \ fname =~ '__Gundo\|NERD_tree' ? '' :
        \ &ft == 'vimfiler' ? vimfiler#get_status_string() :
        \ &ft == 'unite' ? unite#get_status_string() :
        \ &ft == 'vimshell' ? vimshell#get_status_string() :
        \ ('' != MyReadonly() ? MyReadonly() . ' ' : '') .
        \ ('' != fname ? fname : '[No Name]') .
        \ ('' != MyModified() ? ' ' . MyModified() : '')
endfunction

function! MyFugitive()
  try
    if expand('%:t') !~? 'Tagbar\|Gundo\|NERD' && &ft !~? 'vimfiler' && exists('*fugitive#head')
      let mark = ''  " edit here for cool mark
      let _ = fugitive#head()
      return strlen(_) ? mark._ : ''
    endif
  catch
  endtry
  return ''
endfunction

function! MyFileformat()
  return winwidth('.') > 70 ? &fileformat : ''
endfunction

function! MyFiletype()
  return winwidth('.') > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! MyFileencoding()
  return winwidth('.') > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction

function! MyMode()
  let fname = expand('%:t')
  return fname == '__Tagbar__' ? 'Tagbar' :
        \ fname == 'ControlP' ? 'CtrlP' :
        \ fname == '__Gundo__' ? 'Gundo' :
        \ fname == '__Gundo_Preview__' ? 'Gundo Preview' :
        \ fname =~ 'NERD_tree' ? 'NERDTree' :
        \ &ft == 'unite' ? 'Unite' :
        \ &ft == 'vimfiler' ? 'VimFiler' :
        \ &ft == 'vimshell' ? 'VimShell' :
        \ winwidth('.') > 60 ? lightline#mode() : ''
endfunction

function! CtrlPMark()
  if expand('%:t') =~ 'ControlP'
    call lightline#link('iR'[g:lightline.ctrlp_regex])
    return g:lightline.ctrlp_prev . ' ' . g:lightline.subseparator.left . ' ' .
        \ g:lightline.ctrlp_item . ' ' . g:lightline.subseparator.left . ' ' .
        \ g:lightline.ctrlp_next . ' ' . g:lightline.subseparator.left . ' ' .
        \ g:lightline.ctrlp_marked
  else
    return ''
  endif
endfunction

let g:ctrlp_status_func = {
  \ 'main': 'CtrlPStatusFunc_1',
  \ 'prog': 'CtrlPStatusFunc_2',
  \ }

function! CtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
  let g:lightline.ctrlp_regex = a:regex
  let g:lightline.ctrlp_prev = a:prev
  let g:lightline.ctrlp_item = a:item
  let g:lightline.ctrlp_next = a:next
  let g:lightline.ctrlp_marked = a:marked
  return lightline#statusline(0)
endfunction

function! CtrlPStatusFunc_2(str)
  return lightline#statusline(0)
endfunction

let g:tagbar_status_func = 'TagbarStatusFunc'

function! TagbarStatusFunc(current, sort, fname, ...) abort
    let g:lightline.fname = a:fname
  return lightline#statusline(0)
endfunction

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

""""""""""""""GUNDO"""""""""""""""""""""""""""""""
let g:gundo_width = 60
let g:gundo_preview_height = 30
let g:gundo_right = 1
let g:gundo_close_on_revert = 1

""""""""""""""MAPPINGS""""""""""""""""""""""""""""
" Make the current buffer at least 100 cols wide and distribute
" equally across others. Press esc then tab switch to cycle through.
" Adjust `100` based on your taste.
:set winwidth=80

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
""""""""""""""NERDTree""""""""""""""""""""""""""""
" let g:NERDTreeDirArrows=0                      " nerd tree will break because of missing arrow keys with out this

"autocmd vimenter * if !argc()|NERDTree|endif    " Opens nerdtree if no file is specified for vim

let NERDTreeIgnore=['\.pyc$', '\~$']             " ignore files

" close vim if only window open is nerdtree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" let NERDTreeShowHidden=1                       " show hidden files

" These bindings should be declared after defining what is leader.
" NERDTree Ctrl-n for nerdtree
nnoremap <silent> <leader>n :NERDTreeToggle<CR>

" change nerdtree directory to directory containing current file Ctr-d goto dir
nnoremap <silent> <C-d> :NERDTree %:h<CR>

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

""""""""""""""GUNDO mapping"""""""""""""""""""""""
nnoremap <leader>g :GundoToggle<CR>

"""""""""""""""""""""""""""""Ctrlp""""""""""""""""
" Searches for nearest ancestor with projext.xml .git .hg .svn .bzr _darcs
let g:ctrlp_working_path_mode = 'r'
let g:ctrlp_root_markers = ['project.xml']

" have match window at bottom and display results top to bottom
let g:ctrlp_match_window = 'bottom,order:ttb,min:1,max:40'

" ctrlp enabled extensions
let g:ctrlp_extensions = ['buffertag',
                         \'funky',
                         \'quickfix',
                         \'tag']

" open window in current buffer, override conqueterm (or any other plugin
" window).
let g:ctrlp_reuse_window = '.*'

" feed word under cursor to ctrlp
nmap <C-p>w :CtrlP<CR><C-\>w

" map to open buffer mode
nnoremap <C-p>b :CtrlPBuffer<CR>

" map to open mru mode
nnoremap <C-p>m :CtrlPMRUFiles<CR>

" Ctrlp command mode
nnoremap <C-p>c :CtrlP 

" map CtrlPFunky
nnoremap <C-p>t :CtrlPFunky<CR>
" narrow the search with word under cursor
nnoremap <C-p>T :execute 'CtrlPFunky ' . expand('<cword>')<Cr>

"""""""""""""""""""""""""""""NeoComplete""""""""""
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
         \ 'default' : '',
         \ 'vimshell' : $HOME.'/.vimshell_hist',
         \ 'scheme' : $HOME.'/.gosh_completions'
         \ }

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplete#smart_close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplete#close_popup()
inoremap <expr><C-e>  neocomplete#cancel_popup()

" Close popup by <Space>.
inoremap <expr><Space> pumvisible() ? neocomplete#close_popup() : "\<Space>"

" For cursor moving in insert mode(Not recommended)
"inoremap <expr><Left>  neocomplete#close_popup() . "\<Left>"
"inoremap <expr><Right> neocomplete#close_popup() . "\<Right>"
"inoremap <expr><Up>    neocomplete#close_popup() . "\<Up>"
"inoremap <expr><Down>  neocomplete#close_popup() . "\<Down>"
" Or set this.
let g:neocomplete#enable_cursor_hold_i = 1
" Or set this.
"let g:neocomplete#enable_insert_char_pre = 1

" AutoComplPop like behavior.
"let g:neocomplete#enable_auto_select = 1

" Shell like behavior(not recommended).
set completeopt+=longest
let g:neocomplete#enable_auto_select = 1
let g:neocomplete#disable_auto_complete = 1
inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
"let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
"let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
"let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'

"""""""""""""""""""""""""""""NeoSnippet"""""""""""
" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: "\<TAB>"

" For snippet_complete marker.
if has('conceal')
  set conceallevel=2 concealcursor=i
endif

" Enable snipMate compatibility feature.
"let g:neosnippet#enable_snipmate_compatibility = 1

" Tell Neosnippet about the other snippets
"let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'

""""""""""""""CONQUE-TERM"""""""""""""""""""""""""
" Toggle ConqueTerm window.
" Eric Siegel is the original author of this function: https://github.com/esiegel/dotvim

function! s:ToggleConqueTerm(flag)
   " There is a bug in conque_term#get_instance() when there isn't an instance
   " so we will use the global list of terminals instead.
   if !exists("g:ConqueTerm_Terminals") || len(g:ConqueTerm_Terminals) == 0
      "call conque_term#open("bash", ['vsplit'])
      " Doesn't work very well with oh-my-zsh.
      ConqueTermVSplit bash
      return
   endif

   " Current buffer information
   let current_buffer_nr = bufnr("")

   " conque term information
   let term_info   = conque_term#get_instance()
   let buffer_name = term_info['buffer_name']
   let buffer_nr   = bufnr(buffer_name)
   let buffer_win  = bufwinnr(buffer_nr)

   if buffer_win == -1
      " open window
      if a:flag == 1
         execute 'vs ' . buffer_name
      else
         execute 'sp ' . buffer_name
      endif
   else
      " close conque window
      if current_buffer_nr != buffer_nr
         execute buffer_win . "wincmd w"
         wincmd c
         execute bufwinnr(current_buffer_nr) . "wincmd w"
      else
         wincmd c
      endif
   endif
endfunction

let g:ConqueTerm_ReadUnfocused = 1

nnoremap <Leader>v :call <SID>ToggleConqueTerm(1)<CR>
nnoremap <Leader>s :call <SID>ToggleConqueTerm(-1)<CR>

""""""""""""""Open file in current buffer in a split screen and scroll bind on
noremap <silent> <Leader>vs ggzR:<C-u>let @z=&so<CR>:set so=0 noscb<CR>:set columns=160<CR>:bo vs<CR>zRLjzt:setl scb<CR><C-w>p:setl scb<CR>:let &so=@z<CR>
