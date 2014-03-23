""""""""""""""System resource loc"""""""""""""""""

let hostname = substitute(system('hostname'), '\n', '', '')
if hostname == "rohit-desktop"
   " ll ubuntu
   let home ="/usr/local/home/"
   let vimHome=home . ".vim"
   let tmpDir=home . ".tmpvim"
elseif hostname == "code-vm"
   " home vbox
   let home="/home/vagrant/"
   let vimHome=home . ".vim"
   let tmpDir=home . ".vim"
elseif hostname == "pata"
   " home mac
   let home="/Users/pata/"
   let vimHome =home. ".vim"
   let tmpDir =home. ".vim"
else
   " office mac, hostname changes automatically
   let home="/Users/rohit.patali/"
   let vimHome =home. ".vim"
   let tmpDir =home. ".vim"
endif

" Setup backup location and enable
let &backupdir=tmpDir . "/backup"
let &directory=tmpDir . "/swap"
let &undodir=tmpDir . "/undo"

""""""""""""""Global Settings"""""""""""""""""""""

set nocompatible                                 " be iMproved

set t_Co=256
set encoding=utf-8                               " Necessary to show unicode glyphs

set autoread                                     " auto reload files that are changed outside of vim
"set clipboard=unnamed                           " copy across vim in different terminals

set vb t_vb=                                     " removes annoying beeps when bad command

syntax on                                        " syntax highlighting
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

" actually use the default ctrl-[, that is more meaningful.
let mapleader=";"                                " specialized leader key

" use ';' instead of ':' for command mode
nnoremap ; :

" easier navigation between split windows
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l
" change to next buffer
"noremap <silent> <C-h> :bp<CR>
" change to previou buffer
"noremap <silent> <C-l> :bn<CR>

" D deletes till end
" C changes till end
" Y should yank till end
" By default it yanks whole line
nnoremap Y y$

" Trick if forgot to sudo
cmap w!! %!sudo tee > /dev/null %

" removes highlighting from search after space
noremap <silent> <Space> :noh<Bar>echo<CR>

" map in command mode to go back to beginning of line.
" matches normal shell mapping, like emacs.
cmap <c-a> <c-b>

nnoremap <C-c> :close<CR>

" run last created/run macro, stored in `q`
nnoremap Q @@

" Open file in current buffer in a split screen and scroll bind on
noremap <silent> <Leader>vs ggzR:<C-u>let @z=&so<CR>:set so=0 noscb<CR>:set columns=240<CR>:bo vs<CR>zRLjzt:setl scb<CR><C-w>p:setl scb<CR>:let &so=@z<CR>

""""""""""""""Gradle""""""""""""""""""""""""""""""

au BufNewFile,BufRead *.gradle set filetype=groovy

""""""""""""""Vundle"""""""""""""""""""""""""""""

" add local, non git, changes.
exec 'set rtp+='.vimHome."/local_config/after"

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

""""""""""""""Packages""""""""""""""""""""""""""""""""

Bundle 'Lokaltog/powerline'
   set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim

Bundle 'altercation/vim-colors-solarized'
   colorscheme solarized

Bundle 'Lokaltog/vim-easymotion'

Bundle 'git://repo.or.cz/vcscommand'

Bundle 'justincampbell/vim-eighties'

Bundle 'bronson/vim-trailing-whitespace'
   noremap <leader>ss :FixWhitespace<CR>

Bundle 'mattboehm/vim-unstack'
   let g:unstack_mapkey = '<leader>us'

Bundle 'jiangmiao/auto-pairs'

Bundle 'luochen1990/rainbow'
   let g:rainbow_active = 1

" ag requires the_silver_searcher pkg
Bundle 'rking/ag.vim'

Bundle 'scrooloose/nerdcommenter'

Bundle 'sorin-ionescu/python.vim'

Bundle 't9md/vim-choosewin'
   nmap  -  <Plug>(choosewin)

Bundle 'christoomey/vim-tmux-navigator'

Bundle 'terryma/vim-multiple-cursors'

Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-repeat'

Bundle 'javacomplete'
Bundle 'ZoomWin'
Bundle 'bufkill.vim'

""""""""""""""Ctrlp"""""""""""""""""""""""""""""""

Bundle 'kien/ctrlp.vim'

   " Searches for nearest ancestor with projext.xml .git .hg .svn .bzr _darcs
   let g:ctrlp_working_path_mode = 'r'
   let g:ctrlp_root_markers = ['project.xml']

   " have match window at bottom and display results top to bottom
   let g:ctrlp_match_window = 'bottom,order:ttb,min:1,max:40'

   " ctrlp enabled extensions
   let g:ctrlp_extensions = ['buffertag',
                            \'Startify',
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

   Bundle 'FelikZ/ctrlp-py-matcher'
   " PyMatcher for CtrlP
   if !has('python')
      echo 'In order to use pymatcher plugin, you need +python
      compiled vim'
   else
      let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch'  }
   endif

   " To improve |CtrlP| experience it is strongly recommended to install |AG|
   " https://github.com/ggreer/the_silver_searcher
   " and then use following |CtrlP| settings in your .vimrc:

   " Set delay to prevent extra search
   let g:ctrlp_lazy_update = 350

   " Do not clear filenames cache, to improve CtrlP startup
   " You can manualy clear it by <F5>
   let g:ctrlp_clear_cache_on_exit = 0

   " Set no file limit, we are building a big project
   let g:ctrlp_max_files = 0

   " If ag is available use it as filename list generator instead of 'find'
   if executable("ag")
       set grepprg=ag\ --nogroup\ --nocolor
       let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --ignore ''.git'' --ignore ''.svn'' --ignore ''.DS_Store'' --ignore ''node_modules'' --hidden -g ""'
   endif

" requires Ctrlp
Bundle 'tacahiroy/ctrlp-funky'

   " map CtrlPFunky
   nnoremap <C-p>t :CtrlPFunky<CR>
   " narrow the search with word under cursor
   nnoremap <C-p>T :execute 'CtrlPFunky ' . expand('<cword>')<Cr>

Bundle 'ivalkeen/vim-ctrlp-tjump'
   nnoremap <C-p>e :CtrlPtjump<CR>

""""""""""""""Supertab""""""""""""""""""""""""""""

Bundle 'ervandew/supertab'

   let g:SuperTabDefaultCompletionType = "context"
   let g:SuperTabClosePreviewOnPopupClose = 1       " close the scratch window on code completion popup close

   set completeopt=longest,menu,preview             " with completions not autofinishing first match

""""""""""""""Vim EasyTags""""""""""""""""""""""""

" Requires xolox/vim-misc
Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-easytags'

   " generate java-ctags with command:
   " ctags-exuberant --fields=+l --recurse=yes --sort=yes --java-kinds=+p -f .tags --languages=java

      " disable highlighting, its slow
      let g:easytags_auto_highlight = 0
      let g:easytags_events = ['BufWritePost']

      " Currently disabled, use tags by filetype for better perf
      "let g:easytags_file = tmpDir . "/easytags/tags"
      let g:easytags_by_filetype = tmpDir . "/easytags"
      " first look for local tags then global
      let g:easytags_dynamic_files = 2
      :set tags=./.tags;,~/.tmpvim/easytags

      let g:easytags_include_members = 1

""""""""""""""NeoComplete"""""""""""""""""""""""""

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

   let g:neocomplete#enable_cursor_hold_i = 1
   " Disable AutoComplPop.
   let g:acp_enableAtStartup = 0
   " Use neocomplete.
   let g:neocomplete#enable_at_startup = 1
   let g:neocomplete#enable_prefetch = 1
   let g:neocomplete#data_directory = tmpDir . "/neocomplete"
   " Use smartcase.
   let g:neocomplete#enable_smart_case = 1
   " Set minimum syntax keyword length.
   let g:neocomplete#sources#syntax#min_keyword_length = 3
   let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

   " Define dictionary.
   let g:neocomplete#sources#dictionary#dictionaries = {
            \ 'default' : ''
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

   " Enable omni completion.
   autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
   autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
   autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
   autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
   autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
   autocmd FileType java setlocal omnifunc=javacomplete#Complete

   " Enable heavy omni completion.
   if !exists('g:neocomplete#sources#omni#input_patterns')
     let g:neocomplete#sources#omni#input_patterns = {}
   endif

""""""""""""""NeoSnippet""""""""""""""""""""""""""

Bundle 'Shougo/neosnippet'

  " Plugin key-mappings.
  imap <C-k>     <Plug>(neosnippet_expand_or_jump)
  smap <C-k>     <Plug>(neosnippet_expand_or_jump)
  xmap <C-k>     <Plug>(neosnippet_expand_target)

  " SuperTab like snippets behavior.
  imap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : pumvisible() ? "\<C-n>" : "\<TAB>"
  smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

   " For snippet_complete marker.
   if has('conceal')
     set conceallevel=2 concealcursor=i
   endif

""""""""""""""Syntastic"""""""""""""""""""""""""""

Bundle 'scrooloose/syntastic'

   let g:syntastic_mode_map = { 'mode': 'active',
                              \ 'active_filetypes': ['python', 'javascript'],
                              \ 'passive_filetypes': ['java'] }

   " E221 - multiple spaces before operator.  Nice to lineup =.
   " E241 - multiple spaces after :.  Nice to lineup dicts.
   " E272 - multiple spaces before keyword.  Nice to lineup import.
   " W404 - import *, unable to detected undefined names.
   " W801 - redefinition of unused import, try/except import fails.
   let g:syntastic_error_symbol='✗'
   let g:syntastic_warning_symbol='⚠'
   let g:syntastic_python_flake8_args = "--ignore=E221,E241,E272,W404,W801"

""""""""""""""NERDTree""""""""""""""""""""""""""""

Bundle 'scrooloose/nerdtree'

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

""""""""""""""Startify""""""""""""""""""""""""""""

Bundle 'mhinz/vim-startify'

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

""""""""""""""Python Jedi"""""""""""""""""""""""""

Bundle 'davidhalter/jedi-vim'

   let g:jedi#use_tabs_not_buffers = 0
   let g:jedi#use_splits_not_buffers = "bottom"
   let g:jedi#goto_assignments_command = "<leader>ja"
   let g:jedi#goto_definitions_command = "<leader>jd"
   let g:jedi#documentation_command = "K"
   let g:jedi#usages_command = "<leader>ju"
   let g:jedi#completions_command = "<C-Space>"
   let g:jedi#rename_command = "<leader>jr"
   let g:jedi#show_call_signatures = "1"

""""""""""""""Yankring""""""""""""""""""""""""""""

Bundle 'YankRing.vim'

   let g:yankring_replace_n_pkey = "yp"
   let g:yankring_replace_n_nkey = "yn"

""""""""""""""Markdownify"""""""""""""""""""""""""

" LL stuff, don't bother copying this part,
" you are missing markdown2html script.
function! Markdownify()
" markdown current file to html
    let l:urlSpacesRemoved = substitute(expand("%:p"), " ", "\\\\ ", "g")
    execute '!markdown2html -i ' . l:urlSpacesRemoved . ' -g'
endfunction
