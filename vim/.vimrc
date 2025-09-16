set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" }}}

Bundle 'bling/vim-airline'
Bundle 'docunext/closetag.vim'
Bundle 'maxbrunsfeld/vim-yankstack'
Bundle 'mbbill/undotree'
Bundle 'rodjek/vim-puppet'
Bundle 'tmhedberg/matchit'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'vim-scripts/Colour-Sampler-Pack'
Bundle 'vim-scripts/ScrollColors'
Bundle 'prettier/vim-prettier'

" All of your Plugins must be added before the following line
" All of your Plugins must be added before the following line
call vundle#end()

filetype plugin indent on

" YankStack initial call so we can remap Y to y$ {{{
call yankstack#setup()
" }}}
"
"{{{ Terminal Settings
color desert256

" Set terminal font encoding
set encoding=utf-8
set termencoding=utf-8
"}}}

"{{{ General settings

" Turn on syntax highlighting
syntax on

" Save up to N commands executed
set history=200

" enable filetype detection
filetype on

" Set the leader for the whole file
let mapleader = ","

" Fold by marker by default '{{{' and '}}}'
set foldmethod=marker

" Cool tab completion stuff
set wildmenu

"set wildmode=list:longest,full
set wildmode=list:list:longest

set wildignore+=*.swp,*.zip,*.tar,*.gz,*.class,*.o,*/build/*,*dist/*,*.jar

" Allow backspace over newlines
set backspace=2

" Allow buffers to move to the background without
" complaining about needing to be saved
set hidden

" Define the location of our exuberant ctags file
set tags=./.git/tags;./tags;/

"}}}

"{{{ Display settings

" Show matching parens, brackets, etc
set showmatch

" Show commands as you're typing them
set showcmd

" Show the cursor position ruler at the bottom
set ruler

" Display nonprintable characters
set list

" Display tabs as dot-space instead of ^I
set listchars=tab:.\ ,

" Scroll offset of 3
set scrolloff=3

" Do not wrap text lines
set nowrap

" Turn on line numbers and add a quick way to toggle them
set nu
nnoremap <leader>nu :set nu!<CR>

" Change the background color the hightlight menu from ugly pink to nice blue
:highlight Pmenu ctermbg=DarkBlue ctermfg=white gui=bold
:highlight PmenuSel ctermbg=LightBlue ctermfg=red gui=bold

" Highlight trailing whitespace with a red background
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/ " This will not match trailing whitespace when typing on a line
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()
" }}}

"{{{ Spacing and tabbing
set autoindent
set cindent
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab
set smarttab
" }}}

" {{{ Search, replace, and paste

" Ignore case in search - except...
set ignorecase

" Only do case-sensitive search when a capital has been typed
set smartcase

" Search right when you start typing
set incsearch

" Highlight the found search
set hlsearch

" Toggle paste mode
set pastetoggle=<leader>pt
"}}}

"{{{ Mouse options

set mouse=a
set ttymouse=xterm2

" set up command mode abbreviations for mouseoff and mouseon
cabbrev mouseoff set mouse=<CR>
cabbrev mouseon set mouse=a<CR>

"}}}

"{{{ User Interface

" Status line settings
set laststatus=2 "Always have a status line
"set statusline=%-3.3n\ %f%(\ %r%)%(\ %%m%0*%)%=(%l,\ %c)\ %P\ [%{&encoding}%{&fileformat}]%(\ %w%)\ %y
set shortmess+=aI "Use nice short status notices

"hi StatusLine term=inverse cterm=NONE ctermfg=white ctermbg=black
"hi StatusLineNC term=none cterm=NONE ctermfg=darkgray ctermbg=black

"}}}

"{{{ Auto Commands

if has('autocmd')

  " When editing a file, always jump to the last cursor position
  au BufReadPost *
        \ if ! exists("g:leave_my_cursor_position_alone") |
        \     if line("'\"") > 0 && line ("'\"") <= line("$") |
        \         exe "normal g'\"" |
        \     endif |
        \ endif

  " {{{ Git settings
  " Commit messages settings
  "   Enable spell-checking
  "   Wrap commit messages at 72 chars
  "   Start in insert mode
  "   # Example of updating commit message automatically
  " autocmd BufNewFile,BufReadPost COMMIT_EDITMSG set spell tw=72 | exe "%s/TICKET-123/TICKET-456/g" | exe "normal gg" | exe "normal ZZ" | start!
  autocmd BufNewFile,BufReadPost COMMIT_EDITMSG set spell tw=72 | exe "normal gg" | start!
  " }}}
  "

  " {{{ Filetype settings
  autocmd BufNewFile,BufRead *.md set filetype=markdown
  autocmd BufNewFile,BufReadPost *Jenkinsfile* set ft=groovy
  " }}}

endif
" }}}

"{{{ Mappings
" Make Y function like D - copy the whole line
nmap Y y$

" Remap backspace to what comma used to do since that's now our leader
nnoremap \\ ,

" Space will toggle folds.  Very helpful.
nnoremap <space> za

" Search mappings: These will make it so that going to the next one in a
" search will center on the line it's found in.
map N Nzz
map n nzz
map * *zz
map # #zz

" Tab commands
map <leader>tc :tabnew<cr>
map <leader>td :tabclose<cr>
map <leader>tm :tabmove
map <leader>tn :tabnext<cr>
map <leader>tp :tabprevious<cr>

" Load/Source the vimrc quickly
nmap <leader>s :source $MYVIMRC<CR>
nmap <leader>e :e $MYVIMRC<CR>

" Quickly delete a buffer
nmap <leader>d :bd<CR>

" Quickly toggle spellcheck
nmap <leader>l :set spell!<CR>

" Mapping to remove all trailing whitespace on lines
nmap <leader>w :%s/\s\+$//<CR>:let @/=''<CR>

" Change to current file directory upon request
nmap <silent> <Leader>cd :cd %:p:h<CR>

" Fixed width settings
nmap <leader>fw :set autoindent cindent shiftwidth=2 softtabstop=2 tabstop=2 expandtab smarttab<CR>

nmap <leader>ut :UndotreeToggle<CR>

" Make horizontal scrolling faster
nnoremap zh 20zh
nnoremap zl 20zl

" This is totally awesome - remap jj to escape in insert mode.  You'll never type jj anyway, so it's great!
"inoremap <Esc> <nop>
inoremap jj <Esc>
inoremap jk <Esc>

cnoremap %% <C-R>=expand('%:h').'/'<CR>

" Sort the current paragraph
:nmap <leader>is vip:sort<CR>
" }}}

"{{{ Backup settings

set writebackup " We want to write a backup file
set backupdir=~/.vim/backup,.,~/tmp,~/ " Store the backup file outside the current directory if it exists
set directory=~/.vim/tmp,.,~/tmp,/var/tmp,/tmp " Store the swap file outside the current directory if it exists

" Set persistent undo within files
set undodir=~/.vim/undodir
set undofile
set undolevels=1000  "maximum number of changes that can be undone
set undoreload=10000 "maximum number lines to save for undo on a buffer reload

" }}}

"{{{ Testbed

" Ignore diff whitespace
set diffopt+=iwhite

" Test out omnicompletion
set ofu=syntaxcomplete#Complete

" Search the current file for the word under the cursor and display matches
nmap <silent> ,gw :vimgrep /<C-r><C-w>/ %<CR>:ccl<CR>:cwin<CR><C-W>J:nohls<CR>

" Search the current file for the WORD under the cursor and display matches
nmap <silent> ,gW :vimgrep /<C-r><C-a>/ %<CR>:ccl<CR>:cwin<CR><C-W>J:nohls<CR>
" }}}

" Plugin settings {{{

" {{{ fugitive
nnoremap <Leader>gb :Gblame<Enter>
nnoremap <Leader>gd :Gdiff<Enter>
nnoremap <Leader>gl :Glog<Enter>
nnoremap <Leader>gs :Gstatus<Enter>
" Delete fugitive buffers when we close them
autocmd BufReadPost fugitive://* set bufhidden=delete
" }}}

" {{{ Closetag
" Source our closetag function with the right document types
au FileType html,xml,xsl,xslt source ~/.vim/bundle/closetag.vim/plugin/closetag.vim
" }}}

" YankStack {{{
" If we do not manually specify these mappings, ctrlp will take them over for
" some reason even though we've told it not to
let g:yankstack_map_keys = 0
nmap <C-p> <Plug>yankstack_substitute_older_paste
nmap <C-n> <Plug>yankstack_substitute_newer_paste
" }}}

set nocompatible
set colorcolumn=120
set history=1000

" Insert filename
inoremap \fn <C-R>=expand("%:t:r")<CR>
" Insert directory name
inoremap \dn <C-R>=expand("%:h")<CR>
" Insert a pseudo-ish package name that we will clean up
inoremap \pn  <ESC>mqa<C-R>=expand("%:h")<CR><ESC>:s#/#.#g<CR>`q

" Set the window title to the path we started vim with instead of "Thanks for " flying Vim!"
let &titleold=getcwd()

map <leader>p :Prettier<cr>
"autocmd BufWritePre *.js Prettier
