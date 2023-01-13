if !&compatible
  set nocompatible
endif

let s:dein_dir = expand('~/.cache/dein')
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'
if !isdirectory(s:dein_repo_dir)
  execute '!git clone https://github.com/Shougo/dein.vim ' shellescape(s:dein_repo_dir)
endif
let &runtimepath = s:dein_repo_dir . "," . &runtimepath

if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  call dein#add('sjl/badwolf')
  call dein#add('jonathanfilip/vim-lucius')
  call dein#add('w0ng/vim-hybrid')
  call dein#add('Dru89/vim-adventurous')
  call dein#add('Shougo/neocomplete.vim')
  call dein#add('nefo-mi/nyan-modoki.vim')

  call dein#end()
  call dein#save_state()
endif

if dein#check_install()
  call dein#install()
endif

" set laststatus=2

" set statusline=%{g:NyanModoki()}

" let g:nyan_modoki_select_cat_face_number = 2 let g:nayn_modoki_animation_enabled= 1

set background=dark
set backspace=indent,eol,start
set nobackup
set noswapfile
set noundofile
set number
set t_Co=256
set t_ut=

colorscheme lucius
syntax on
