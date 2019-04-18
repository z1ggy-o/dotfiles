if &compatible
      set nocompatible
endif

"Add the dein installation directory into runtimepath
set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.cache/dein')
  call dein#begin('~/.cache/dein')

  call dein#add('~/.cache/dein/repos/github.com/Shougo/dein.vim')
  call dein#add('Shougo/deoplete.nvim')

  if !has('nvim')
    call dein#add('roxma/nvim-yarp')
    call dein#add('roxma/vim-hug-neovim-rpc')
  endif
    
  call dein#add ('morhetz/gruvbox')  " Colorscheme
  colorscheme gruvbox

  let g:python3_host_prog = '/Users/zgy/.pyenv/versions/neovim3/bin/python'
  let g:deoplete#enable_at_startup=1  " enable complement

  call dein#end()
  call dein#save_state()
endif

set termguicolors

" Use TAB to insert the auto complete
inoremap <silent><expr><TAB>
    \ pumvisible()? "\<C-n>" :
    \ <SID>check_back_space() ? "\<TAB>" :
    \ deoplete#manual_complete()


" For true color
if !has('gui_running')
  set t_Co=256
endif

" >>>> Vim Build in Settings
set encoding=utf-8

filetype plugin indent on
syntax enable                  " syntax highlight

auto FileType make setlocal noexpandtab " do not use soft tab in MAKEFILE
set nu                     " show row number
set colorcolumn=80 " add line prompt
highlight ColorColumn ctermbg=0
set textwidth=79

"set autoindent
set expandtab              " Use space to replace table
set tabstop=4              " Table key's width
set softtabstop=4          " Table's visual width, not the real one in file
set shiftwidth=4           " Indent width when shift line
set shiftround             " Indents to next multiple of 'shiftwidth'

set backspace =indent,eol,start  " Make backspace work as you expect (?)
set hidden                 " Switch between buffers without having to save first
set mouse=a " allow mouse in vim
set laststatus=2  " Bottom  status bar
set showtabline=2 " Top buffer line
set relativenumber " relative line number

set incsearch              " Highlighting while searching with / or ?
set hlsearch               " Highlight matching objects

set noshowmode             " lightline has provided this function

set ttyfast                " Faster redrawing
set lazyredraw             " Only redraw when necessary

set splitbelow             " Open new windows below the current window
set splitright             " Open new windows right of  the current window

set cursorline             " Find the current line quickly.

" Put all temporary files under the same directory.
" set backup
" set backupdir     =$HOME/.vim/files/backup/
" set backupext     =-vimbackup
" set backupskip    =
" set directory     =$HOME/.vim/files/swap//
" set updatecount   =100
" set undofile
" set undodir       =$HOME/.vim/files/undo/
"set viminfo       ='100,n$HOME/.vim/files/info/viminfo

