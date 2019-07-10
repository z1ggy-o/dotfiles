if &compatible
      set nocompatible
endif

set tags=./.tags;,.tags  " set ctags search path

" Set leader key to space bar
let mapleader="\<Space>"

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

"set incsearch              " Highlighting while searching with / or ?
set nohlsearch               " Highlight matching objects

set noshowmode             " lightline has provided this function

set ttyfast                " Faster redrawing
set lazyredraw             " Only redraw when necessary

set splitbelow             " Open new windows below the current window
set splitright             " Open new windows right of  the current window

set cursorline             " Find the current line quickly.
