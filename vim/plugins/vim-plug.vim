" >>> vim plug settings
call plug#begin('~/.local/share/nvim/plugged')

  Plug 'itchyny/lightline.vim'  " Status Line
  Plug 'mengelbrecht/lightline-bufferline'  " Top buffer
  Plug 'tpope/vim-surround'
  Plug 'Yggdroot/indentLine'
  Plug 'jiangmiao/auto-pairs'
  Plug 'majutsushi/tagbar'  " show function declear info

  Plug 'morhetz/gruvbox'  " Colorscheme

  " Use release branch
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  " Or build from source code
  " Install yarn from https://yarnpkg.com
  Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}


call plug#end()

source ~/.config/nvim/plugins/plugged/lightline.vim
source ~/.config/nvim/plugins/plugged/coc.vim
source ~/.config/nvim/plugins/themes.vim
