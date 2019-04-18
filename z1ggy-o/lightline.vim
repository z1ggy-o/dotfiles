" >>>>> lightline and lightline-bufferline part <<<<<<<

" Colorscheme of lightline
let g:lightline = {
    \ 'colorscheme': 'wombat',
    \ 'tabline': {
    \   'left': [['buffers']],
    \   'right': [['close']],
    \ },
    \ 'component_expand':{
    \   'buffers': 'lightline#bufferline#buffers'
    \},
    \ 'component_type': {
    \    'buffers': 'tabsel'
    \}
\ }
