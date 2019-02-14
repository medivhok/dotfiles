colorscheme gruvbox
set background=dark
set laststatus=2
set showtabline=2
syntax on
filetype plugin indent on

"
" Haskell
"
let g:ghc="/usr/bin/ghc-6.6.1"
let b:ghc_staticoptions="-dynamic"

" configure browser for haskell_doc.vim
let g:haddock_browser = "chromium"

" use ghc functionality for haskell files
au Bufenter *.hs compiler ghc	

