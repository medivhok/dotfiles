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

"
" Javascript
"
let g:javascript_plugin_jsdoc=1


"
" javascript standard linting
" (ale)
"
let g:ale_linters = { 'javascript': ['standard'] }
let g:ale_lint_on_save=1
let g:ale_fix_on_save=1

"
" NerdTree
"
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
