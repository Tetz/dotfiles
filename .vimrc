" @Tetsuro Takemoto
set encoding=utf-8
set mouse=a
autocmd VimEnter * VimFiler -split -simple -winwidth=30 -no-quit

" Hilight Current Line
hi CursorLine   cterm=NONE ctermbg=DarkGray ctermfg=white guibg=Green guifg=white
hi CursorColumn cterm=NONE ctermbg=DarkGray ctermfg=white guibg=Green guifg=white
nnoremap <Leader>c :set cursorline! cursorcolumn!<CR>
"set cursorline

" Change Color when entering Insert Mode
autocmd InsertEnter * highlight  CursorLine ctermbg=None ctermfg=None
" Revert Color to default when leaving Insert Mode
autocmd InsertLeave * highlight  CursorLine ctermbg=DarkGray ctermfg=None

" Line 
set nu

" Ctags
set tags=tags;/

" NeoBundle
if has('vim_starting')
   set nocompatible               " Be iMproved

   " Required:
   set runtimepath+=~/.vim/bundle/neobundle.vim/
 endif

 " Required:
 call neobundle#begin(expand('~/.vim/bundle/'))

 " Let NeoBundle manage NeoBundle
 " Required:
 NeoBundleFetch 'Shougo/neobundle.vim'

 " My Bundles here:
 NeoBundle 'Shougo/neosnippet.vim'
 NeoBundle 'Shougo/neosnippet-snippets'
 NeoBundle 'tpope/vim-fugitive'
 NeoBundle 'kien/ctrlp.vim'
 NeoBundle 'flazz/vim-colorschemes'
 NeoBundle 'derekwyatt/vim-scala'
 NeoBundle 'Shougo/neocomplcache.vim'
 NeoBundle 'Shougo/unite.vim'
 NeoBundle 'Shougo/vimfiler.vim'
 NeoBundle 'bling/vim-airline'

 " You can specify revision/branch/tag.
 NeoBundle 'Shougo/vimshell', { 'rev' : '3787e5' }

 call neobundle#end()

 " Required:
 filetype plugin indent on

 " If there are uninstalled bundles found on startup,
 " this will conveniently prompt you to install them.
 NeoBundleCheck

" vim:fdm=marker
" Editor basics {{{
" Behave like Vim instead of Vi
 set nocompatible
" Show a status line
 set laststatus=2
" Show the current cursor position
 set ruler
" Enable syntax highlighting
 syn on
" }}}
" Mouse {{{
" Send more characters for redraws
 set ttyfast
" Enable mouse use in all modes
 set mouse=a
" Set this to the name of your terminal that supports mouse codes.
" Must be one of: xterm, xterm2, netterm, dec, jsbterm, pterm
 set ttymouse=xterm2
" }}}
" Disable arrow keys {{{
 noremap <Up> <NOP>
 inoremap <Down> <NOP>
 inoremap <Left> <NOP>
 inoremap <Right> <NOP>
 noremap <Up> <NOP>
 noremap <Down> <NOP>
 noremap <Left> <NOP>
 noremap <Right> <NOP>
" }}}


