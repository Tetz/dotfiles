" @Tetsuro Takemoto
set encoding=utf-8
autocmd VimEnter * VimFiler -split -simple -winwidth=30 -no-quit
let g:neocomplcache_enable_at_startup = 1
set tabstop=2 shiftwidth=2 expandtab

" Tab ======================
noremap to :let g:vimfiler_edit_action = 'tabopen'<CR>
noremap no :let g:vimfiler_edit_action = 'open'<CR>
map <C-l> gt 
map <C-h> gT

" Anywhere SID. 
function! s:SID_PREFIX()
   return matchstr(expand('<sfile>'),'<SNR>\d\+_\zeSID_PREFIX$')
endfunction

" Set tabline.
function! s:my_tabline()  "{{{
  let s = ''
  for i in range(1, tabpagenr('$'))
    let bufnrs = tabpagebuflist(i)
    let bufnr = bufnrs[tabpagewinnr(i) - 1]  " first window, first appears
    let no = i  " display 0-origin tabpagenr.
    let mod = getbufvar(bufnr, '&modified') ? '!' : ' '
    let title = fnamemodify(bufname(bufnr), ':t')
    let title = '[' . title . ']'
    let s .= '%'.i.'T'
    let s .= '%#' . (i == tabpagenr() ? 'TabLineSel' : 'TabLine') . '#'
    let s .= no . ':' . title
    let s .= mod
    let s .= '%#TabLineFill# '
  endfor
  let s .= '%#TabLineFill#%T%=%#TabLine#'
  return s
endfunction "}}}
let &tabline = '%!'. s:SID_PREFIX() . 'my_tabline()'
set showtabline=2 " 常にタブラインを表示

" The prefix key.
nnoremap    [Tag]   <Nop>
nmap    t [Tag]
" Tab jump
for n in range(1, 9)
  execute 'nnoremap <silent> [Tag]'.n  ':<C-u>tabnext'.n.'<CR>'
endfor
map <silent> [Tag]n :tabnext<CR>

" END of Tab ======================

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

 " Tab
 NeoBundle 'alpaca-tc/alpaca_powertabline'

 " colorschemes
 NeoBundle 'altercation/vim-colors-solarized'
 NeoBundle 'baskerville/bubblegum'
 NeoBundle 'nanotech/jellybeans.vim'
 NeoBundle 'w0ng/vim-hybrid'
 NeoBundle 'vim-scripts/twilight'
 NeoBundle 'jonathanfilip/vim-lucius'
 NeoBundle 'jpo/vim-railscasts-theme'

 NeoBundle 'tpope/vim-rails'
 NeoBundle 'vim-ruby/vim-ruby'
 NeoBundle 'tpope/vim-cucumber'
 NeoBundle 'thinca/vim-quickrun'
 NeoBundle 'scrooloose/nerdcommenter'

 NeoBundle 'thinca/vim-ref'
 NeoBundle 'taka84u9/vim-ref-ri'
 NeoBundle 'ujihisa/ref-hoogle'

 NeoBundle 'vim-scripts/surround.vim'
 NeoBundle 'vim-scripts/L9'
 NeoBundle 'vim-scripts/YankRing.vim'
 NeoBundle 'vim-scripts/grep.vim'
 NeoBundle 'vim-scripts/sudo.vim'
 NeoBundle 'vim-scripts/AnsiEsc.vim'

 NeoBundle 'kana/vim-smartchr'
 NeoBundle 'kana/vim-textobj-user'
 NeoBundle 'nelstrom/vim-textobj-rubyblock'
 NeoBundle 'motemen/hatena-vim'
 NeoBundle 'kchmck/vim-coffee-script'
 NeoBundle 'carlosvillu/coffeScript-VIM-Snippets'

 NeoBundle 'mattn/emmet-vim'
 NeoBundle 'claco/jasmine.vim'
 NeoBundle 'digitaltoad/vim-jade'
 NeoBundle 'tpope/vim-haml'
 NeoBundle 'nono/vim-handlebars'
 NeoBundle 'juvenn/mustache.vim'

 NeoBundle 'nathanaelkane/vim-indent-guides'
 NeoBundle 'kana/vim-submode'
 NeoBundle 'thinca/vim-poslist'
 NeoBundle 'thinca/vim-visualstar'
 NeoBundle 'Lokaltog/vim-easymotion'
 NeoBundle 'taku-o/vim-toggle'
 NeoBundle 'majutsushi/tagbar'
 NeoBundle 'thinca/vim-qfreplace'
 NeoBundle 'mattn/webapi-vim'

 NeoBundle 'eagletmt/ghcmod-vim'
 NeoBundle 'ujihisa/neco-ghc'
 NeoBundle 'dag/vim2hs'
 NeoBundle 'pbrisbin/html-template-syntax'


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

" Copy and paste from the system clipboard
set clipboard=unnamed

