if has('gui_running') && !has('unix')
  set encoding=utf-8
endif

set fileencodings=iso-2022-jp,utf-8,cp932,euc-jp,default,latin

" 行番号.
set number

" 起動時のメッセージを表示しない.
set shortmess+=I

" カーソル位置の保存.
if has("autocmd")
  augroup redhat
    " In text files, always limit the width of text to 78 characters
    autocmd BufRead *.txt set tw=78
    " When editing a file, always jump to the last cursor position
    autocmd BufReadPost *
    \ if line("'\"") > 0 && line ("'\"") <= line("$") |
    \   exe "normal! g'\"" |
    \ endif
  augroup END
endif

" カーソル行の強調表示.
set cursorline

" バックアップ不要.
set nobackup

" IMEがonになるのをやめる
set iminsert=0
set imsearch=-1

" フォント設定
set guifont=Ricty_Diminished_Discord:h12:cSHIFTJIS:qDRAFT

" シンタックスハイライト
syntax on

" 折り返さない
set nowrap

