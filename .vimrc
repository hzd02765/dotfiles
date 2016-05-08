set encoding=utf-8
set fileencoding=utf-8
set fileencodings=iso-2022-jp,iso-2022-jp-2,euc-jp,sjis,utf-8
set fileformats=unix,dos,mac
scriptencoding utf-8

" Note: Skip initialization for vim-tiny or vim-small.
if 0 | endif

set noerrorbells
set visualbell
set viminfo=

syntax enable
colorscheme slate
"colorscheme solarized
"colorscheme molokai
"set background=dark

"起動時のメッセージを消す(ウガンダに寄付しないフトドキモノです)
set shortmess+=I

"-----------------------------------------------------------
" NeoBundle
"-----------------------------------------------------------

if has('vim_starting')
    " Be iMproved
    set nocompatible
    set runtimepath+=$HOME/vimfiles/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('$HOME/vimfiles/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" My Bundles here:
" Refer to |:NeoBundle-examples|.
" Note: You don't set neobundle setting in .gvimrc!

NeoBundle 'fuenor/qfixhowm'
" ファイルをtree表示してくれる
NeoBundle 'scrooloose/nerdtree'
" ファイルオープンを便利に
NeoBundle 'Shougo/unite.vim'
" Unite.vimで最近使ったファイルを表示できるようにする
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'Shougo/neoyank.vim'
NeoBundle 'ujihisa/unite-colorscheme'
" コメントON/OFFを手軽に実行
NeoBundle 'tomtom/tcomment_vim'

" インデントに色を付けて見やすくする
NeoBundle 'nathanaelkane/vim-indent-guides'
" vimを立ち上げたときに、自動的にvim-indent-guidesをオンにする
let g:indent_guides_enable_on_vim_startup = 1

" 行末の半角スペースを可視化
"NeoBundle 'bronson/vim-trailing-whitespace'

NeoBundle 'itchyny/lightline.vim'
NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'tomtom/tcomment_vim'

NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/vimproc'

NeoBundle 'ctrlpvim/ctrlp.vim'

NeoBundle 'Shougo/neocomplete.vim'
let g:neocomplete#enable_at_startup = 1

call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck

if !has('vim_starting')
    " .vimrcを読み込み直した時のための設定
    call neobundle#call_hook('on_source')
endif

" 行番号を表示
set number
" 構文ハイライト
syntax on
" 入力されているテキストの最大幅。行がそれより長くなると、この幅を超えないように空白の後で改行される。
" 値を 0 に設定すると無効になる。
set textwidth=0
" 長い行を折り返さない
set nowrap
" 保存時に行末の空白を除去する
autocmd BufWritePre * :%s/\s\+$//ge

" 常にステータスラインを表示
set laststatus=2
set t_Co=256

" 開いているファイルのディレクトリに移動する
"set autochdir

" スワップファイルの出力先を変更する
set directory=~/vimfiles/tmp
" バックアップファイルの出力先を変更する
set backupdir=~/vimfiles/tmp
" viminfoファイルの出力先を変更する
set viminfo+=n~/vimfiles/tmp/viminfo.txt

"-------------------------------------------------------------------------------
" Tab
"-------------------------------------------------------------------------------
"タブ入力を複数の空白入力に置き換える
set expandtab
"画面上でタブ文字が占める幅
set tabstop=4
"自動インデントでずれる幅
set shiftwidth=4
"連続した空白に対してタブキーやバックスペースキーでカーソルが動く幅
set softtabstop=4
"改行時に前の行のインデントを継続する
set autoindent
"改行時に入力された行の末尾に合わせて次の行のインデントを増減する
set smartindent

"-----------------------------------------------------------
" qfixhowm
"-----------------------------------------------------------
" キーマップリーダー
let QFixHowm_Key = 'g'

" howm_dirはファイルを保存したいディレクトリを設定
"let howm_dir             = '~/howm'
let howm_dir             = 'C:/Users/tomoyuki/Dropbox/howm'
let howm_filename        = '%Y/%m/%Y-%m-%d-%H%M%S.txt'
let howm_fileencoding    = 'utf-8'
let howm_fileformat      = 'unix'

" キーコードやマッピングされたキー列が完了するのを待つ時間(ミリ秒)
set timeout timeoutlen=3000 ttimeoutlen=100
" プレビューや絞り込みをQuickFix/ロケーションリストの両方で有効化(デフォルト:2)
let QFixWin_EnableMode = 1

" QFixHowm/QFixGrepの結果表示にロケーションリストを使用する/しない
let QFix_UseLocationList = 1

set shellslash

" textwidthの再設定
au Filetype qfix_memo setlocal textwidth=0

" grepにagrep.vimを使用する
let mygrepprg = 'agrep.vim'

"let mygrepprg = 'C:\gnupack\app\cygwin\cygwin\bin\grep.exe'
"let MyGrep_cygwin17 = 1

"メニュー画面のプレビュー("i"でOn/Off)
let QFixHowm_MenuPreview = 1
"メニュー画面で表示する最近のメモの数
let QFixHowm_MenuRecent = 30

"------------------------------------
"NERDTreeを開く
"------------------------------------
nnoremap <silent> <leader>e :NERDTreeToggle<CR>

"------------------------------------
" unite.vim
"------------------------------------

"" http://blog.remora.cx/2010/12/vim-ref-with-unite.html
"" 入力モードで開始する
"let g:unite_enable_start_insert=1
"" バッファ一覧
"noremap <C-P> :Unite buffer<CR>
"" ファイル一覧
"noremap <C-N> :Unite -buffer-name=file file<CR>
"" 最近使ったファイルの一覧
"noremap <C-Z> :Unite file_mru<CR>
"" sourcesを「今開いているファイルのディレクトリ」とする
"noremap :uff :<C-u>UniteWithBufferDir file -buffer-name=file<CR>
"" ウィンドウを分割して開く
"au FileType unite nnoremap <silent> <buffer> <expr> <C-J> unite#do_action('split')
"au FileType unite inoremap <silent> <buffer> <expr> <C-J> unite#do_action('split')
"" ウィンドウを縦に分割して開く
"au FileType unite nnoremap <silent> <buffer> <expr> <C-K> unite#do_action('vsplit')
"au FileType unite inoremap <silent> <buffer> <expr> <C-K> unite#do_action('vsplit')
"" ESCキーを2回押すと終了する
"au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
"au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>

"------------------------------------

" 入力モードで開始する 0 -> nomal, 1 -> insert
let g:unite_enable_start_insert=0
let g:unite_source_history_yank_enable =1
let g:unite_source_file_mru_limit = 200
"ヤンクの履歴 -> Unite.vimからunite-history/yankが分離してneoyank.vimになった
nnoremap <silent> ,uy :<C-u>Unite history/yank<CR>
" バッファ一覧
nnoremap <silent> ,ub :<C-u>Unite buffer<CR>
" ファイル一覧
nnoremap <silent> ,uf :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
" レジスタ一覧
nnoremap <silent> ,ur :<C-u>Unite -buffer-name=register register<CR>
" 最近使ったファイルの一覧とバッファを表示
nnoremap <silent> ,uu :<C-u>Unite file_mru buffer<CR>
" ファイル非同期検索
"nnoremap <silent> ,up  :<C-u>Unite file_rec/async:!<CR>
" ESCキーを2回押すと終了する
au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>

"------------------------------------

"" 入力モードで開始する
"let g:unite_enable_start_insert=0
"" バッファ一覧
"noremap <C-U><C-B> :Unite buffer<CR>
"" ファイル一覧
"noremap <C-U><C-F> :UniteWithBufferDir -buffer-name=files file<CR>
"" 最近使ったファイルの一覧
"noremap <C-U><C-R> :Unite file_mru<CR>
"" レジスタ一覧
"noremap <C-U><C-Y> :Unite -buffer-name=register register<CR>
"" ファイルとバッファ
"noremap <C-U><C-U> :Unite buffer file_mru<CR>
"" 全部
"noremap <C-U><C-A> :Unite UniteWithBufferDir -buffer-name=files buffer file_mru bookmark file<CR>
"" ESCキーを2回押すと終了する
"au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
"au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>

""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""
" 全角スペースの表示
""""""""""""""""""""""""""""""
" http://inari.hatenablog.com/entry/2014/05/05/231307
"function! ZenkakuSpace()
"    highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=darkgray
"endfunction
"
"if has('syntax')
"    augroup ZenkakuSpace
"        autocmd!
"        autocmd ColorScheme * call ZenkakuSpace()
"        autocmd VimEnter,WinEnter,BufRead * let w:m1=matchadd('ZenkakuSpace', '　')
"    augroup END
"    call ZenkakuSpace()
"endif

highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=darkgray
match ZenkakuSpace /　/

""""""""""""""""""""""""""""""
"-----------------------------------------------------------
" 若干Emacs風味なvimのキー設定
"-----------------------------------------------------------
noremap <silent> <C-j> <esc>
inoremap <silent> <C-j> <esc>
vnoremap <silent> <C-j> <esc>

noremap j gj
noremap k gk
noremap <S-h>   ^
noremap <S-j>   }
noremap <S-k>   {
noremap <S-l>   $
"noremap m  %
nnoremap <CR> A<CR><ESC>
nnoremap == gg=G''
"nnoremap <Space>n  :NERDTree<CR>
"nnoremap <Space>v  :vs<CR>:<C-u>VimShell<CR>
"nnoremap <Space>tl  :vs<CR>:TweetVimHomeTimeline<CR>
"nnoremap <Space>tm  :vs<CR>:TweetVimMentions<CR>
"nnoremap <Space>ts  :TweetVimSay<CR>
"inoremap <C-f> <C-x><C-o>

"スペースキーは一画面移動にしてみる試み
nnoremap <SPACE>   <C-d>zz
nnoremap <S-SPACE> <C-u>zz

"カーソル一文字単位移動
inoremap <silent> <C-s> <Left>
inoremap <silent> <C-d> <Right>

"単語単位移動（行末で止めたい場合）
inoremap <silent> <C-f> <C-r>=MyMoveWord_i('w')<CR>
inoremap <silent> <C-b> <C-r>=MyMoveWord_i('b')<CR>

"単語単位移動（行末で止まる必要がない場合）
inoremap <silent> <C-f> <S-Left>
inoremap <silent> <C-b> <S-Right>

"非補完時は行移動をj,kと同じ動作にして補完中は候補選択
inoremap <silent> <expr> <C-p>  pumvisible() ? "\<C-p>" : "<C-r>=MyExecExCommand('normal k')<CR>"
inoremap <silent> <expr> <C-n>  pumvisible() ? "\<C-n>" : "<C-r>=MyExecExCommand('normal j')<CR>"
inoremap <silent> <expr> <Up>   pumvisible() ? "\<C-p>" : "<C-r>=MyExecExCommand('normal k')<CR>"
inoremap <silent> <expr> <Down> pumvisible() ? "\<C-n>" : "<C-r>=MyExecExCommand('normal j')<CR>"

"行頭へ
"inoremap <silent> <C-a> <C-r>=MyJumptoBol('　。、．，／！？「」')<CR>
noremap <silent> <C-a> <HOME>
inoremap <silent> <C-a> <HOME>
vnoremap <silent> <C-a> <HOME>
"行末へ
"inoremap <silent> <C-e> <C-r>=MyJumptoEol('　。、．，／！？「」')<CR>
noremap <silent> <C-e> <END>
inoremap <silent> <C-e> <END>
vnoremap <silent> <C-e> <END>

"カーソル前の文字削除
inoremap <silent> <BS>  <C-g>u<BS>
inoremap <silent> <C-h> <C-g>u<C-h>
"カーソル後の文字削除
inoremap <silent> <Del> <C-g>u<Del>
inoremap <silent> <C-g> <C-g>u<Del>

"カーソル位置から前の単語を削除
inoremap <silent> <C-w> <C-g>u<C-r>=MyExecExCommand('normal! db')<CR>
"カーソル位置から後の単語を削除
inoremap <silent> <C-t> <C-g>u<C-r>=MyDeleteWord()<CR>

"最後に挿入した文字列を挿入
inoremap <silent> <C-z> <C-g>u<C-a>

"現在行をインデント
inoremap <silent> <Tab>   <C-g>u<C-t>
inoremap <silent> <S-Tab> <C-g>u<C-d>

"undo
inoremap <silent> <C-u> <C-g>u<C-r>=MyExecExCommand('undo', 'onemore')<CR>

"２ストロークキー使用
if 1
  "カーソル以降削除
  inoremap <silent> <C-k><C-k> <C-g>u<C-r>=MyExecExCommand('normal! D', 'onemore')<CR>
  "redo
  inoremap <silent> <C-k><C-r> <C-r>=MyExecExCommand('redo', 'onemore')<CR>
  "行連結
  inoremap <silent> <C-k><C-j> <C-g>u<C-r>=MyExecExCommand('normal! J')<CR>
  "新行挿入
  inoremap <silent> <C-k><C-n> <C-g>u<C-r>=MyExecExCommand("call cursor(line('.'), col('$'))")<CR><CR>
endif

"メタ(alt)キー使用
if 0
  "カーソル以降削除
  inoremap <silent> <C-k> <C-g>u<C-r>=MyExecExCommand('normal! D', 'onemore')<CR>
  "リドゥ
  inoremap <silent> <M-r> <C-r>=MyExecExCommand('redo', 'onemore')<CR>
  "行連結
  inoremap <silent> <M-j> <C-g>u<C-r>=MyExecExCommand('normal! J')<CR>
  "新行挿入
  inoremap <silent> <M-n> <C-g>u<C-r>=MyExecExCommand("call cursor(line('.'), col('$'))")<CR><CR>
endif

""""""""""""""""""""""""""""""
"sepが空でなければ、sepをセパレータとしてジャンプ。
"見つからなければ見かけの行頭へ。
"カーソル位置が見かけの行頭の場合は真の行頭へ。
""""""""""""""""""""""""""""""
function! MyJumptoBol(sep)
  if col('.') == 1
    call cursor(line('.')-1, col('$'))
    call cursor(line('.'), col('$'))
    return ''
  endif
  if matchend(strpart(getline('.'), 0, col('.')), '[[:blank:]]\+') >= col('.')-1
    silent exec 'normal! 0'
    return ''
  endif
  if a:sep != ''
    call search('[^'.a:sep.']\+', 'bW', line("."))
    if col('.') == 1
      silent exec 'normal! ^'
    endif
    return ''
  endif
  exec 'normal! ^'
  return ''
endfunction

""""""""""""""""""""""""""""""
"sepが空でなければ、sepをセパレータとしてジャンプ。
"見つからなければ行末へ。
""""""""""""""""""""""""""""""
function! MyJumptoEol(sep)
  if col('.') == col('$')
    silent exec 'normal! w'
    return ''
  endif

  if a:sep != ''
    let prevcol = col('.')
    call search('['.a:sep.']\+[^'.a:sep.']', 'eW', line("."))
    if col('.') != prevcol
      return ''
    endif
  endif
  call cursor(line('.'), col('$'))
  return ''
endfunction

""""""""""""""""""""""""""""""
"行末でも停止する単語単位移動コマンド
""""""""""""""""""""""""""""""
function! MyMoveWord_i(cmd)
  let isEol = 0
  if col('$') == col('.')
    let isEol = 1
  endif
  let prevline = line('.')
  silent exec 'normal! '.a:cmd
  if line('.') == prevline
    return ''
  endif
  if a:cmd == 'w'
    if isEol == 0
      call cursor(prevline, 0)
      call cursor(line('.'), col('$'))
    endif
    if line('.') - prevline > 1
      call cursor(prevline+1, 0)
      call cursor(line('.'), col('$'))
    endif
  elseif a:cmd == 'b'
    call cursor(line('.'), col('$'))
    if prevline - line('.') > 1
      call cursor(prevline-1, 0)
      call cursor(line('.'), col('$'))
    endif
  endif
  return ''
endfunction

""""""""""""""""""""""""""""""
"カーソル以降の単語削除
""""""""""""""""""""""""""""""
function! MyDeleteWord()
    if col('.') == col('$')
        return ''
    endif
    let save_cursor = getpos('.')
    silent exec 'normal! wge'
    if save_cursor[1] != line('.') || (save_cursor[2] > col('.'))
        call setpos('.', save_cursor)
        return MyExecExCommand('normal! dw', 'onemore')
    endif
    silent exec 'normal! v'
    call setpos('.', save_cursor)
    return MyExecExCommand('normal! d')
endfunction

""""""""""""""""""""""""""""""
"IMEの状態とカーソル位置保存のため<C-r>を使用してコマンドを実行。
""""""""""""""""""""""""""""""
function! MyExecExCommand(cmd, ...)
    let saved_ve = &virtualedit
    let index = 1
    while index <= a:0
        if a:{index} == 'onemore'
            silent setlocal virtualedit+=onemore
        endif
        let index = index + 1
    endwhile

    silent exec a:cmd
    if a:0 > 0
        silent exec 'setlocal virtualedit='.saved_ve
    endif
    return ''
endfunction

"-----------------------------------------------------------
" 拡張子
"-----------------------------------------------------------
au BufRead,BufNewFile *.hta set filetype=html
au BufRead,BufNewFile *.jse set filetype=javascript

