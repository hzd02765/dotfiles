set encoding=utf-8
scriptencoding utf-8
" ↑1行目は読み込み時の文字コード
" ↑2行目はVim Script内でマルチバイトを使う場合に設定する
" Vim Scritptにvimrcも含まれるので、日本語でコメントを書く場合は先頭にこの設定が必要になる

" Note: Skip initialization for vim-tiny or vim-small.
if 0 | endif

"dein Scripts-----------------------------
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=~/.vim/bundle/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('~/.vim/bundle')
  call dein#begin('~/.vim/bundle')

  " Let dein manage dein
  " Required:
  call dein#add('~/.vim/bundle/repos/github.com/Shougo/dein.vim')

  " Add or remove your plugins here:

  " スニペットの補完機能
  call dein#add('Shougo/neosnippet.vim')
  " スニペットの補完機能
  call dein#add('Shougo/neosnippet-snippets')
  " コードの自動補完
  call dein#add('Shougo/neocomplete.vim')

  " カラースキームmolokai
  call dein#add('tomasr/molokai')

  " ステータスラインの表示内容強化
  call dein#add('itchyny/lightline.vim')

  " 構文エラーチェック
  call dein#add('scrooloose/syntastic')

  " 多機能セレクタ
  call dein#add('ctrlpvim/ctrlp.vim')
  " CtrlPの拡張プラグイン. 関数検索
  call dein#add('tacahiroy/ctrlp-funky')
  " CtrlPの拡張プラグイン. コマンド履歴検索
  call dein#add('suy/vim-ctrlp-commandline')
  " CtrlPの検索にagを使う
  call dein#add('rking/ag.vim')

  " ファイラー
  call dein#add('scrooloose/nerdtree')
  " コメント
  call dein#add('tomtom/tcomment_vim')

  " ファイルオープンを便利に
  call dein#add('Shougo/unite.vim')
  " Unite.vimで最近使ったファイルを表示できるようにする
  call dein#add('Shougo/neomru.vim')
  " Unite.vimで最近使ったyankを表示できるようにする
  call dein#add('Shougo/neoyank.vim')

  " You can specify revision/branch/tag.
  call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" If you want to install not installed plugins on startup.
"if dein#check_install()
"  call dein#install()
"endif

"End dein Scripts-------------------------

"----------------------------------------------------------
" カラースキーム
"----------------------------------------------------------
set t_Co=256
" syntax enable " 構文に色を付ける

""" colorscheme molokai
if &term == "xterm-256color"
    colorscheme molokai
    hi Comment ctermfg=102
    hi Visual  ctermbg=236
endif

"-------------------------------------------------------------------------------
" File
"-------------------------------------------------------------------------------
" 更新時自動再読込み
set autoread
" 編集中でも他のファイルを開けるようにする
set hidden
" スワップファイルを作らない
set noswapfile
" バックアップを取らない
set nobackup
" 保存時に行末の空白を除去する
autocmd BufWritePre * :%s/\s\+$//ge

"----------------------------------------------------------
" 文字
"----------------------------------------------------------
set fileencoding=utf-8 " 保存時の文字コード
set fileencodings=ucs-boms,utf-8,euc-jp,cp932 " 読み込み時の文字コードの自動判別. 左側が優先される
set fileformats=unix,dos,mac " 改行コードの自動判別. 左側が優先される
set ambiwidth=double " □や○文字が崩れる問題を解決

"----------------------------------------------------------
" ステータスライン
"----------------------------------------------------------
set laststatus=2 " ステータスラインを常に表示
" set showmode " 現在のモードを表示
" set showcmd " 打ったコマンドをステータスラインの下に表示
" set ruler " ステータスラインの右側にカーソルの位置を表示する

"----------------------------------------------------------
" コマンドモード
"----------------------------------------------------------
set wildmenu " コマンドモードの補完
set history=5000 " 保存するコマンド履歴の数

"----------------------------------------------------------
" タブ・インデント
"----------------------------------------------------------
set expandtab " タブ入力を複数の空白入力に置き換える
set tabstop=4 " 画面上でタブ文字が占める幅
set softtabstop=4 " 連続した空白に対してタブキーやバックスペースキーでカーソルが動く幅
set autoindent " 改行時に前の行のインデントを継続する
set smartindent " 改行時に前の行の構文をチェックし次の行のインデントを増減する
set shiftwidth=4 " smartindentで増減する幅
autocmd BufWritePre * :%s/\s\+$//ge " 保存時に行末の空白を除去する

"----------------------------------------------------------
" 文字列検索
"----------------------------------------------------------
set incsearch " インクリメンタルサーチ. １文字入力毎に検索を行う
set ignorecase " 検索パターンに大文字小文字を区別しない
set smartcase " 検索パターンに大文字を含んでいたら大文字小文字を区別する
set hlsearch " 検索結果をハイライト

" ESCキー2度押しでハイライトの切り替え
" nnoremap <Esc><Esc> :<C-u>set nohlsearch!<CR>

"-------------------------------------------------------------------------------
" Key Map
"-------------------------------------------------------------------------------
" 「map」コマンドは、キー入力を別のキーに割り当てる

" |モード                                 | 再割当無し | 再割当有り |
" |=======================================|============|============|
" |ノーマルモード＋ビジュアルモード       | noremap    | map        |
" |コマンドラインモード＋インサートモード | noremap!   | map!       |
" |ノーマルモード                         | nnoremap   | nmap       |
" |ビジュアル(選択)モード                 | vnoremap   | vmap       |
" |コマンドラインモード                   | cnoremap   | cmap       |
" |インサート(挿入)モード                 | inoremap   | imap       |

" ※「再割当無し」を使うこと

"----------------------------------------------------------
" カーソル
"----------------------------------------------------------
set whichwrap=b,s,h,l,<,>,[,],~ " カーソルの左右移動で行末から次の行の行頭への移動が可能になる
set number " 行番号を表示
" set cursorline " カーソルラインをハイライト
set nowrap " 行の折り返し表示をしない

" 行が折り返し表示されていた場合、行単位ではなく表示行単位でカーソルを移動する
nnoremap j gj
nnoremap k gk
nnoremap <down> gj
nnoremap <up> gk

" バックスペースキーの有効化
set backspace=indent,eol,start
" <esc>キーを<Ctrl + j>に割り当て
" imap <C-j> <esc>
noremap <C-j> <esc>
noremap! <C-j> <esc>

"----------------------------------------------------------
" カッコ・タグの対応
"----------------------------------------------------------
set showmatch " 括弧の対応関係を一瞬表示する
source $VIMRUNTIME/macros/matchit.vim " Vimの「%」を拡張する

"----------------------------------------------------------
" クリップボードからのペースト
"----------------------------------------------------------
" 挿入モードでクリップボードからペーストする時に自動でインデントさせないようにする
if &term =~ "xterm"
    let &t_SI .= "\e[?2004h"
    let &t_EI .= "\e[?2004l"
    let &pastetoggle = "\e[201~"

    function XTermPasteBegin(ret)
        set paste
        return a:ret
    endfunction

    inoremap <special> <expr> <Esc>[200~ XTermPasteBegin("")
endif

"-------------------------------------------------------------------------------
" vimrc help
"-------------------------------------------------------------------------------
nnoremap <Space>. :<C-u>edit $MYVIMRC<Enter>
nnoremap <Space>s. :<C-u>source $MYVIMRC<Enter>
nnoremap <C-h> :<C-u>help<Space>
nnoremap <C-h><C-h> :<C-u>help<Space><C-r><C-w><Enter>

"-------------------------------------------------------------------------------
" print datetime
"-------------------------------------------------------------------------------
inoremap <expr> ,df strftime('%Y-%m-%d %H:%M:%S')
inoremap <expr> ,dd strftime('%Y-%m-%d')
inoremap <expr> ,dt strftime('%H:%M:%S')

"-----------------------------------------------------------
" Buffer
"-----------------------------------------------------------
" バッファリストの一つ前のバッファを開く
nnoremap <silent> ,bp :bprevious<CR>
" バッファリストの次のバッファを開く
nnoremap <silent> ,bn :bnext<CR>
" 直前のバッファを開く
nnoremap <silent> ,bb :b#<CR>

"----------------------------------------------------------
" Syntastic
"----------------------------------------------------------
" 構文エラー行に「>>」を表示
" let g:syntastic_enable_signs = 1
" 他のVimプラグインと競合するのを防ぐ
" let g:syntastic_always_populate_loc_list = 1
" 構文エラーリストを表示
" let g:syntastic_auto_loc_list = 1
" ファイルを開いた時に構文エラーチェックを実行する
" let g:syntastic_check_on_open = 1
" 「:wq」で終了する時も構文エラーチェックする
" let g:syntastic_check_on_wq = 1

" Javascript用. 構文エラーチェックにESLintを使用
"let g:syntastic_javascript_checkers=['eslint']
" Javascript以外は構文エラーチェックをしない
"let g:syntastic_mode_map = { 'mode': 'passive',
"                           \ 'active_filetypes': ['javascript'],
"                           \ 'passive_filetypes': [] }

" Python3用
" let g:syntastic_python_python_exec = '/usr/bin/python3'
" let g:syntastic_python_checkers = ["flake8"]
" let g:syntastic_python_flake8_args="--ignore=E501"

"----------------------------------------------------------
" CtrlP
"----------------------------------------------------------
" マッチウインドウの設定. 「下部に表示, 大きさ20行で固定, 検索結果100件」
let g:ctrlp_match_window = 'order:ttb,min:20,max:20,results:100'
" .(ドット)から始まるファイルも検索対象にする
let g:ctrlp_show_hidden = 1
"ファイル検索のみ使用
let g:ctrlp_types = ['fil']
" CtrlPの拡張として「funky」と「commandline」を使用
let g:ctrlp_extensions = ['funky', 'commandline']

" CtrlPCommandLineの有効化
command! CtrlPCommandLine call ctrlp#init(ctrlp#commandline#id())

" CtrlPFunkyの絞り込み検索設定
let g:ctrlp_funky_matchtype = 'path'

if executable('ag')
  let g:ctrlp_use_caching=0 " CtrlPのキャッシュを使わない
  let g:ctrlp_user_command='ag %s -i --hidden -g ""' " 「ag」の検索設定
endif

"-------------------------------------------------------------------------------
" NERDTree
"-------------------------------------------------------------------------------
if dein#check_install(['nerdtree']) == 0
    " map <C-n> :NERDTreeToggle<CR>
    " ---> \e
    nnoremap <silent> <leader>e :NERDTreeToggle<CR>

    " ブックマークを初期表示
    " let g:NERDTreeShowBookmarks=1
    let g:NERDTreeShowBookmarks=0

    " 起動時にNERDTreeを表示
    " autocmd vimenter * NERDTree
    " ファイル名が指定されてVIMが起動した場合はNERDTreeを表示しない
    autocmd StdinReadPre * let s:std_in=1
    autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
endif

"-----------------------------------------------------------
" Unite.vim
"-----------------------------------------------------------
" 入力モードで開始する 0 -> nomal, 1 -> insert
let g:unite_enable_start_insert=1
let g:unite_source_history_yank_enable =1
let g:unite_source_file_mru_limit = 200
" ヤンクの履歴 -> Unite.vimからunite-history/yankが分離してneoyank.vimになった
nnoremap <silent> ,uy :<C-u>Unite history/yank<CR>
" バッファ一覧
nnoremap <silent> ,ub :<C-u>Unite buffer<CR>
" ファイル一覧
nnoremap <silent> ,uf :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
" レジスタ一覧
nnoremap <silent> ,ur :<C-u>Unite -buffer-name=register register<CR>
" 最近使ったファイルの一覧とバッファを表示
nnoremap <silent> ,uu :<C-u>Unite file_mru buffer<CR>
" タブ一覧
nnoremap <silent> ,ut :<C-u>Unite tab<CR>

"----------------------------------------------------------
" taglist.vim
"----------------------------------------------------------
set tags=tags
" ctagsのコマンド
let Tlist_Ctags_Cmd = $HOME."/local/bin/ctags"
" 現在表示中のファイルのみのタグしか表示しない
let Tlist_Show_One_File = 1
" 右側にtag listのウインドうを表示する
let Tlist_Use_Right_Window = 1
" taglistのウインドウだけならVimを閉じる
let Tlist_Exit_OnlyWindow = 1
" \lでtaglistウインドウを開いたり閉じたり出来るショートカット
let tlist_php_settings = 'php;c:class;d:constant;f:function'
" If you want to close the taglist window when a file or tag is selected, then
" set the 'Tlist_Close_On_Select' variable to 1. By default, this variable is
" set zero and when you select a tag or file from the taglist window, the window
" is not closed.
let Tlist_Close_On_Select = 1

" map <silent> <leader>l :TlistToggle<CR>
map <silent> <leader>l :TlistOpen<CR>

"----------------------------------------------------------
" neocomplete・neosnippetの設定
"----------------------------------------------------------
if dein#check_install(['neocomplete.vim']) == 0
    " Vim起動時にneocompleteを有効にする
    let g:neocomplete#enable_at_startup = 1
    " smartcase有効化. 大文字が入力されるまで大文字小文字の区別を無視する
    let g:neocomplete#enable_smart_case = 1
    " 3文字以上の単語に対して補完を有効にする
    let g:neocomplete#min_keyword_length = 3
    " 区切り文字まで補完する
    let g:neocomplete#enable_auto_delimiter = 1
    " 1文字目の入力から補完のポップアップを表示
    let g:neocomplete#auto_completion_start_length = 1
    " バックスペースで補完のポップアップを閉じる
    inoremap <expr><BS> neocomplete#smart_close_popup()."<C-h>"

    " エンターキーで補完候補の確定. スニペットの展開もエンターキーで確定
    imap <expr><CR> neosnippet#expandable() ? "<Plug>(neosnippet_expand_or_jump)" : pumvisible() ? "<C-y>" : "<CR>"
    " タブキーで補完候補の選択. スニペット内のジャンプもタブキーでジャンプ
    imap <expr><TAB> pumvisible() ? "<C-n>" : neosnippet#jumpable() ? "<Plug>(neosnippet_expand_or_jump)" : "<TAB>"
endif

"----------------------------------------------------------
" タブ
"----------------------------------------------------------
" Anywhere SID.
function! s:SID_PREFIX()
  return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSID_PREFIX$')
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
" t1 で1番左のタブ、t2 で1番左から2番目のタブにジャンプ

map <silent> [Tag]c :tablast <bar> tabnew<CR>
" tc 新しいタブを一番右に作る
map <silent> [Tag]x :tabclose<CR>
" tx タブを閉じる
map <silent> [Tag]n :tabnext<CR>
" tn 次のタブ
map <silent> [Tag]p :tabprevious<CR>
" tp 前のタブ
