# dotfiles
.（ドット）何とかってファイルを管理する方法です。
.vimrc,.vim,.bash_profile,.wgetrc何でも来たれです。

- 対象者:環境設定するのに毎回設定を書くのめんどくせんだよって思う人
- 前提条件:ドットファイルの設定を行っていること。GitHubについての基礎の基礎知識がある。
- 必要なソフト:git
- 終着点:環境設定がgit cloneによって一瞬で終わる。

# GitHubにリポジトリを作成
まずは何はともあれGitHub上にdotfilesリポジトリが必要です。
サインインして右上の＋ボタンからnew repositoryを選択して、リポジトリを作成しましょう。
先述したとおりdotfilesで作るのが主流みたいです。まぁなんでもいいけど。
repository nameのところにdotfilesって入力して、create repositoryボタンを押せばできます。

# 今まで管理していたファイルを移動
ホームディレクトリにdotfilesディレクトリを作成して、管理対象のファイルやディレクトリをぶちこみます。
下はmacの場合での例、自分で追加したいのとかいらないのとか取捨選択してね。

$ cd ~/
$ mkdir dotfiles
$ mv .vimrc dotfiles
$ mv .vim/colors dotfiles
$ mv .vim/ftdetect dotfiles
$ mv .vim/indent dotfiles
$ mv .bash_profile dotfiles
$ mv .wgetrc dotfiles
$ mv Brewfile dotfiles
ちなみに下３つは筆者は使っていない。
みんなは使うのかなーとか思って入れてみただけです。

実際のところこの部分はコマンドでやらずにFinderやエクスプローラーでやっても同じです。

# シンボリックリンクを張る
まぁいわゆるショートカットです。
シェルスクリプトやバッチファイルにしちゃいます。(dotfilesディレクトリ内に作ってください。)

Linux
dotfilesLink.sh
$ #!/bin/sh
$ ln -sf ~/dotfiles/.vimrc ~/.vimrc
$ ln -sf ~/dotfiles/colors ~/.vim
$ ln -sf ~/dotfiles/ftdetect ~/.vim
$ ln -sf ~/dotfiles/indent ~/.vim
$ ln -sf ~/dotfiles/.bash_profile ~/.bash_profile
$ ln -sf ~/dotfiles/.wgetrc ~/.wgetrc
$ ln -sf ~/dotfiles/Brewfile ~/Brewfile
Windows(Linux系のファイルは入れないようにしてください。lnコマンドと比べターゲットとリンク先の指定が逆なので注意)
mklink.bat
mklink %HOMEPATH%"\.vimrc" %HOMEPATH%"\dotfiles\.vimrc"
mklink /D %HOMEPATH%"\.vim\ftdetect" %HOMEPATH%"\dotfiles\ftdetect"
mklink /D %HOMEPATH%"\.vim\indent" %HOMEPATH%"\dotfiles\indent"
mklink /D %HOMEPATH%"\.vim\colors" %HOMEPATH%"\dotfiles\colors"
exit 0
スクリプトを作りましたら、まずは動かしてみてください。
シンボリックリンク(いわゆるショートカット)ができているかと思います。
以降ほかの環境でgit cloneでGitHubから持ってきてスクリプトを動かす際、同名のファイルは消すか移動してから動かしましょう。(Winのみ)

# 現在のディレクトリ構成
こんな感じです。ただしcolors,ftdetect,indentは筆者の環境下での話になります。

dotfiles
  │  .vimrc
  │  .bash_profile
  │  .wgetrc
  │  dotfilesLink.sh
  │  mklink.bat
  │  Brewfile
  │
  ├─colors
  │      desert.vim
  │
  ├─ftdetect
  │      bas.vim
  │      cls.vim
  │      frm.vim
  │
  └─indent
          css.vim
          html.vim
          javascript.vim
          ruby.vim
          scss.vim
# GitHubへのpush
ここの説明がめんどくさいのでコマンドだけ、
意味はcommitとpushしかできない人のためのgithubの使い方まとめを参照してみて。

$ cd ~/dotfiles # if windows use this > cd %HOMEPATH%/dotfiles
$ git init
$ git add .
$ git commit -m 'first commit'
$ git remote add origin git://github.com/your_name/dotfiles.git
$ git push origin master # type username & password
下から２行目のyour_nameの部分は自身のgithubアカウント名ね。

# 他のコンピュータからの取得
他のコンピュータからgit cloneして設定を持ってきます。

$ cd ~/ # if windows use this > cd %HOMEPATH%
$ git clone https://github.com/your_name/dotfiles.git
$ sh dotfilesLink.sh # if windows use this > mklink.bat
# 更新
dotfilesのファイルを更新した際は更新をコミットした後にプッシュしてGitHubに送り込みます。

$ git add .
$ git commit -m "anything"
$ git push origin master
新規のファイルが無い場合は以下で代用できます。

$ git commit -a -m "anything"
$ git push origin master
# 同期方法
↑の更新を取得する場合はGitHubの状態と一致させます。

$ git pull
# 最後に
なぜ.vim以下全てを持っていかないのか->私はなるべく最新のpluginなど使いたいですし、要らんものがいっぱいついてくるので必要最低限しかもっていきません。NeoBundleInstallで入るのですからいいのです。
Windowsなら以前紹介したchocolateyの設定を入れて置くのもありだと思います。

追記
もっと自動化しましたミニマルに始めるDotfiles自動化計画
