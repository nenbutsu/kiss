ISLisp Processor KISS

KISS is aimed to be a conforming ISLisp processor.
The development is at the initial stage.

The current KISS is an interpreter written in C and ISLisp.


run make and you get kiss.exe if you're lucky.
(modify makefile according to your environment.)

If you have rlwrap installed, it helps (./kis to invoke or ./kiss without rlwrap).

(load "test/test.lisp") runs a test suite.

kiss uses gnu mp library.

I'm using msys2 mingw64 on Windows 10 and Ubuntu.

The rep loop is already running.

The object system and generic functions are working, too.

The license of this software is GPL. See gpl.txt

Download the latest version of KISS at https://github.com/nenbutsu/kiss .

Any comments are welcome.
I read English and Japanese.
-------------------------
ISLisp処理系KISS

国際標準化機構(ISO)で定められたプログラミング言語ISLispの処理系KISSです。

makeでコンパイル(makefileを環境に合わせて変えてください)

私はWindows10 msys2 mingw64環境とUbuntu環境で開発しております。
rlwrapがインストールしてあれば./kisで起動します。（なければ./kiss）
(load "test/test.lisp") でテストースーツが走ります。
まだ開発の初期段階です。

現状、repループは動いています。

ライセンスはGPLです。同梱のgpl.txtをご覧ください。

-------------------------
Yuji Minejima (峯島雄治)
mail: bmonkey@nifty.com
blog: http://minejima.jp/blog/category/islisp-kiss/
