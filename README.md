# ltmpc
C++11TMP compile time C compiler

## Usage
```
$ clang++-4.0 -std=c++11 main.cpp
$ ./a.out > asm.s
$ gcc -m32 asm.s
$ ./a.out
```

## ltmpcについて
ltmpcは、C++11のテンプレートメタプログラミングで書かれたコンパイル時C言語コンパイラです。
上のコマンド例は、以下のN Queen問題を解くC言語ソースコードをコンパイルし、実行するものです。
ソースコードは https://gist.github.com/rui314/2018964 のものを参考にしていますが、ltmpcの未実装部分を避けるために、一部変更した点があります。

## 詳しい使い方
コンパイラへの入力は、入力ファイルの先頭に`R"(`を、末尾に`)";`をつけることで、C++の生文字リテラルとし、`ltmpc.hpp`にインクルードすることで行います。
`main.cpp`をコンパイルするのではなく、`dump.cpp`をコンパイルすると、コンパイラの途中状況をダンプすることができます。

## ltmpcを使用するのに必要な環境
ltmpcは、clang++-4.0以降でコンパイルできます（それ以前のclang++には、VoidTupleイディオムを使っている部分が正しくコンパイルできないというバグがあるため、失敗します）。
ltmpcの出力は、x86(32bit)のアセンブリです。これは、`gcc -m32`コマンドでアセンブルできますが、32bitのlibcが必要です。例えば、`$ sudo apt-get install libc6-dev-i386`でインストールできます。
アセンブルした結果は、x86(32bit)のバイナリであり、64bit環境では動かせないかもしれません。x86-64 Ubuntu環境であれば、`$ sudo apt-get install lib32z1`で動かせるはずです。

## ltmpcでコンパイルできないもの
今後制作を進めていくうちにコンパイルできるようになる予定です。
ここに上げたもの以外にも、単なるバグやC言語規格の勘違い・不知などがたくさんある見込みです。

### Lexer
* Cプリプロセッサすべて
* `\xdd`形式のエスケープ文字

### Parser
* `struct`, `union`, `enum`, `typedef`
* `short`, `long`, `float`, `double`, `unsigned`, `signed`
* `auto`, `register`, `static`, `extern`
* `const`, `volatile`
* `goto`, `switch`, `case`
* 十六進整数リテラル、八進整数リテラル、二進整数リテラル
* suffix付きの整数リテラル
* 配列宣言の要素数に整数リテラル以外を使用すること
* 変数宣言時の初期化
* K&Rスタイルの関数宣言

### TypeAnalysis
* 条件演算子の共通型を導き出す部分が不正確
* 同じ名前の識別子が出現した時、内側のスコープのものを優先するべきだが、UndefinedIdentifier扱いになる

### CodeEmitter
未実装の部分が多く、ごく一部の命令しか出力できないです。
ここまで順調にコンパイルのステージが進んでいることは、`dump.cpp`をコンパイルするとわかります。
出力できるのは、
* 自動変数の宣言と利用、グローバル変数の宣言
* 二項加算、二項減算、通常代入、大小比較、等値比較、後置増分、後置減分、配列添字
* `return`文、関数ポインタを介さない関数呼び出し
* `for`文、`if``else`文

くらいです。
