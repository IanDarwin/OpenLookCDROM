/*
 *	cconv.c -- 入力文字変換ライブラリ (for X11)
 *		ver 10.2
 */

/*
 * Copyright (C) 1988  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 *		ishisone@sra.co.jp
 */

/*
 * 概要
 *
 * 入力文字変換は、かな漢字変換の前段階であり、その目的はキーボードから
 * 入力されたキーコードを解釈することである。
 * 従って、大きく分けると次の2つの機能を持つことになる。
 *
 * 1. ローマ字かな変換など入力された文字を別の文字に変える機能
 * 2. キーの入力により、かな漢字変換で使われる「変換」、「確定」などの
 *    各種機能を呼び出す機能
 *
 * この cconv ライブラリは X Window version 11 のもとで使われることを
 * 想定している。
 */

/*
 * メモ
 *
 * version 6.0	88/06/05
 *	だいたいできた
 * version 6.1	88/06/06
 *	コメント入れ
 * version 6.2	88/06/07
 *	これだけ入れればマニュアル書かなくても大丈夫だろう
 * version 6.4	88/09/05
 *	加藤氏@東工大の意見により redo 機能を入れる
 *	ついでに static function の宣言をつける
 *	いくつかの関数を external から static に変える
 * version 6.5	88/09/07
 *	function が複数書けるようにした
 *	そのためデータ構造を変更する
 * version 6.6	88/10/07
 *	なんと readRuleFile() でオープンしたファイルを
 *	閉じるのを忘れていた -- あーはずかしい
 *	従って wterm で何回も KanjiConvert の on/off を繰り返すと
 *	初期化に失敗する
 * version 6.7	88/12/19
 *	wstrlen() はこのライブラリで定義するべきものではないので外す
 * version 6.8	89/07/21
 *	影山氏@松下電器からキーコード表記がうまく動かないとの連絡を受ける
 *	getKey() で XKEY 表記の場合、#0x１６進数 の読みとりにバグが
 *	あることが判明
 * version 7.0	89/08/16
 *	jclib の Wnn version4 対応により、変換バッファが複数持てるように
 *	なった
 *	それに合わせて、cconv もバッファが複数持てるようにしなければ
 *	ならない
 *	結局半分以上書き換えてしまったようだ
 * version 7.1	89/08/17
 *	バグフィックス
 *	モード変更通知用のコールバックを追加する
 * version 7.2	89/08/23
 *	ファンクションテーブルは、やはり ccParseRule() ではなく、
 *	ccCreateBuf() で指定するべきものなので、そのように変更する
 * version 7.3 89/08/25
 *	lint で警告が出ないように修正
 *	dumpAllRules() でちゃんと変換ルールがダンプされるように修正
 * version 7.4 89/08/26
 *	ccParseRule() で、変換定義ファイルが前と同じであれば
 *	ルールを共用するように変更
 *	ccParseRule/ccFreeRule での memory leak の修正
 * version 7.5 89/09/26
 *	変換定義ファイルで include が使えるように修正
 * version 7.6 89/09/27
 *	include の説明を書くのを忘れていたのでそれを追加
 * version 7.7 89/10/26
 *	getKey() で #数字 型の表記をした時にまだバグがあったので修正
 *	そもそもこの方式の表記ではマスクが指定できないという根本的な
 *	問題があるが、まあこれはいいことにして。
 * version 8.0 89/10/27
 *	R4 contrib 向けの Release にあたってバージョン番号を修正
 * version 8.1 89/12/25
 *	include ファイルがなかった時にコアダンプするバグを修正
 * version 8.2 90/02/15
 *	X11R4 keyboard group support 追加
 * version 8.3 90/02/16
 *	keyboard group support の結果、例えば
 *		"" shift-Right "" foo
 *		"" Right       "" bar
 *	というルールがあった時、shift-Right を押したにもかかわらず
 *	したのルールにマッチしてしまうというバグを修正
 *	定義ファイル (include ファイルも含む) のサーチパスとして
 *	環境変数 CC_DEF_PATH を使うように修正
 * version 8.4 90/04/17
 *	keyboard group support のコードのケアレスミスを修正
 *	変数名 ks1 と ks2 を間違えた
 * version 8.5 90/05/31
 *	keyboard group support の不具合を修正
 *	民田氏@アステックからのレポート
 * version 8.6 91/03/20
 *	どのルールにもマッチしなかった時にはデフォルトアクションが呼ばれるが、
 *	この処理で
 *		+ 引数が間違っていた
 *		+ XLookupString() のリターンバリューが 0 のとき (つまり
 *		  キーイベントに対応する文字列がなかった時) にデフォルト
 *		  アクションが呼ばれなかった
 *	という問題を修正
 *	デフォルトアクションが NULL の時、どのルールにもマッチしなければ
 *	ccConvchar() が -1 を返すように修正
 * version 8.7 91/03/25
 *	前のバージョンの修正により、単にシフトキーやコントロールキーを
 *	押しただけでデフォルトアクションが呼ばれるようになったが、これは
 *	ユーザの期待する動作ではないような気がするので、XLookupString() の
 *	リターンバリューが 0 の時にはデフォルトアクションを呼ばないように
 *	再修正
 *	また、デフォルトアクションが NULL でかつルールにマッチしない時に
 *	ccConvchar() が -1 を返すようにしたが、デフォルトアクションの値に
 *	よってリターンバリューが変わるのは変だし、-1 を返すのはエラーが
 *	起こったみたいなので、結局デフォルトアクションが NULL であるか
 *	どうかに関わらずマッチした時は 1、マッチしない時は 0 を返す
 *	ように修正
 *	この変更により ccConvchar() のリターンバリューが 0 かどうかチェック
 *	していたアプリケーションは動かなくなるが、今まで ccConvchar() の
 *	リターンバリューは定義していなかったのでこれはそのアプリケーションの
 *	責任
 * version 9.0 91/08/15
 *	文字のデータ型として今までずっと wchar_t を使っていたが、wchar_t の
 *	定義が機種によって異なるため、wchar という型に変更する。本当は
 *	Wnn の次期バージョンに合わせたいので後で再び変更する可能性がある
 * version 9.1 91/09/18
 *	Files 構造体の index という名前のフィールドが、SystemV 系のマシン
 *	だと strchr に変わってしまうので findex という名前に変更
 * version 9.2 91/09/23
 *	DEBUG が定義されていると変換ルールがダンプされてしまうのは kinput2
 *	で使用する場合困るので、DEBUG を DEBUG_CCONV に変更
 * version 10.0 91/10/01
 *	R5 contrib 向けのリリースにあたってバージョン番号を修正
 * version 10.1 92/06/05
 *	Display 構造体の mode_switch を参照していたが、Display 構造体は
 *	本来 opaque であり、中のメンバーを直接アクセスするのはまずいので
 *	mode_switch の値を自分で計算するように変更
 * version 10.2 94/04/21
 *	関数 eproc は varargs.h/stdarg.h を使わずに書かれていたが、
 *	移植性に問題がある (int と long のサイズが異なるマシンなど)
 *	ので修正
 */

/*
 * 使用法
 *
 * 使用法は割合簡単である。最初に変換ルール定義ファイルを読み込むために
 * ccParseRule() をコールする。
 * 次に変換バッファを作るために ccCreateBuf() を呼び、バッファを作る。
 * この時に、使用するルールや種々のコールバック関数を指定する。
 * 変換ルールと変換バッファは複数持つことができる。
 * また、前のバージョンとの互換性のために、変換ルールの読み込みと
 * 変換バッファの作成を同時に行なう、ccInit() という関数も用意されている。
 *
 * あとはキープレスのイベントが来たら ccConvchar() を呼ぶだけである。
 * このファンクションが変換を行ない、適当なコールバックルーチンを呼ぶ。
 *
 * 後は必要に応じて用意されているその他のファンクションを呼べばよい。
 */

/*
 * 変換の仕組み
 *
 * この入力文字変換の仕組みを理解するためのキーワードは次の3つである。
 *	・モード
 *	・変換ルール
 *	・コンテキスト
 *
 * ・モード
 *	入力文字変換には「モード」がある。これは例えば、
 *	「ローマ字仮名変換をしてかなを入力するモード」とか
 *	「アルファベットを入力するモード」といったものである。
 *	モードごとに異なる変換ルールが設定できる。当然モードの切替えも
 *	変換ルールとして記述できる。
 * 
 * ・変換ルール
 *	変換ルールは4つの項目から構成される。
 *		入力キー
 *		コンテキスト文字列
 *		変換結果文字列
 *		ファンクション
 *	このうち、入力キーとコンテキスト文字列がルールのマッチングに使われる。
 * 
 *	入力キーはキーコードを指定するもので、X のキーイベントコードで
 *	指定する方法と、それを XLookupString した結果のキャラクタコードで
 *	指定する方法との2通りがある。
 *
 *	コンテキスト文字列は、それまでにどのような文字が入力されたかを
 *	指定するものである。
 *
 *	変換結果文字列はその名の通り、変換結果の指定で、マッチした
 *	コンテキストがこの変換結果に置きかわる。
 *
 *	ファンクションはルールがマッチしたときに実行されるファンクション 
 *	("変換" とか "確定" とか) の指定である。ファンクションは複数
 *	指定することもできる。
 * 
 *	例をあげると、
 *	「"ky" と入っているところで 'a' というキーが押されたら
 *	"きゃっ!" と変換し、ベルを鳴らす」
 *	というルール (あんまり意味ないけど) で、
 *		'a' が入力キー、
 *		"ky" がコンテキスト、
 *		"きゃっ!" が変換結果、
 *		<ベルを鳴らす> がファンクション
 *	ということになる。
 *
 * ・コンテキスト
 *	以上のように、コンテキストというのはそれまでにどのような文字が
 *	入力されていなければならないかを指定するものである。
 *	それと同時にその文字を変換結果と置き変えることを指定するもの
 *	でもある。
 *
 *	コンテキストは空文字列でもよい。そのときにはそれまでにどのような
 *	文字が入力されたかにかかわらず、マッチングが起こる。
 *
 *	コンテキストは半角の場合大文字小文字を区別しない。
 *	どちらにもマッチする。
 *
 * ・変換の実際
 *	さて、どのようにして変換が行なわれるのかを簡単に書いてみる。
 *
 *	キープレスイベントを引数にして ccConvchar() が呼ばれる。
 *	すると、まずはイベントコードでカレントモードで定義されている
 *	変換ルールの中からマッチするものを探す。探索はルールの記述順に
 *	行なわれ、最初にマッチしたものが選ばれる。
 *	もしマッチするルールがなければ、今度はイベントコードを
 *	XLookupString() で変換したキャラクタコードで、マッチするものを探す。
 *
 *	それでもマッチするルールが見つからなかったときには、デフォルト
 *	アクションのコールバックルーチンが呼ばれる。
 *
 *	マッチするルールが見つかれば、変換結果文字列への置き換えを行なう。
 *	つまり、まずルールに記述されているコンテキスト文字列の文字数分だけ
 *	1文字削除用のコールバックルーチンが呼ばれ、その後変換結果文字列が
 *	1文字入力用のコールバックルーチンを呼んで入力される。
 *	そのルールにファンクションが指定されていればディスパッチ用の
 *	コールバックルーチンが呼び出される。
 *
 *	ファンクションが他のモードへの切替えだった場合には少し複雑である。
 *	そのときには、まず EXITMODE という疑似キーを引数にして ccConvchar()
 *	が呼ばれる。これはあるモードから抜ける際に何か処理をしたい、
 *	というときのためにある。その後カレントモードを新しいモードにして、
 *	今度は ENTERMODE という疑似キーを引数にして ccConvchar が呼ばれる。
 *	これによってあるモードに入ったときの処理をすることができる。
 *	その後、コンテキストがクリアされ、最後に、モード変更通知用の
 *	コールバック関数が指定されていればそれが呼ばれる。
 */

/*
 * ccdef ファイルの書き方
 *
 * 入力文字変換定義ファイル (ccdef ファイルと省略する) の書き方を簡単に記す。
 *
 * ccdef ファイルは次の3つのパートから構成される。
 * これらはこの順に並んでいなければならない。
 *
 *	<モード宣言>
 *	<初期モード宣言>
 *	<各モードの変換ルール記述>
 *
 * <モード宣言> は使用するモード名を宣言するもので、フォーマットは次の通り。
 *	defmode Mode1 Mode2 Mode3...
 * 使用するモードはすべてここで宣言しておかなくてはならない。
 *
 * <初期モード宣言> は cconv が初期化されたときのモードを指定するものである。
 * フォーマットは次の通り。
 *	initialmode Mode3
 *
 *
 * <各モードの変換ルール記述> が実際の変換ルールを記述する部分である。
 * 1つのモードに対する記述形式は次の通り。
 *
 *	mode <モード名> "<プロンプト>" [fallthrough <モード名>]
 *		"<コンテキスト>" <キー> "<変換結果>" [<ファンクション>...]
 *			:
 *			:
 *	endmode
 *
 * <モード名> は <モード宣言> で宣言したモード名である。
 *
 * <プロンプト> はモードを表す文字列である。これはカレントモードを表示する
 * 際に使用されるもので、漢字でもよい。長さは自由であるが、余り長いと
 * すべて表示されるかどうか疑問である。
 * その後の、[ ] にくるまれた部分は省略可能なことを示す。
 * もしあれば、このモードでマッチするルールがなかったときには fallthrough で
 * 指定されるモードのルールが引き続いて探される。
 *
 * mode と endmode にはさまれた部分が個々の変換ルールである。
 * <コンテキスト> がコンテキスト文字列である。ここでは '^' と '\' が特殊
 * キャラクタとして使用できる。"^C" と書けばそれは コントロール-C を表す。
 * バックスラッシュ '\' は次の文字の特殊文字としての意味をなくす。
 * '^' 自身を使いたければ "\^" とエスケープする。同様に '\' 自身を使いたければ
 * "\\" と重ねればよい。
 *
 * <キー> は入力されたキーで、X のキーイベント、キャラクタコード、
 * メタキャラクタ、疑似キーのいずれかで指定する。
 *
 * X のキーイベントは、基本的に、
 *	モディファイア-Keysym名
 * で表す。例えば、
 *	Tab
 *	control-a
 *	shift-control-space
 * などである。
 *
 * キャラクタコードを書くには、シングルクォートを使用する。例えば
 *	'a'
 *	'^['
 *	'\''
 * などである。わかると思うが、2番目は ESCAPE、3番目はシングルクォート
 * そのものを示す。
 *
 * メタキャラクタは次の10種類が使用できる。
 *  キーイベントでもキャラクタコードでもマッチするもの
 *	@any		- 何にでもマッチする
 *  キーイベントにマッチするもの
 *	@raw		- キーイベントなら何にでもマッチ
 *	@func		- ファンクションキーにマッチ
 *	@cursor		- カーソルキーにマッチ
 *	@keypad		- テンキーパッドのキーにマッチ
 *	@modifier	- モディファイア (shift, control など) にマッチ
 *	@non-ascii	- キーイベントのうち、XLookupString() の結果が
 *			  長さ0、つまり対応するアスキー文字がないものにマッチ
 *  キャラクタコードにマッチするもの
 *	@ascii		- キャラクタコードなら何にでもマッチ
 *	@printable	- 印字可能文字 (0x20 ≦ c ≦ 0x7e) にマッチ
 *	@control	- コントロール文字 (0x00 ≦ c ≦ 0x1f) にマッチ
 *
 * 疑似キーは本当のキー入力ではない。これには2種類ある。
 *	ENTERMODE	- あるモードに入ったときに仮想的に入力されるキー
 *	EXITMODE	- あるモードから抜けるときに仮想的に入力されるキー
 *
 * <変換結果> は変換結果を指定する。ここでは '&'、'/'、'^'、'\' の4種類の
 * 特殊文字が使用できる。'^' と '\' については <コンテキスト> と同じである。
 * '&' は マッチした <キー> と置き換えられる。<キー> がイベントの場合には
 * そのイベントを XLookupString() した結果の文字列で置き換わる。
 * '/' が出てくると、そこでコンテキストはクリアされる。
 *
 * <ファンクション> はなくてもよい。あれば変換の後、ファンクションが
 * 実行される。ファンクションを複数指定することもでき、その場合には
 * 指定された順序で実行される。ファンクションの区切りは空白文字である。
 *	func1 func2 func3
 * のように指定する。
 * ファンクションとしてカレントモードの変更を指定することもできる。
 *	goto <モード名>
 * でカレントモードが変わる。このモード名として "PREV" が使用できる。
 * これは一つ前のモードを表す。
 * また、再変換を指定することもできる。<ファンクション>として
 *	redo
 * と書くと、もう一度マッチするルールを最初から探しにいく。ただしその前に
 * <コンテキスト> が <変換結果> に置き換えられていることに注意。この機能を
 * 使う時には無限ループに陥らないように注意してルールを書かなければならない。
 * 一応安全策として redo が MAXREDO (=20) 回呼ばれると失敗するように
 * なっている。
 * 注意: goto と redo のあとに指定されたファンクションは実行されない。
 * つまり、
 *	func1 goto XX func2
 * だと、最初に func1 が実行され、次にカレントモードが XX に変更されるが
 * func2 は実行されない。
 *
 * 最後に重要な注意を一つ。ccdef ファイルは EUC コードで書かれていなければ
 * ならない。
 *
 * なお、書き忘れたが '#' で始まる行はコメント行である。
 *
 * また、他のファイルを include 文を使ってインクルードすることができる。
 * 書式は
 *	include	ファイル名
 * である。ファイル名中に空白文字が含まれる時には ' か " で囲めばよい。
 * ファイル名が相対パス名の時にはまずカレントディレクトリが探され、なければ
 * 環境変数 CC_DEF_PATH が定義されていればそれに指定されたディレクトリ
 * (':' で区切って複数指定することができる) の下が探され、それでもなければ
 * CCDEFPATH (/usr/lib/X11/ccdef/) の下が探される。
 */

/*
 * インターフェイス・ファンクション 
 *
 * - 変換ルール読み込み -
 *
 * ccRule ccParseRule(char *deffile, void (*errprint)())
 *	入力文字変換ファイルを読み込む。
 *	deffile で指定されるファイルから変換ルールを読み込む。もしも
 *	deffile が NULL の時は、環境変数 CC_DEF の値が使用される。
 *	deffile が相対パス名の時にはまずカレントディレクトリの下が探され、
 *	なければ、環境変数 CC_DEF_PATH で指定されたディレクトリの下が
 *	探される。CC_DEF_PATH には環境変数 PATH などと同じく複数の
 *	ディレクトリを ':' で区切って指定することができる。
 *	なければ CCDEFPATH (/usr/lib/X11/ccdef) の下が探される。
 *	同じ変換ファイルに対して複数回 ccParseRule() を行なっても、
 *	実際に読み込まれるのは最初の一回だけである。
 *	errprint はエラー表示用のファンクションであるが、 NULL の場合には
 *	stderr へ fprintf() を使用して出力される。
 *	成功時には変換ルールを、失敗時には NULL を返す。
 *
 * void ccFreeRule(ccRule rule)
 *	使用しなくなった変換ルールで使われていた領域を解放する。
 *
 *
 * - 変換バッファ作成 -
 *
 * ccBuf ccCreateBuf(ccRule rule, int contextsize,
 *		     char *functable[], int functablesize,
 *		     void (*default_action)(), void (*insert_char)(),
 *		     void (*delete_char)(), void (*function_dispatch)(),
 *		     void (*mode_notify)(), caddr_t client_data);
 *	変換バッファを作り、それを返す。
 *	rule で、どの変換ルールを使用するか指定する。
 *	contextsize にはコンテキストを何文字保持するかを指定する。
 *	よほど特殊なルールがない限り、普通は数文字で足りるはずである。
 *	functable はファンクション名の一覧表、functablesize はそのサイズ
 *	である。
 *	default_action は、入力されたキーがどのルールにもマッチしなかった
 *	ときに呼ばれるファンクションである。NULL を指定すると、マッチ
 *	しなかったときには何も起こらない。
 *	insert_char, delete_char はそれぞれ 1文字挿入 / 1文字削除用の
 *	ファンクションである。
 *	function_dispatch にはファンクションコールのためのディスパッチ
 *	ルーチンを指定する。
 *	mode_notify は、モードが変わった時に呼び出されるファンクションである。
 *	コールバックの必要がないときは NULL を指定しておけばよい。
 *	client_data は、コールバック時に引数として渡されるデータである。
 *	成功時にはバッファを、エラーの時には NULL が返される。
 *
 * void ccDestroyBuf(ccBuf buf)
 *	使用しなくなった変換バッファで使われていた領域を解放する。
 *
 *
 * - 変換 -
 *
 * int ccConvchar(ccBuf buf, XKeyPressedEvent *event)
 *	X11 のキーボードのプレスイベントを受け取り、変換を行なう。
 *	行なった結果、文字の挿入・削除は ccCreateBuf() で指定された
 *	insert_char, delete_char が呼び出され、ファンクションについては
 *	同じく ccCreateBuf() で指定される function_dispatch がディスパッチの
 *	ために呼ばれる。
 *	どのルールにもマッチしなければ、default_action が呼ばれる。
 *	どのルールにもマッチしなかった時には 0、マッチした時には 1 が
 *	返される。
 *
 *
 * - カレントモード -
 *
 * int ccGetMode(ccBuf buf)
 *	カレントモード番号が返される。
 *
 * wchar *ccGetModePrompt(ccBuf buf)
 *	カレントモード名が EUCプロセスコードで返される。
 *
 *
 * - バッファの使用しているルール -
 *
 * ccRule ccGetRule(ccBuf buf)
 *	指定された変換バッファが使用している変換ルールを返す。
 *
 *
 * - コンテキスト -
 *
 * コンテキストへの文字の追加や削除は、ルールにマッチしたときには
 * 自動的に行なわれる。また、カレントモードの変更にともなって
 * コンテキストは自動的にクリアされる。
 * 従って普通はユーザがこれらのファンクションを呼ぶ必要はない。
 * ただし、マッチしなかった文字を default_action が挿入するとか、
 * バックスペースの処理を行なう、などの場合にはユーザが explicit に
 * 呼ぶ必要がある。
 *
 * void ccContextAppend(ccBuf buf, int c)
 *	コンテキストの最後に文字 c (EUC プロセスコード)を付け加える。
 *
 * void ccContextDelete(ccBuf buf)
 *	コンテキストの最後の1文字を削除する。
 *
 * void ccContextClear(ccBuf buf)
 *	コンテキストをクリアする。
 *
 * void ccContextSet(ccBuf buf, wchar *cstr)
 *	コンテキスト文字列をセットする。文字列は null ターミネートされた
 *	EUC プロセスコードでなければならない。
 *	文字列の長さが ccCreateBuf() で指定された contextsize より
 *	長いときには、最後の contextsize 文字がセットされる。
 *
 * void ccContextGet(ccBuf buf, wchar *cstr)
 *	現在のコンテキスト文字列を返す。文字列は null ターミネートされた
 *	EUC プロセスコードである。
 */

/*
 * コールバックファンクションとその引数
 *
 *	ディスパッチファンクション:
 *	    function_dispatch(int func, unsigned char *str, int nbytes,
 *			      caddr_t client_data)
 *		マッチしたルールにファンクションが書かれていたときに
 *		呼び出される。
 *		引数 func は、ファンクション番号である。これは
 *		ccParseRule() の引数として渡された、functable[] の
 *		インデックスである。str, nbytes はこの
 *		ファンクションを引き起こしたキーの表す文字列。
 *		client_data は ccCreateBuf() の時に指定されたデータである。
 *
 *	デフォルトアクション用ファンクション:
 *	    default_action(unsigned char *str, int nbytes, caddr_t client_data)
 *		押されたキーに対してマッチするルールがなかったときに
 *		呼び出される。
 *		str, nbytes はキーの表す文字列。
 *		client_data は ccCreateBuf() の時に指定されたデータ。
 *
 *	文字挿入ファンクション:
 *	    insert_char(wchar c, caddr_t client_data)
 *		変換結果の文字をアプリケーションに渡すのに使用される。
 *		c は EUC プロセスコードである。このファンクションは
 *		1文字ごとに呼ばれる。
 *		client_data は ccCreateBuf() の時に指定されたデータ。
 *
 *	文字削除ファンクション:
 *	    delete_char(caddr_t client_data)
 *		変換結果にしたがって削除する文字があればその文字数分
 *		このファンクションが呼ばれる。
 *		client_data は ccCreateBuf() の時に指定されたデータ。
 *
 *	モード変更ファンクション:
 *	    mode_notify(int newmode, int oldmode, caddr_t client_data)
 *		カレントモードが変更された時にこのファンクションが
 *		呼ばれる。newmode が新しいカレントモード、oldmode が
 *		前のカレントモード番号である。
 *
 *	エラー表示ファンクション:
 *	    error_handler(char *errstr)
 *		ccParseRule() 中で、エラーが発生した時に呼び出される。
 */

#ifndef lint
static char	*rcsid = "$Id: cconv.c,v 10.2 1994/04/21 07:52:46 ishisone Rel $";
#endif

#include	<stdio.h>
#include	<X11/Xlib.h>
#include	<X11/keysym.h>
#include	<X11/Xutil.h>
#include	<X11/Xos.h>
#include	"cconv.h"

#define uchar	unsigned char
#define ushort	unsigned short
#define ulong	unsigned long

#define Malloc(size)		malloc((unsigned int)(size))
#define Realloc(p, size)	realloc((char *)(p), (unsigned int)(size))
#define Free(p)			{ if (p) (void)free((char *)(p)); }

#define Strcmp(s1, s2)		strcmp((char *)(s1), (char *)(s2))
#define Strncmp(s1, s2, n)	strncmp((char *)(s1), (char *)(s2), n)
#define Strcpy(s1, s2)		strcpy((char *)(s1), (char *)(s2))
#define Strcat(s1, s2)		strcat((char *)(s1), (char *)(s2))
#define Strlen(s)		strlen((char *)(s))

#define EPROC2(efunc, format, a) { \
	char tmp[1024]; \
	(void)sprintf(tmp, format, a); \
	eproc(efunc, tmp); }

/* キーコードのエンコード方法 */
#define RAWKEY	(1L<<31)
#define PSEUDO	(1L<<30)
#define METAC	(1L<<29)
#define META_ASCII	1	/* any ascii character (not useful) */
#define META_CONTROL	2	/* any control character */
#define META_RAW	3	/* any key event (not useful) */
#define META_ANY	4	/* anything (character or event) */
#define META_FUNC	5	/* any function key event */
#define META_CURSOR	6	/* any cursor key event */
#define META_KEYPAD	7	/* any keypad key event */
#define META_MODIFIER	8	/* any modifier key event */
#define META_PRINTABLE	9	/* any printable character */
#define META_NONASCII	10	/* key event that has no ascii interpretation */

/* pseudo key code */
#define ENTERMODE	(ulong)(PSEUDO|1)
#define EXITMODE	(ulong)(PSEUDO|2)

/* function code */
#define ENDFUNC		0xffff
#define MODECHANGE	0x8000
#define REDO		0x7fff
#define PREVMODE	(MODECHANGE|0x1000)

#define MAXREDO		20

/* key encoding */
#define ccEncodeKey(ev) ((ulong)XLookupKeysym(ev, 0) | (ulong)RAWKEY)
#define ccEncodeMask(ev) ((int)((ev)->state & 0xff))

#define ccEncodeChar(c)	((ulong)(c))

#define NOMODE		0xffff

#define MATCHED_CHAR	0x8080	/* これは EUC としては illegal なコードなので
				 * 特別な目的に用いる */
#define CCLEAR_CHAR	0x8081	/* 上に同じ */

#define CANONIC(c)	(((c) >= 'A' && (c) <= 'Z') ? c += ('a' - 'A') : c)

/* データ構造 */
/* 変換ルール */
typedef struct convdesc {
	ulong	key;		/* 入力キー */
	ushort	mask;		/* modifier mask (of X KeyEvent) */
	ushort	context;	/* context文字列 (逆順に格納される) */
	ushort	result;		/* 変換結果文字列 */
	ushort	function;	/* function ベクタのインデックス */
} ConvDesc;

/* モードごとのテーブル */
typedef struct {
	char		*name;		/* モード名 */
	int		nrule;		/* ルールの数 */
	ConvDesc	*cdbuf;		/* ルール */
	wchar		*prompt;	/* プロンプト文字列 */
	ushort		fallthrough;
} ModeTable;

/* ccParseRule() で内部表現に変換されたルール */
typedef struct _ccRule {
	char		*rulefile;	/* 定義ファイル名 */
	ccRule		next;		/* ルールリストの次の要素 */
	int		refcnt;		/* 参照数 */
	int		nmode;		/* モードの数 */
	int		initialmode;	/* 初期モード */
	ModeTable	*modes;		/* モードテーブル */
	wchar		*strbuf;	/* ストリングバッファ */
	ushort		*funcbuf;	/* ファンクションバッファ */
	int		nfunc;		/* ファンクションの数 */
	char		*funcnamebuf;	/* ファンクション名のバッファ */
	void		(*errorfunc)();	/* エラーコールバック */
} ccRuleRec;

static ccRule	ccrules;	/* 変換ルールのリスト */

/* ccCreateBuf() で作られる変換バッファ */
typedef struct _ccBuf {
	ccRule	rule;			/* 変換ルール */
	short	*functbl;
	void	(*defaultaction)();	/* callback functions */
	void	(*insertchar)();
	void	(*deletechar)();
	void	(*executefunction)();
	void	(*modenotify)();
	caddr_t	client_data;
	int	currentmode;		/* 現在のモード */
	int	previousmode;		/* 一つ前のモード */
	wchar	*context;
	wchar	*contextend;
	int	contextsize;
} ccBufRec;

#define STRSIZE		200
#define MORESTRSIZE	100
typedef struct {
	wchar	*strbuf;
	wchar	*strend;
	wchar	*strp;
} _strbufRec;

#define	CDSIZE		10
#define	MORECDSIZE	30
typedef struct {
	ConvDesc	*cdbuf;		/* ConvDesc アレイ */
	ConvDesc	*cdend;
	ConvDesc	*cdp;
} _cdbufRec;

#define FUNCSIZE	20
#define MOREFUNCSIZE	20
typedef struct {
	ushort	*funcbuf;	/* ファンクションベクタ */
	ushort	*funcend;
	ushort	*funcp;
} _funcbufRec;

#define MAXFUNC			1024	/* ファンクションの種類の上限 */
#define FUNCNAMESIZE		100
#define MOREFUNCNAMESIZE	50
typedef struct {
	int	nfunc;
	ushort	funcnames[MAXFUNC];
	char	*funcnamebuf;
	char	*funcnameend;
	char	*funcnamep;
	int	funcsize;
} _funcnameRec;

/* 定義ファイル (for include directive) */
#define MAXINC		10
typedef struct {
	int	findex;
	FILE	*fp[MAXINC];
} Files;

extern char	*malloc();
extern char	*realloc();

#ifdef __STDC__
static int wstrlen(wchar *);
static wchar *wrev(wchar *);
static void eproc(void (*)(), char *);
static wchar *promptsave(wchar *);
static int parseLine(uchar *, uchar **, int);
static FILE *openfile(char *);
static int doinclude(uchar *, Files *, void (*)());
static uchar *getline(uchar *, int, Files *, void (*)());
static int readRuleFile(ccRule, char *);
static int registMode(ccRule, int, uchar **);
static int newMode(ccRule, Files *, _strbufRec *, _funcbufRec *,
		   _funcnameRec *, int, uchar **);
static int getDesc(ccRule, uchar *, _funcbufRec *, _funcnameRec *,
		   ulong *, int *, wchar *, wchar *, int *);
static int getMode(ccRule, uchar *);
static uchar *getQuote(uchar *, wchar *, int);
static int getKey(uchar *, ulong *, int *);
static int getmask(uchar *);
static char *strinc(char *, char *);
static ulong getcode(uchar *);
static int getFunc(ccRule, _funcbufRec *, _funcnameRec *, int, uchar **);
static ccRule findRule(char *);
static void addRule(ccRule);
static void deleteRule(ccRule);
static int wstrsave(_strbufRec *, wchar *);
static int wstralloc(_strbufRec *, int);
static void wstradjust(_strbufRec *);
static ConvDesc *cdalloc(_cdbufRec *);
static void cdadjust(_cdbufRec *);
static int funcalloc(_funcbufRec *, int);
static void funcadjust(_funcbufRec *);
static int funcsearch(_funcnameRec *, char *);
static void funcnameadjust(_funcnameRec *);
static int convchar(ccBuf, ulong, int, char *, int);
static int cconvert(ccBuf, int, ulong, int, int *, char *, int);
static int metamatch(ulong, ulong, int);
static int contextMatch(ccBuf, wchar *);
static void substStr(ccBuf, wchar *, wchar *, char *, int);
#else
static int wstrlen();
static wchar *wrev();
static void eproc();
static wchar *promptsave();
static int parseLine();
static int readRuleFile();
static int registMode();
static int newMode();
static int getDesc();
static int getMode();
static uchar *getQuote();
static int getKey();
static int getmask();
static char *strinc();
static ulong getcode();
static int getFunc();
static int wstrsave();
static int wstralloc();
static void wstradjust();
static ConvDesc *cdalloc();
static void cdadjust();
static int funcalloc();
static void funcadjust();
static int funcsearch();
static void funcnameadjust();
static int convchar();
static int cconvert();
static int metamatch();
static int contextMatch();
static void substStr();
#endif

/*
 *	Private Functions
 */

static int wstrlen(str)
wchar *str;
{
	int	len = 0;

	while (*str++)
		len++;
	return len;
}

static wchar *wrev(s)
wchar *s;
{
	wchar	*str = s;
	wchar	*end = str;
	int	c;

	while (*end++)
		;
	end -= 2;
	while (str < end) {
		c = *str;
		*str++ = *end;
		*end-- = c;
	}
	return s;
}

static void eproc(efunc, msg)
void (*efunc)();
char *msg;
{
	if (efunc == NULL) {
		/* stderr にプリント */
		(void)fprintf(stderr, "%s\n", msg);
	} else {
		/* エラーハンドラを呼ぶ */
		(*efunc)(msg);
	}
}

static wchar *promptsave(str)
wchar *str;
{
	int	len = (wstrlen(str) + 1) * sizeof(wchar);
	wchar	*p = (wchar *)Malloc(len);

	if (p != NULL) {
		wchar	*q = p;
		while (*q++ = *str++)
			;
	}
	return p;
}

static int parseLine(line, argv, argvsize)
uchar *line;
uchar **argv;
int argvsize;
{
	int	c, qc;
	int	argc;
	int	state;
#define IN_WORD		1	/* ワードの中 */
#define IN_QUOTE	2	/* ワードの中でさらにクォートの中 */

	qc = 0;		/* not necessary, but for lint */
	argc = 0;
	state = 0;	/* ワードの外 */
	while (c = *line) {
		/* クォートされていない空白文字は、ワードを終らせる */
		if (state != IN_QUOTE && (c == ' ' || c == '\t' || c == '\n')) {
			/* NULL ターミネートさせる */
			*line++ = '\0';
			state = 0;
			continue;
		}
		/* ワードの外の空白以外の文字は、ワードの始まりになる */
		if (state == 0) {
			if (argc >= argvsize)
				return argc;
			argv[argc++] = line;
			state = IN_WORD;
		}
		/* バックスラッシュは、次の文字をスキップさせる */
		if (c == '\\') {
			/* とはいっても次が Nul 文字なら話は別 */
			if (*++line == '\0') {
				*--line = '\0';
				break;
			}
		} else if (state == IN_QUOTE) {
			/* クォートは始まりと同じ文字で終る */
			if (c == qc)
				state = IN_WORD;
		} else if (c == '\'' || c == '"') {
			/* クォートの外にクォート文字があればクォートの始まり */
			state = IN_QUOTE;
			qc = c;
		}
		line++;
	}

	/* 最後のクォートが閉じていないかもしれない */
	return state != IN_QUOTE ? argc : argc - 1;
}

/* openfile -- 定義ファイルをオープンする */
static FILE *openfile(file)
char *file;
{
	FILE	*fp;
	char	filename[1024];
	FILE	*fopen();
	char	*getenv();

	if ((fp = fopen(file, "r")) == NULL && *file != '/') {
		char	*p, *q;
		/* 環境変数 CC_DEF_PATH があればそのパスの下を
		 * サーチする
		 */
		if ((p = getenv("CC_DEF_PATH")) != NULL) {
			while (*p != '\0') {
				q = filename;
				while (*p != '\0' && *p != ':')
					*q++ = *p++;
				if (*p == ':') p++;
				if (q == filename) continue;
				*q++ = '/';
				*q = '\0';
				(void)Strcat(filename, file);
				if ((fp = fopen(filename, "r")) != NULL)
					return fp;
			}
		}
		/* デフォルトのサーチパス CCDEFPATH(/usr/lib/X11/ccdef) の
		 * 下をサーチする
		 */
		(void)Strcpy(filename, CCDEFPATH);
		(void)Strcat(filename, file);
		fp = fopen(filename, "r");
	}
	return fp;
}

/* doinclude -- include 行の処理をする */
static int doinclude(line, files, efunc)
uchar *line;
Files *files;
void (*efunc)();
{
	int	argc;
	uchar	*argv[2];
	char	*name;

	argc = parseLine(line, argv, 2);
	if (files->findex > MAXINC - 2) {
		eproc(efunc, "include nesting too deep");
		return -1;
	}
	if (argc < 2) {
		eproc(efunc, "missing include filename");
		return -1;
	}
	name = (char *)argv[1];
	if (*name == '\'' || *name == '"') {
		name++;
		name[strlen(name) - 1] = '\0';
	}
	if ((files->fp[++files->findex] = openfile(name)) == NULL) {
		EPROC2(efunc, "can't open %s", name);
		--files->findex;
		return -1;
	}
	return 0;
}

/* getline -- 1行読み込む (その際 include の処理を行なう) */
static uchar *getline(line, linesize, files, efunc)
uchar *line;
int linesize;
Files *files;
void (*efunc)();
{
 redo:
	if (fgets((char *)line, linesize, files->fp[files->findex])) {
		register uchar	*p = line;
		while (*p == ' ' || *p == '\t')
			p++;
		if (!Strncmp(p, "include", 7)) {
			if (doinclude(p, files, efunc) < 0) {
				return NULL;
			} else {
				goto redo;
			}
		}
		return line;
	} else if (files->findex > 0) {
		(void)fclose(files->fp[files->findex]);
		files->findex--;
		goto redo;
	}
	(void)fclose(files->fp[files->findex]);
	return NULL;
}

/* readRuleFile -- 変換ルール定義ファイルを読み込む */
static int readRuleFile(rule, file)
ccRule rule;
char *file;
{
	FILE		*fp;
	int		moderegistered;
	uchar	line[256], tmp[256];
	uchar	*argv[20];
	int		argc;
	_strbufRec	strrec;
	_funcbufRec	funcrec;
	_funcnameRec	fnrec;
	Files		files;
	void		(*efunc)() = rule->errorfunc;

	if ((fp = openfile(file)) == NULL) {
		EPROC2(efunc, "can't open file %s", file);
		return -1;
	}
	files.findex = 0;
	files.fp[0] = fp;

	moderegistered = 0;

	strrec.strbuf = NULL;
	funcrec.funcbuf = NULL;
	fnrec.nfunc = 0;
	fnrec.funcnamebuf = NULL;

	while (getline(line, sizeof(line), &files, efunc)) {
		(void)Strcpy(tmp, line);
		if ((argc = parseLine(tmp, argv, 20)) == 0)
			continue;

		/* '#' で始まる行はコメント */
		if (*line == '\0' || *line == '\n' || *line == '#') {
			continue;
		} else if (!moderegistered && argc > 1 &&
			   !Strcmp(argv[0], "defmode")) {
			/* モード定義行 */
			if (registMode(rule, argc, argv) < 0) {
				(void)fclose(fp);
				return -1;
			}
			moderegistered++;
		} else if (!Strcmp(argv[0], "mode") && argc > 2) {
			/* あるモードに対する変換定義 */
			if (!moderegistered) {
				eproc(efunc, "'mode' before 'defmode'");
				(void)fclose(fp);
				return -1;
			}
			if (newMode(rule, &files, &strrec, &funcrec, &fnrec,
				    argc, argv) < 0)
				return -1;
		} else if (!Strcmp(argv[0], "initialmode") &&
			   argc > 1) {
			if (!moderegistered) {
				eproc(efunc, "'initialmode' before 'defmode'");
				(void)fclose(fp);
				return -1;
			}
			rule->initialmode = getMode(rule, argv[1]);
		} else {
			EPROC2(efunc, "syntax error - %s", line);
		}
	}
	(void)fclose(fp);

	wstradjust(&strrec);
	funcadjust(&funcrec);
	funcnameadjust(&fnrec);

	rule->strbuf = strrec.strbuf;
	rule->funcbuf = funcrec.funcbuf;
	rule->funcnamebuf = fnrec.funcnamebuf;
	rule->nfunc = fnrec.nfunc;
#ifdef DEBUG_CCONV
	dumpAllRules(rule);
#endif

	return 0;
}

static int registMode(rule, ac, av)
ccRule rule;
int ac;
uchar **av;
{
	int		nmode;
	ModeTable	*modes;
	int		i;

	ac--, av++;

	nmode = ac;
	modes = (ModeTable *)Malloc(nmode * sizeof(ModeTable));
	if (modes == NULL) {
		eproc(rule->errorfunc, "can't alloc memory");
		return -1;
	}
	rule->modes = modes;

	for (i = 0; i < nmode; i++) {
		if ((modes[i].name = Malloc(Strlen(av[i]) + 1)) == NULL) {
			eproc(rule->errorfunc, "can't alloc memory");
			Free(modes);
			return -1;
		}
		(void)Strcpy(modes[i].name, av[i]);
		modes[i].nrule = 0;
		modes[i].cdbuf = NULL;
		modes[i].prompt = NULL;
		modes[i].fallthrough = NOMODE;
	}

	rule->nmode = nmode;
	return 0;
}

/* newMode -- あるモードについてのルールを読み込む */
static int newMode(rule, files, srec, frec, fnrec, ac, av)
ccRule rule;
Files *files;
_strbufRec *srec;
_funcbufRec *frec;
_funcnameRec *fnrec;
int ac;
uchar **av;
{
	uchar	line[256];
	int		mode;
	ulong		inkey;
	int		modmask;
	wchar		prompt[30], context[100], result[100];
	int		func;
	int		ndesc = 0;
	ModeTable	*mp;
	ConvDesc	*cdp;
	_cdbufRec	cdbuf;
	void		(*efunc)() = rule->errorfunc;

	/* フォーマットは
	 * "mode <モード名> <"プロンプト文字列"> [fallthrough <モード名>]
	 */
	/* モードのチェック */
	if ((mode = getMode(rule, av[1])) < 0) {
		EPROC2(efunc, "illegal modename: %s", av[1]);
		return -1;	/* No Such Mode */
	}
	mp = &rule->modes[mode];

	if (getQuote(av[2], prompt, 0) == NULL) {
		EPROC2(efunc, "illegal prompt: %s", av[2]);
		return -1;
	}
	mp->prompt = promptsave(prompt);
	mp->nrule = 0;
	
	if (ac > 4 && !Strcmp(av[3], "fallthrough")) {
		mp->fallthrough = getMode(rule, av[4]);
	} else {
		mp->fallthrough = NOMODE;
	}

	cdbuf.cdbuf = NULL;

	/* ルールを読んでストアする */
	while (getline(line, sizeof(line), files, efunc)) {
		/* '#' で始まる行はコメント */
		if (*line == '\0' || *line == '\n' || *line == '#')
			continue;
		if (!Strncmp(line, "endmode", 6))
			break;
		if (getDesc(rule, line, frec, fnrec, &inkey, &modmask,
			    context, result, &func)) {
			if ((cdp = cdalloc(&cdbuf)) == NULL) {
				return -1;
			}

			/* ルールのストア */
			cdp->key = inkey;
			cdp->mask = modmask;
			cdp->context = *context ? wstrsave(srec, wrev(context)) : 0;
			cdp->result = *result ? wstrsave(srec, result) : 0;
			cdp->function = func;
			ndesc++;
		} else
			EPROC2(efunc, "illegal description - %s", line);
	}

	/* ルールが１つもなければエラーにすべきだろう */
	/* と思ったが例えば ASCII モードのときにはルールが１つもないことが
	 * ありうる
	 */

	cdadjust(&cdbuf);

	mp->nrule = ndesc;
	mp->cdbuf = cdbuf.cdbuf;

	return 0;
}

static int getDesc(rule, line, frec, fnrec, keyp, maskp, context, result, funcp)
ccRule rule;
uchar *line;
_funcbufRec *frec;
_funcnameRec *fnrec;
ulong *keyp;
int *maskp;
wchar *context;
wchar *result;
int *funcp;
{
	uchar	tmp[256];
	uchar	*av[20];
	int		ac;
	void		(*efunc)() = rule->errorfunc;

	/* valid description format is:
		"context"	key	"result"	[function...]
	*/

	(void)Strcpy(tmp, line);
	ac = parseLine(tmp, av, 20);
	if (ac < 3) {
		EPROC2(efunc, "syntax error - %s", line);
		return 0;
	}

	/* context の読み込み */
	if (getQuote(av[0], context, 0) == NULL)
		return 0;

	/* キーコードを読み込んで */
	if (getKey(av[1], keyp, maskp) < 0) {
		EPROC2(efunc, "no such key (%s)", av[1]);
		return 0;
	}

	/* result を読み込んで */
	if (getQuote(av[2], result, 1) == NULL)
		return 0;

	/* ファンクションの記述があればそれを読み込む */
	/* もし相当するファンクションがなくてもエラーにしない */
	if (ac > 3) {
		*funcp =  getFunc(rule, frec, fnrec, ac - 3, &av[3]);
	} else {
		*funcp = 0;
	}

	return 1;
}

static int getMode(rule, str)
ccRule rule;
uchar *str;
{
	ModeTable	*modes = rule->modes;
	int	i;

	for (i = 0; i < rule->nmode; i++) {
		if (!Strcmp(str, modes[i].name))
			return i;
	}
	EPROC2(rule->errorfunc, "undefined mode %s", str);
	return -1;
}

/* getQuote -- クォーテーション記号で囲まれた文字列を読んで wchar にする */
static uchar *getQuote(line, str, metaf)
uchar	*line;
wchar		*str;
int		metaf;	/* '&' と '/' をメタ・キャラクタとするかどうか */
{
	int	c;
	int	quote;		/* quote flag */
	int	qc = *line++;	/* quote character */
#define SS2	0x8e
#define SS3	0x8f

	if (qc != '\'' && qc != '"')
		return((uchar *)NULL);
	
	quote = 0;
	while ((c = *line++) && c != qc) {
		if (c == '\\' && !quote) {
			quote = 1;
			continue;
		}
			
		if (c == '^' && !quote) {
			if (c = *line++)
				*str++ = c - '@';
			else
				break;
		} else if (metaf && c == '&' && !quote)
			*str++ = MATCHED_CHAR;
		else if (metaf && c == '/' && !quote)
			*str++ = CCLEAR_CHAR;
		else if (c < 0x80)
			*str++ = c;
		else if (c == SS2)
			*str++ = *line++ | 0x80;
		else if (c == SS3) {
			c = *line++;
			*str++ = (c << 8) | (*line++ & 0x7f) | 0x8000;
		} else {
			*str++ = (c << 8) | *line++ | 0x8080;
		}
	}
	*str = 0;

	return((c == qc) ? line : (uchar *)NULL);
}

/* getKey -- キーコードを読む */
static int getKey(line, keyp, maskp)
uchar *line;
ulong *keyp;
int *maskp;
{
	/*
	 * キーコードの記述法は２通り
	 * 1. ASCII 表記
	 *	'a'
	 *	'^H'
	 *	'\033'
	 *	'\xff'
	 *
	 * 2. XKEY 表記
	 *	#124
	 *	#0132
	 *	#0x58
	 *	shift-A
	 *	shift-control-meta-HENKAN
	 *
	 * pseudo code
	 *	ENTERMODE
	 *	EXITMODE
	 *
	 * wild character
	 *	@ascii
	 *	@control
	 *	@raw
	 *	@any
	 */

	int	key = 0;

	*maskp = 0;

	if (*line == '\'') {	/* シングルクォートで始まるので ASCII 表記 */
		if (*++line == '\\') {
			/* '\'' の場合(シングルクォート自身)、
			 * '\\' の場合(バックスラッシュ)
			 * '\033' のような８進表記の場合と
			 * '\x27' のような１６進表記の場合がある
			 */
			if (*++line == '\'')	/* '\'' */
				key = '\'';
			else if (*line == '\\')	/* '\\' */
				key = '\\';
			else if (*line == 'x')	/* hexadecimal */
				(void)sscanf((char *)++line, "%x", &key);
			else			/* octal */
				(void)sscanf((char *)line, "%o", &key);
			key &= 0xff;
		} else if (*line == '^') {
			/* '^' (カレット自身) またはコントロールコード */
			if (*++line == '\'')
				key = '^';
			else
				key = *line - '@';
		} else {
			key = *line;
		}
		*keyp = key;
	} else if (*line == '#') {	/* event code */
		if (*++line == '0') {	/* octal or hexadecimal */
			if (*(line + 1) == 'x')	/* hexadecimal */
				(void)sscanf((char *)line + 2, "%x", &key);
			else			/* octal */
				(void)sscanf((char *)line, "%o", &key);
		} else {
			key = atoi((char *)line);
		}
		*keyp = (ulong)key | (ulong)RAWKEY;
	} else if (!Strcmp(line, "ENTERMODE")) {
		*keyp = ENTERMODE;
	} else if (!Strcmp(line, "EXITMODE")) {
		*keyp = EXITMODE;
	} else if (*line == '@') {
		/* ワイルド・キャラクタ */
		line++;
		if (!Strcmp(line, "ascii"))
			key = META_ASCII;
		else if (!Strcmp(line, "printable"))
			key = META_PRINTABLE;
		else if (!Strcmp(line, "control"))
			key = META_CONTROL;
		else if (!Strcmp(line, "raw"))
			key = META_RAW;
		else if (!Strcmp(line, "any"))
			key = META_ANY;
		else if (!Strcmp(line, "func"))
			key = META_FUNC;
		else if (!Strcmp(line, "cursor"))
			key = META_CURSOR;
		else if (!Strcmp(line, "keypad"))
			key = META_KEYPAD;
		else if (!Strcmp(line, "modifier"))
			key = META_MODIFIER;
		else if (!Strcmp(line, "non-ascii"))
			key = META_NONASCII;
		else
			return -1;
		*keyp = (ulong)key | (ulong)METAC;
	} else {
		if ((key = getcode(line)) == 0)
			return -1;
		*keyp = (ulong)key | (ulong)RAWKEY;
		*maskp = getmask(line);
	}
	return 0;
}

/* getmask -- モディファイア・マスクを返す */
static int getmask(s)
uchar *s;
{
	int	mask = 0;
	char	buf[256];
	uchar	*p;

	if ((p = (uchar *)rindex((char *)s, '-')) == NULL)
		return 0;
	(void)strncpy(buf, (char *)s, p - s);
	buf[p - s] = '\0';
	
	if (strinc(buf, "shift"))
		mask |= ShiftMask;
	if (strinc(buf, "control"))
		mask |= ControlMask;
	if (strinc(buf, "lock"))
		mask |= LockMask;
	if (strinc(buf, "mod1"))
		mask |= Mod1Mask;
	if (strinc(buf, "mod2"))
		mask |= Mod2Mask;
	if (strinc(buf, "mod3"))
		mask |= Mod3Mask;
	if (strinc(buf, "mod4"))
		mask |= Mod4Mask;
	if (strinc(buf, "mod5"))
		mask |= Mod5Mask;
	return mask;
}

static char *strinc(s, k)
char *s;
char *k;
{
	register int	len = Strlen(k);

	while (s = index(s, *k))
		if (!Strncmp(s, k, len))
			return s;
		else
			s++;
	return NULL;
}


/* getcode -- KeySym を返す */
static ulong getcode(s)
uchar *s;
{
	register uchar	*t;
	KeySym			keysym;
	KeySym			XStringToKeysym();

	if ((t = (uchar *)rindex((char *)s, '-')) == NULL)
		t = s;
	else
		t++;
	keysym = XStringToKeysym((char *)t);
	if (keysym == NoSymbol)
		return (ulong)0;
	else
		return (ulong)keysym;
}

static int getFunc(rule, frec, fnrec, n, args)
ccRule rule;
_funcbufRec *frec;
_funcnameRec *fnrec;
int	n;
uchar	**args;
{
	int	i, j;
	uchar	*func;
	uchar	*arg;
	ushort	*fp;
	int	findex;
	void	(*efunc)() = rule->errorfunc;

	findex = funcalloc(frec, n + 1);
	fp = frec->funcbuf + findex;

	j = 0;
	while (n > 0) {
		func = *args++;
		if (!Strcmp(func, "goto")) {
			/* モードの変更 */
			if (n < 2)
			    break;	/* モード名が書いてない */
			arg = *args++;
			--n;
			if (!Strcmp(arg, "PREV")) {
				fp[j++] = PREVMODE;
				break;
			}
			if ((i = getMode(rule, arg)) < 0) {
				break;
			}
			fp[j++] = MODECHANGE | i;
			break;
		} else if (!Strcmp(func, "redo")) {
			fp[j++] = REDO;
			break;
		} else {
			/* ファンクション表の検索 */
			int	fnum;
			if ((fnum = funcsearch(fnrec, (char *)func)) < 0) {
				EPROC2(efunc, "too many functions (> %d)", MAXFUNC);
			} else {
				fp[j++] = fnum;
			}
		}
		--n;
	}
	fp[j++] = ENDFUNC;
	frec->funcp = fp + j;	/* kludge */
	return findex;
}

static ccRule
findRule(rulefile)
char *rulefile;
{
	ccRule	rule = ccrules;

	while (rule) {
		if (rule->rulefile && !strcmp(rulefile, rule->rulefile)) {
			return rule;
		}
		rule = rule->next;
	}
	return NULL;
}

static void
addRule(rule)
ccRule rule;
{
	rule->refcnt = 1;
	rule->next = ccrules;
	ccrules = rule;
}

static void
deleteRule(rule)
ccRule	rule;
{
	ccRule	rp = ccrules;
	ccRule	rp0 = NULL;

	while (rp) {
		if (rule == rp) {
			if (rp0 == NULL) {
				ccrules = rp->next;
			} else {
				rp0->next = rp->next;
			}
			return;
		}
		rp0 = rp;
		rp = rp->next;
	}
}

static int wstrsave(srec, str)
_strbufRec *srec;
wchar *str;
{
	int	len = wstrlen(str);
	int	pos;
	wchar	*p;

	pos = wstralloc(srec, len + 1);
	if (pos > 0) {
		p = srec->strbuf + pos;
		while (len-- > 0) {
			*p++ = *str++;
		}
		*p = 0;
	}
	return pos;
}

/* バッファアロケートファンクション */

static int wstralloc(srec, len)
_strbufRec *srec;
int len;
{
	int	ret;

	if (srec->strbuf == NULL) {
		/* allocate srec->strbuf */
		srec->strbuf = (wchar *)Malloc(STRSIZE * sizeof(wchar));
		if (srec->strbuf == NULL)
			return 0;
		srec->strend = srec->strbuf + STRSIZE;
		srec->strp = srec->strbuf;
		*srec->strp++ = 0;	/* dummy */
	}
	if (srec->strp + len > srec->strend) {
		/* allocate more memory */
		int	size = (srec->strp + len) - srec->strend;
		int	offset = srec->strp - srec->strbuf;
		wchar	*wp;

		if (size < MORESTRSIZE)
			size = MORESTRSIZE;
		size += srec->strend - srec->strbuf;
		wp = (wchar *)Realloc(srec->strbuf, size * sizeof(wchar));
		if (wp == NULL)
			return 0;
		srec->strp = wp + offset;
		srec->strbuf = wp;
		srec->strend = wp + size;
	}

	ret = srec->strp - srec->strbuf;
	srec->strp += len;

	return ret;
}

static void wstradjust(srec)
_strbufRec *srec;
{
	int	size = srec->strp - srec->strbuf;
	wchar	*wp;

	if (size == 0) return;
	wp = (wchar *)Realloc(srec->strbuf, size * sizeof(wchar));
	if (wp != NULL) {
		srec->strbuf = wp;
		srec->strp = srec->strend = wp + size;
	}
}

static ConvDesc *cdalloc(crec)
_cdbufRec *crec;
{
	ConvDesc	*ret;

	if (crec->cdbuf == NULL) {
		crec->cdbuf = (ConvDesc *)Malloc(CDSIZE * sizeof(ConvDesc));
		if (crec->cdbuf == NULL)
			return NULL;
		crec->cdend = crec->cdbuf + CDSIZE;
		crec->cdp = crec->cdbuf;
	}
	if (crec->cdp >= crec->cdend) {
		int	size = crec->cdend - crec->cdbuf + MORECDSIZE;
		int	offset = crec->cdp - crec->cdbuf;
		ConvDesc	*cdp;

		cdp = (ConvDesc *)Realloc(crec->cdbuf, size * sizeof(ConvDesc));
		if (cdp == NULL) {
			return NULL;
		}
		crec->cdp = cdp + offset;
		crec->cdbuf = cdp;
		crec->cdend = cdp + size;
	}

	ret = crec->cdp++;
	return ret;
}

static void cdadjust(crec)
_cdbufRec *crec;
{
	int		size = crec->cdp - crec->cdbuf;
	ConvDesc	*cdp;

	if (size == 0) return;
	cdp = (ConvDesc *)Realloc(crec->cdbuf, size * sizeof(ConvDesc));
	if (cdp != NULL) {
		crec->cdbuf = cdp;
		crec->cdp = crec->cdend = cdp + size;
	}
}

static int funcalloc(frec, n)
_funcbufRec *frec;
int n;
{
	int	ret;

	if (frec->funcbuf == NULL) {
		/* allocate funcbuf */
		frec->funcbuf = (ushort *)Malloc(FUNCSIZE * sizeof(ushort));
		if (frec->funcbuf == NULL)
			return 0;
		frec->funcend = frec->funcbuf + FUNCSIZE;
		frec->funcp = frec->funcbuf;
		*(frec->funcp)++ = ENDFUNC;	/* dummy */
	}
	if (frec->funcp + n > frec->funcend) {
		/* allocate more memory */
		int	size = (frec->funcp + n) - frec->funcend;
		int	offset = frec->funcp - frec->funcbuf;
		ushort	*up;

		if (size < MOREFUNCSIZE)
			size = MOREFUNCSIZE;
		size += frec->funcend - frec->funcbuf;
		up = (ushort *)Realloc(frec->funcbuf, size * sizeof(ushort));
		if (up == NULL)
			return 0;
		frec->funcp = up + offset;
		frec->funcbuf = up;
		frec->funcend = up + size;
	}

	ret = frec->funcp - frec->funcbuf;
	frec->funcp += n;

	return ret;
}

static void funcadjust(frec)
_funcbufRec *frec;
{
	int	size = frec->funcp - frec->funcbuf;
	ushort	*fp;

	if (size == 0) return;
	fp = (ushort *)Realloc(frec->funcbuf, size * sizeof(ushort));
	if (fp != NULL) {
		frec->funcbuf = fp;
		frec->funcp = frec->funcend = fp + size;
	}
}

static int funcsearch(fnrec, funcname)
_funcnameRec *fnrec;
char *funcname;
{
	int	nfunc = fnrec->nfunc;
	ushort	*fnames = fnrec->funcnames;
	char	*fnbuf = fnrec->funcnamebuf;
	int	i;
	int	len;

	for (i = 0; i < nfunc; i++) {
		if (!strcmp(funcname, fnbuf + *fnames++))
			return i;
	}

	if (nfunc >= MAXFUNC)
		return -1;

	len = strlen(funcname) + 1;

	/* add new function */
	if (fnrec->funcnamebuf == NULL) {
		/* allocate funcnamebuf */
		if ((fnrec->funcnamebuf = Malloc(FUNCNAMESIZE)) == NULL)
			return -1;
		fnrec->funcnameend = fnrec->funcnamebuf + FUNCNAMESIZE;
		fnrec->funcnamep = fnrec->funcnamebuf;
	}
	if (fnrec->funcnamep + len > fnrec->funcnameend) {
		/* allocate more memory */
		int	size = (fnrec->funcnamep + len) - fnrec->funcnameend;
		int	offset = fnrec->funcnamep - fnrec->funcnamebuf;
		char	*cp;

		if (size < MOREFUNCNAMESIZE)
			size = MOREFUNCNAMESIZE;
		size += fnrec->funcnameend - fnrec->funcnamebuf;
		if ((cp = Realloc(fnrec->funcnamebuf, size)) == NULL)
			return 0;
		fnrec->funcnamep = cp + offset;
		fnrec->funcnamebuf = cp;
		fnrec->funcnameend = cp + size;
	}

	(void)strcpy(fnrec->funcnamep, funcname);
	fnrec->funcnames[nfunc] = fnrec->funcnamep - fnrec->funcnamebuf;
	fnrec->funcnamep += len;

	return fnrec->nfunc++;
}

static void funcnameadjust(fnrec)
_funcnameRec *fnrec;
{
	int	size = fnrec->funcnamep - fnrec->funcnamebuf;
	char	*cp;

	if (size == 0) return;
	if (cp = Realloc(fnrec->funcnamebuf, size)) {
		fnrec->funcnamebuf = cp;
		fnrec->funcnamep = fnrec->funcnameend = cp + size;
	}
}

static int convchar(buf, key, mask, str, len)
ccBuf buf;
ulong key;	/* keysym (RAWKEY) or ascii code */
int mask;	/* modifier mask */
char *str;	/* ascii interpretation */
int len;	/* length of str */
{
	int	r;
	int	func;
	int	redocount = 0;
	ushort	*fp;
	ccRule	rule = buf->rule;

 redo:
	/* まずは現在のモードのルールで変換してみる */
	r = cconvert(buf, buf->currentmode, key, mask, &func, str, len);
	if (r < 0) {
		/* マッチするルールが見つからなかったので
		 * fallthrough で指定されるモードのルールを探す
		 */
		int	tmpmode = rule->modes[buf->currentmode].fallthrough;

		while (tmpmode != NOMODE) {
			r = cconvert(buf, tmpmode, key, mask, &func, str, len);
			if (r >= 0)	/* マッチした */
				break;
			tmpmode = rule->modes[tmpmode].fallthrough;
		}
	}

	if (r < 0)
		return -1;	/* どれにもマッチしなかった */

	if (func == 0)
		return 0;	/* 何もしない */

	fp = rule->funcbuf + func;
	while ((func = *fp++) != ENDFUNC) {
		if (func == REDO) {
			if (redocount++ > MAXREDO)
			    return -1;	/* たぶん無限ループ */
			else
			    goto redo;	/* redo -- もう一度 */
		} else if (func & MODECHANGE) {	/* カレントモードの変更 */
			int	tmpmode = buf->currentmode;
			
			/* pseudo-key の入力 */
			(void)convchar(buf, EXITMODE, 0, (char *)NULL, 0);
			
			if (func == PREVMODE) {
				buf->currentmode = buf->previousmode;
			} else {
				buf->currentmode = func & ~MODECHANGE;
			}
			buf->previousmode = tmpmode;
			
			/* pseudo-key の入力 */
			(void)convchar(buf, ENTERMODE, 0, (char *)NULL, 0);
			
			/* モードが変わった時にはコンテキストをクリアする */
			ccContextClear(buf);

			/* モード変更コールバックがあれば呼び出す */
			if (buf->modenotify) {
				(*buf->modenotify)(buf->currentmode,
						  buf->previousmode,
						  buf->client_data);
			}
			break;
		} else {
			int	truefunc = buf->functbl[func];
			/* ディスパッチ・ルーチンを呼ぶ */
			if (truefunc >= 0) {
				(*buf->executefunction)(truefunc, str, len,
							buf->client_data);
			}
		}
	}
	return 0;
}

static int cconvert(buf, mode, inkey, mask, func, str, len)
ccBuf buf;
int mode;	/* current mode */
ulong inkey;	/* input key (raw/mapped) */
int mask;
int *func;	/* function */
char *str;
int len;
{
	ccRule		rule = buf->rule;
	ConvDesc	*entry;
	ulong		key;
	int		n;
	ModeTable	*modep;

	if (mode < 0 || mode >= rule->nmode)
		return -1;	/* No Such Mode */

	modep = &rule->modes[mode];

	if ((n = modep->nrule) <= 0)
		return -1;	/* No Rules */

	for (entry = modep->cdbuf; --n >= 0; entry++) {
		key = entry->key;
		if (key & (ulong)METAC) {
			/* ワイルドカード文字のマッチング */
			if (!metamatch(key, inkey, len == 0))
				continue;
		} else if (key & (ulong)RAWKEY && mask != entry->mask) {
			continue;
		} else if (key != inkey) {
			continue;
		}

		/* キーがマッチした */
		if (contextMatch(buf, rule->strbuf + entry->context)) {
			substStr(buf, rule->strbuf + entry->context,
				 rule->strbuf + entry->result, str, len);
			*func = entry->function;
			return 0;
		}
	}

	return -1;	/* No Match */
}

static int metamatch(rkey, inkey, nonascii)
ulong rkey;
ulong inkey;
int nonascii;
{
	int	type = (int)(rkey & 0xff);

	switch (type) {
	case META_ASCII:
		return !(inkey & (ulong)(RAWKEY|PSEUDO));
	case META_PRINTABLE:
		return (0x20 <= inkey && inkey < 0x7f);
	case META_CONTROL:
		return inkey < 0x20;
	case META_RAW:
		return inkey & (ulong)RAWKEY;
	case META_ANY:
		return 1;
	case META_FUNC:
		return (inkey & (ulong)RAWKEY) && IsFunctionKey(inkey & 0xffff);
	case META_CURSOR:
		return (inkey & (ulong)RAWKEY) && IsCursorKey(inkey & 0xffff);
	case META_KEYPAD:
		return (inkey & (ulong)RAWKEY) && IsKeypadKey(inkey & 0xffff);
	case META_MODIFIER:
		return (inkey & (ulong)RAWKEY) && IsModifierKey(inkey & 0xffff);
	case META_NONASCII:
		return (inkey & (ulong)RAWKEY) && nonascii;
	default:
		return 0;
	}
	/* NOTREACHED */
}

static int contextMatch(buf, context)
ccBuf buf;
wchar *context;
{
	wchar	*c0 = buf->contextend;
	wchar	*c1 = buf->context;

	if (context == 0 || *context == 0)
		return 1;	/* 無条件マッチ */
	if (c0 - c1 < wstrlen(context))	/* 長さのチェック */
		return 0;	/* matching fail */
	c0--;
	while (*context) {
		if (CANONIC(*c0) == CANONIC(*context))
			c0--, context++;
		else
			return 0;	/* fail */
	}
	return 1;
}

static void substStr(buf, context, result, str, len)
ccBuf buf;
wchar *context;
wchar *result;
char *str;
int len;
{
	register int	c;
	int		nbytes;
	uchar	*bufp;

	/* コンテキストの分を消す */
	while (*context++) {
		ccContextDelete(buf);
		(*buf->deletechar)(buf->client_data);
	}

	while (c = *result++) {
		if (c == MATCHED_CHAR) {
			nbytes = len;
			bufp = (uchar *)str;
			while (nbytes-- > 0) {
				c = *bufp++;
				/* bufp の中身は ASCII か カナなので
				 * EUC プロセスコードへの変換をわざわざ
				 * やる必要はない
				 */
				ccContextAppend(buf, c);
				(*buf->insertchar)(c, buf->client_data);
			}
			continue;
		} else if (c == CCLEAR_CHAR) {
			/* コンテキストをクリアする */
			ccContextClear(buf);
			continue;
		}
		ccContextAppend(buf, c);
		(*buf->insertchar)(c, buf->client_data);
	}
}

/* getModeSwitchMask -- mode-switch のモディファイアマスクを調べる */
int getModeSwitchMask(dpy)
Display *dpy;
{
	KeyCode	modeswkey;
	struct modesw {
		Display		*dpy;
		int		mode_switch;
		struct modesw	*next;
	} *msp;
	static struct modesw	*modeswlist;

	for (msp = modeswlist; msp != NULL; msp = msp->next) {
		if (dpy == msp->dpy) return msp->mode_switch;
	}
	msp = (struct modesw *)Malloc(sizeof(struct modesw));
	msp->dpy = dpy;
	msp->next = modeswlist;
	modeswlist = msp;

	msp->mode_switch = 0;

	if ((modeswkey = XKeysymToKeycode(dpy, XK_Mode_switch)) != 0) {
		XModifierKeymap	*map = XGetModifierMapping(dpy);
		int	keypermod = map->max_keypermod;
		int	modbit;
		int	i;

		for (modbit = 3; modbit < 8; modbit++) {
			for (i = 0; i < keypermod; i++) {
				if (map->modifiermap[keypermod * modbit + i] == modeswkey) {
					msp->mode_switch = 1 << modbit;
					goto found;
				}
			}
		}
	found:
		XFreeModifiermap(map);
	}

	return msp->mode_switch;
}

/*
 *	Public Functions
 */

/* ccParseRule -- 変換定義ファイルを読み込む */
ccRule ccParseRule(deffile, errprint)
char *deffile;		/* 入力文字変換定義ファイル */
void (*errprint)();	/* エラーメッセージ表示用コールバック */
{
	ccRule		rule;
	extern char	*getenv();

	/* 変換定義ファイルの読み込み */
	if (deffile == NULL) {
		/* 環境変数 CC_DEF を調べる */
		if ((deffile = getenv("CC_DEF")) == NULL) {
			return (ccRule)NULL;
		}
	}

	if (rule = findRule(deffile)) {
		/* same rule found */
		rule->refcnt++;
		return rule;
	}

	/* ccRuleRec のアロケート */
	if ((rule = (ccRule)Malloc(sizeof(ccRuleRec))) == NULL) {
		return (ccRule)NULL;
	}

	rule->errorfunc = errprint;
	rule->rulefile = Malloc(Strlen(deffile) + 1);
	if (rule->rulefile) {
		(void)Strcpy(rule->rulefile, deffile);
	}

	if (readRuleFile(rule, deffile) < 0) {
		Free(rule);
		return (ccRule)NULL;
	}

	addRule(rule);

	return rule;
}

/* ccCreateBuf -- 変換バッファを作る */
ccBuf ccCreateBuf(rule, csize, functable, nfunc,
		  def_action, insert, delete, execute, modenotify, data)
ccRule rule;		/* 使用する入力文字変換ルール */
int csize;		/* context size (文字数) */
char *functable[];	/* ファンクション・テーブル */
int nfunc;		/* functable のエントリ数 */
void (*def_action)();	/* デフォルト・アクション・コールバック */
void (*insert)();	/* 文字入力コールバック */
void (*delete)();	/* 文字削除コールバック */
void (*execute)();	/* ファンクション実行コールバック */
void (*modenotify)();	/* モード変更通知コールバック */
caddr_t data;		/* callback データ */
{
	ccBuf	buf;
	char	*funcnamep;
	short	*functblp;
	int	i, j;

	/* ccBuf のアロケート */
	if ((buf = (ccBuf)Malloc(sizeof(ccBufRec))) == NULL) {
		return (ccBuf)NULL;
	}

	buf->rule = rule;

	/* context バッファのアロケート */
	if (csize <= 0) csize = 1;
	buf->context = (wchar *)Malloc(csize * sizeof(wchar));
	if (buf->context == NULL) {
		Free(buf);
		return (ccBuf)NULL;
	}
	buf->contextend = buf->context;
	buf->contextsize = csize;

	/* function コンバートテーブルのアロケート */
	buf->functbl = (short *)Malloc(rule->nfunc * sizeof(short));
	if (buf->functbl == NULL) {
		Free(buf->context);
		Free(buf);
		return (ccBuf)NULL;
	}
	/* ccRule に入っているファンクション表と、引数で与えられた
	 * ファンクション表から、ccRule 内部のファンクション番号と
	 * 今与えられたファンクション番号との対応表を作る
	 */
	funcnamep = rule->funcnamebuf;
	functblp = buf->functbl;
	for (i = rule->nfunc, functblp = buf->functbl; i > 0; i--, functblp++) {
		for (j = 0; j < nfunc; j++) {
			if (!strcmp(functable[j], funcnamep)) {
				*functblp = j;
				break;
			}
		}
		if (j >= nfunc) *functblp = -1;

		while (*funcnamep++)
			;
	}

	buf->defaultaction = def_action;
	buf->insertchar = insert;
	buf->deletechar = delete;
	buf->executefunction = execute;
	buf->modenotify = modenotify;
	buf->client_data = data;

	/* カレントモードの設定 */
	buf->previousmode = buf->currentmode = rule->initialmode;

	return buf;
}

/* ccFreeRule -- ルールを捨てる */
void ccFreeRule(rule)
ccRule rule;
{
	ModeTable	*modep;
	int		i;

	if (rule == NULL) return;
	if (--rule->refcnt > 0) return;

	deleteRule(rule);

	for (modep = rule->modes, i = 0; i < rule->nmode; modep++, i++) {
		Free(modep->name);
		Free(modep->cdbuf);
		Free(modep->prompt);
	}
	Free(rule->rulefile);
	Free(rule->modes);
	Free(rule->strbuf);
	Free(rule->funcbuf);
	Free(rule->funcnamebuf);
	Free(rule);
}

/* ccDestroyBuf -- 変換バッファを捨てる */
void ccDestroyBuf(buf)
ccBuf buf;
{
	if (buf == NULL) return;
	Free(buf->context);
	Free(buf->functbl);
	Free(buf);
}

/* ccConvchar -- 変換用ファンクション */
int ccConvchar(buf, event)
ccBuf buf;
XKeyPressedEvent *event;	/* キーイベント */
{
	int	r;
	char	str[256];
	char	*p;
	int	nbytes;
	int	n;
	KeySym	ks1, ks2;
	int	mask;

	/* とりあえず LookupString しておく */
	nbytes = XLookupString(event, str, sizeof(str), &ks2, 0);

	/* まずはイベントコードで変換してみる */
	r = -1;
	mask = ccEncodeMask(event);

	/* まずはモディファイアを一際考慮しない KeySym を
	 * コードとして変換してみる
	 */
	ks1 = (ulong)XLookupKeysym(event, 0);
	if (ks1 != NoSymbol) {
		r = convchar(buf, (ulong)ks1 | (ulong)RAWKEY,
			     mask, str, nbytes);
	}

	/* 上がマッチしなければ、Shift, Lock, ModeSwitch を
	 * 考慮した KeySym (つまり XLookupString() が返す KeySym) が
	 * NoSymbol でなければそのコードで変換してみる
	 */
	if (r < 0 && ks2 != NoSymbol) {
		int	msw = getModeSwitchMask(event->display);

		if (mask & (ShiftMask | LockMask | msw)) {
			mask &= ~(ShiftMask | LockMask | msw);
			r = convchar(buf, (ulong)ks2 | (ulong)RAWKEY,
				     mask, str, nbytes);
		}
	}

	if (r < 0) {
		int	match = 0;

		if (nbytes == 0) return 0;

		/* ASCII 表記で変換してみる */
		p = str;
		n = nbytes;
		while (n-- > 0) {
			r = convchar(buf, ccEncodeChar(*p), 0, p, 1);
			if (r >= 0) match = 1;
			p++;
		}
		if (!match) {
			/* default action のファンクションをよぶ */
			if (buf->defaultaction != NULL) {
				(void)(*buf->defaultaction)(str, nbytes, buf->client_data);
			}
			return 0;
		}
	}
	return 1;
}

/* ccGetMode -- 現在のモード番号を返す */
int ccGetMode(buf)
ccBuf buf;
{
	return buf->currentmode;
}

/* ccGetModePrompt -- 現在のモードのプロンプト文字列を返す */
wchar *ccGetModePrompt(buf)
ccBuf buf;
{
	return buf->rule->modes[buf->currentmode].prompt;
}

/* ccGetRule -- 変換バッファで使われている変換ルールを返す */
ccRule ccGetRule(buf)
ccBuf buf;
{
	return buf->rule;
}

/* ccContextAppend -- コンテキストに1文字加える */
void ccContextAppend(buf, c)
ccBuf buf;
int c;
{
	wchar	*p;

	/* コンテキスト文字列に挿入 */
	if (buf->contextend - buf->context < buf->contextsize) {
		*buf->contextend++ = c;
	} else {
		/* コンテキスト文字列が一杯なので
		 * 先頭の１文字を捨ててつめる
		 */
		p = buf->context + 1;
		while (p < buf->contextend) {
			*(p - 1) = *p;
			p++;
		}
		/* あいた所に挿入 */
		*--p = c;
	}
}

/* ccContextDelete -- コンテキストを1文字削除する */
void ccContextDelete(buf)
ccBuf buf;
{
	if (buf->contextend > buf->context)
		buf->contextend--;
}

/* ccContextClear -- コンテキストをクリアする */
void ccContextClear(buf)
ccBuf buf;
{
	/* モードが変わった時には自動的にクリアされるがそれ以外に
	 * 現在のコンテキストを強制的にクリアしたい場合に用いる
	 */
	buf->contextend = buf->context;
}

/* ccContextSet -- コンテキストをセットする */
void ccContextSet(buf, cstr)
ccBuf buf;
wchar *cstr;
{
	int	len = wstrlen(cstr);
	wchar	*p = buf->context;

	if (len > buf->contextsize) {
		cstr += len - buf->contextsize;
		len = buf->contextsize;
	}
	while (len-- > 0) {
		*p++ = *cstr++;
	}
}

/* ccContextGet -- 現在のコンテキストを返す */
void ccContextGet(buf, cstr)
ccBuf buf;
wchar	*cstr;
{
	register wchar	*wp = buf->context;

	while (wp < buf->contextend)
		*cstr++ = *wp++;
	*cstr = 0;
}


/*
 *	Obsolete Functions
 */

/* ccInit -- 変換ルールを読み込んでバッファを作る */
ccBuf ccInit(deffile, contextsize, defactfunc, insertfunc, deletefunc, dofunc,
	     errprint, functable, functablesize)
char *deffile;
int contextsize;
void (*defactfunc)();
void (*insertfunc)();
void (*deletefunc)();
void (*dofunc)();
void (*errprint)();
char *functable[];
int functablesize;
{
	ccRule	rule;

	if ((rule = ccParseRule(deffile, errprint)) == NULL) {
		return (ccBuf)NULL;
	}

	return ccCreateBuf(rule, contextsize, functable, functablesize,
			   defactfunc, insertfunc, deletefunc, dofunc,
			   (void (*)())NULL, (caddr_t)NULL);
}

/* ccTerminate -- 入力文字変換を終了する */
void ccTerminate(buf)
ccBuf buf;
{
	ccFreeRule(buf->rule);
	ccDestroyBuf(buf);
}


#ifdef DEBUG_CCONV
/*
 *	Debug Functions
 */

static void putws(s)
wchar *s;
{
	unsigned char	line[256];

	(void)convJWStoSJIS(s, line);
	fputs(line, stdout);
}

static void puteuc(s)
uchar *s;
{
	wchar	tmp[256];

	(void)convEUCtoJWS(s, tmp);
	putws(tmp);
}

void dumpRules(rule, mode)
ccRule rule;
int mode;
{
	int	nkey;
	ModeTable	*modep;
	ConvDesc	*cdp;
	wchar		*strbuf = rule->strbuf;
	ushort		*funcbuf = rule->funcbuf;
	char		**funcnames;
	wchar		*p, *q;
	wchar		restmp[256];
	ushort		*funcp;
	int	i, j;

	funcnames = (char **)__builtin_alloca(rule->nfunc * sizeof(char *));
{	char	*cp, **fnp;
	cp = rule->funcnamebuf;
	fnp = funcnames;
	for (i = 0; i < rule->nfunc; i++) {
		*fnp++ = cp;
		while (*cp++)
			;
	}
}

	if (mode < 0 || mode >= rule->nmode) {
		printf("No such mode %d\n", mode);
		return;
	}
	modep = &rule->modes[mode];

	printf("mode: %s (%d) prompt: ", modep->name, mode);
	putws(modep->prompt);
	if (modep->fallthrough != NOMODE) {
		printf(" fallthrough: %d", modep->fallthrough);
	}
	putchar('\n');
	cdp = modep->cdbuf;
	for (i = 0; i < modep->nrule; i++) {
		printf("rule[%d]: \"", i);
		putws(strbuf + cdp->context);
		printf("\"\t");
		if (cdp->key & RAWKEY) {
			ulong	key = cdp->key & ~RAWKEY;
			int	mask = cdp->mask;
			char	*keysymname;
			if (mask & ShiftMask) printf("shift-");
			if (mask & ControlMask) printf("control-");
			if (mask & LockMask) printf("lock-");
			if (mask & Mod1Mask) printf("mod1-");
			if (mask & Mod2Mask) printf("mod2-");
			if (mask & Mod3Mask) printf("mod3-");
			if (mask & Mod4Mask) printf("mod4-");
			if (mask & Mod5Mask) printf("mod5-");
			keysymname = XKeysymToString((KeySym)key);
			printf(keysymname ? keysymname : "<illegal keysym>");
		} else if (cdp->key & METAC) {
			switch (cdp->key & ~METAC) {
			case META_ASCII:	printf("@ascii"); break;
			case META_CONTROL:	printf("@control"); break;
			case META_RAW:		printf("@raw"); break;
			case META_ANY:		printf("@any"); break;
			case META_FUNC:		printf("@func"); break;
			case META_CURSOR:	printf("@cursor"); break;
			case META_KEYPAD:	printf("@keypad"); break;
			case META_MODIFIER:	printf("@modifier"); break;
			case META_NONASCII:	printf("@non-ascii"); break;
			case META_PRINTABLE:	printf("@printable"); break;
			default:		printf("<illegal meta>");
			}
		} else if (cdp->key & PSEUDO) {
			switch (cdp->key) {
			case ENTERMODE:	printf("ENTERMODE"); break;
			case EXITMODE:	printf("EXITMODE"); break;
			default:	printf("<illegal pseudo>");
			}
		} else {
			putchar('\'');
			if (cdp->key >= 0x80) {
				printf("\\x%x", cdp->key);
			} else if (cdp->key < 0x20) {
				putchar('^');
				putchar(cdp->key + '@');
			} else if (cdp->key == 0x7f) {
				printf("^?");
			} else {
				putchar(cdp->key);
			}
			putchar('\'');
		}
		printf("\t\"");
		p = restmp;
		q = strbuf + cdp->result;
		while (*q) {
			if (*q == MATCHED_CHAR) {
				*p++ = '&';
			} else if (*q == CCLEAR_CHAR) {
				*p++ = '/';
			} else {
				*p++ = *q;
			}
			q++;
		}
		*p = 0;
		putws(restmp);
		printf("\"\t");
		funcp = funcbuf + cdp->function;
		while (*funcp != ENDFUNC) {
			if (*funcp == REDO) {
				printf("redo ");
			} else if (*funcp == PREVMODE) {
				printf("goto prev ");
			} else if (*funcp & MODECHANGE) {
				int	gotomode = *funcp & ~MODECHANGE;
				if (gotomode < 0 || gotomode >= rule->nmode) {
					printf("<illegal goto>");
				} else {
					printf("goto %s ",
					       rule->modes[gotomode].name);
				}
			} else {
				if (*funcp >= rule->nfunc) {
					printf("<illegal function> ");
				} else {
					printf("%s ", funcnames[*funcp]);
				}
			}
			funcp++;
		}
		putchar('\n');
		cdp++;
	}
	putchar('\n');
}

void dumpAllRules(rule)
ccRule rule;
{
	int	i;

	printf("** RULE DUMP **\n");
	printf("number of modes: %d  initialmode: %s (%d)\n\n",
	       rule->nmode,
	       rule->modes[rule->initialmode].name,
	       rule->initialmode);
	for (i = 0; i < rule->nmode; i++) {
		dumpRules(rule, i);
	}
	fflush(stdout);
}
#endif
