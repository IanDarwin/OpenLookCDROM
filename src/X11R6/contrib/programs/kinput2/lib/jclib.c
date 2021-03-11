/*
 *	jclib -- かな漢字変換用ライブラリ (Wnn Version4.0 対応版)
 *		version 5.2
 *		ishisone@sra.co.jp
 */

/*
 * Copyright (c) 1989  Software Research Associates, Inc.
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
 * Portability issue:
 *
 *	+ define SYSV, SVR4 or USG if you don't have bcopy() or bzero().
 *
 *	  if you define USG (which should be defined if your OS is based
 *	  on System V Rel 2) or SYSV (in case of System V Rel 3),
 *	  memchr() is used for bzero(), and my own version of bcopy()
 *	  is used in order to handle overlapping regions.
 *
 *	  if you define SVR4 (yes, System V Rel4), memmove() is used for
 *	  bcopy(), and memchr() is used for bzero().
 *
 *	+ jclib assumes bcopy() can handle overlapping data blocks.
 *	  If your bcopy() can't, you should define OVERLAP_BCOPY,
 *	  which force to use my own bcopy() rather than the one
 *	  in libc.a.
 */

/*
 * 概要
 *
 * jclib は Wnn の日本語入力ライブラリ jslib の上に作られた
 * (比較的)高レベルのライブラリである。かな漢字変換部分だけが
 * 用意されているので、その他辞書に関する操作などは
 * 直接 jslib を呼び出すことになる。
 *
 * jclib はかなバッファとディスプレイバッファという２つのバッファを持つ。
 * かなバッファには読み文字列が入り、ディスプレイバッファには変換結果
 * (表示文字列)が入る。かなバッファと言う呼び方はあまり正確ではない。
 * Wnn Version 4 では漢字かな変換もできるからである。
 *
 * ドットとカレント文節という概念を持ち、文字の挿入 / 削除はドットの位置に
 * 対して行なわれ、変換その他の操作はカレント文節に対して行なわれる。
 * Wnn Version 4 では大文節と小文節という２種類の文節の概念があるが、
 * それに対応して jclib もこの２種類を扱うことができる。
 *
 * このライブラリは次のような機能を提供する。
 *	・かなバッファへの文字の挿入 / 削除
 *	・かな漢字変換 / 再変換 / 無変換
 *	・ひらがな⇔カタカナ変換
 *	・確定
 *	・文節の拡大 / 縮小
 *	・カレント文節 / ドットの移動
 *	・次候補/前候補置き換え
 *	・候補取り出し / 選択
 *	・バッファのクリア
 *
 * 文字コードとしては Wnn と同じく EUC 内部コード (process code) を使用する。
 */

/*
 * Wnn Version 4 対応にあたって
 *
 * jclib はもともと Wnn Version 3 の libjd の上に作られたライブラリである。
 * これをを ver4 対応にするにあたって、いくつかの方針を立てた。
 *
 * 1. かなバッファとディスプレイバッファの二つの文字バッファを持ち、
 * かなバッファには読み、ディスプレイバッファには変換結果が入るとか
 * 様々な操作はカレント文節と呼ばれる文節に対して行なわれるとかいった
 * 基本的なコンセプトは変えない。
 *
 * 2. 昔のライブラリを使ったアプリケーションが新しいライブラリに
 * 移行しやすいように、ファンクションインターフェイスもできるだけ
 * 似たものにする。
 *
 * 3. 1,2 の方針をできるだけ守りつつ、version4.0 で導入された次のような
 * 機能をサポートする。
 *	・環境
 *	・マルチバッファ (変換バッファが複数持てるようにする)
 *	・大文節 / 小文節 (大文節を基本とするが、小文節も扱えるようにする)
 *
 * 4. 1 から 3 までの方針に従いつつ、できるだけ急いで作る :-)
 */

/*
 * メモ
 *
 * ver 0.0	89/07/21
 *	とりあえず作りはじめる
 * ver 0.1	89/08/02
 *	半分くらいかけた
 *	次候補関連がまだできていない
 * ver 0.2	89/08/04
 *	jcInsertChar() / jcDeleteChar() を作成
 * ver 0.3	89/08/07
 *	一応できた
 *	まだいくつか疑問点があるけれど
 * ver 0.4	89/08/08
 *	今使ったよビットの扱いを残して、ほぼできたのではないかと
 *	思われる
 *	細かいバグをかなり修正
 * ver 0.5	89/08/09
 *	立木さん@KABA に質問した所、今使ったよビットを落すのも
 *	クライアント側の責任であることがわかる
 *	これへの対応
 *	ついでにデータ構造の説明を追加
 *	ファイルのサイズが 80KB を越えてしまった
 *	コメントをとればかなり小さくなるんだけど
 * ver 0.6	89/08/22
 *	jcDeleteChar() を全面的に書き直す
 *	これで一応正しく動作するようになった
 *	jcInsertChar() で最後の clauseInfo の設定が間違っていたので
 *	それを修正
 *	jcPrintDetail() に簡単な clauseInfo データの consistency check を
 *	入れる
 * ver 0.7	89/08/26
 *	jcExpand() のバグ修正
 *	小文節の単文節変換を少し修正
 * ver 0.8	89/08/30
 *	changecinfo() で conv フラグをセットするのを忘れていた
 *	moveKBuf()/moveDBuf()/moveCInfo() を少し修正
 *	SYSV が define されていれば bcopy()/bzero() の代わりに
 *	memcpy()/memset() を使うように修正
 * ver 0.9	89/09/22
 *	setLCandData() で次候補バッファの候補数にカレント大文節の
 *	分を加えるのを忘れていた
 * ver 0.10	89/10/16
 *	wnn-4.0.1 で commonheader.h -> commonhd.h になったので
 *	それの修正
 * ver 0.11	89/10/18
 *	USG が define されていても memcpy()/memset() を使うように修正
 * ver 0.12	89/10/19
 *	resizeBuffer() でドットの再設定を忘れているという重大なバグを修正
 * ver 4.0	89/10/27
 *	バージョン番号を修正して 4.0 にする。
 * --- kinput を R4 に contribute ---
 * ver 4.1	90/06/04
 *	クライアント側にある辞書・頻度ファイルのセーブができないという
 *	重大なバグを修正
 * ver 4.2	90/06/15
 *	辞書が登録可能かどうかの判定が間違っていて、逆変換可能辞書の
 *	セーブができないというまたまた重大なバグを修正
 *	今のところ kinput/wterm とも単語登録機能がついてないので
 *	実害はなかったが
 * ver 4.3	91/08/15
 *	文字データ型として wchar_t ではなく、wchar を使うようにする
 *	最終的には Wnn の次期バージョンの型に合わせるつもり
 * ver 4.4	91/09/18
 *	SYSV または USG が定義されている場合には自動的に OVERLAP_BCOPY
 *	も定義するようにした
 *	SVR4 が定義されている場合には bcopy の代わりに memmove() を使用
 *	するようにした
 * ver 4.5	91/09/23
 *	DEBUG を DEBUG_JCLIB に変更
 * ver 5.0	91/10/01
 *	kinput2 リリース向けにバージョン番号を修正して 5.0 にする。
 * --- kinput2 を R5 に contribute ---
 * ver 5.1	92/02/07
 *	John Yates さん (yates@bldrsoft.com) から getLCandDataLen() で
 *	文字数を数え間違えていたのを指摘されたのでそれの修正
 * ver 5.2	92/12/24
 *	jcInsertChar() でデータの初期化をしていなかった部分があった
 *	ので修正 (値が代入されるまで使用されないのでバグではないのだが
 *	ちょっと気持ちわるいので)
 */

/*
 * ファンクション
 *
 * jcConvBuf *jcCreateBuffer(struct wnn_env *env, int nclause, int buffersize)
 *	指定された環境を使って変換のバッファを作成する。バッファは
 *	複数作ることができる。一つのバッファでは同時に複数の文を
 *	変換することはできないので、複数の文を並行して変換したい場合には
 *	幾つかのバッファを用意しなくてはならない。
 *	環境の設定までを予めやっておく必要がある。つまりサーバとの接続、
 *	環境の生成、辞書の設定などは jclib の守備範囲ではない。
 *	引数の nclause と buffersize で、それぞれ初期化時にアロケートする
 *	文節情報およびかな・ディスプレイバッファの大きさが指定できる。
 *	ただしこれらは、サイズが足りなくなれば必要に応じて自動的に
 *	増やされるため、ここに指定した以上の数の文節や、文字列が変換できない
 *	わけではない。それぞれ 0 または負の値を指定すると、デフォルトの
 *	サイズでアロケートされる。従って通常は nclause/buffersize とも
 *	0 を指定しておけばよい。
 *	リターンバリューとしてバッファを返す。エラーの時には NULL が
 *	返される。
 *
 * int jcDestroyBuffer(jcConvBuf *buf, int savedic)
 *	バッファの使用を終了する。環境を消したり、サーバとの接続を切ったり
 *	することは jclib の守備範囲外である。
 *	引数 savedic が 0 でなければ、環境中で使用されている全ての辞書を
 *	セーブする。
 *
 * int jcClear(jcConvBuf *buf)
 *	バッファをクリアする。新たに変換を始める際には最初にこの
 *	ファンクションを呼ばなければならない。
 *
 * int jcInsertChar(jcConvBuf *buf, int c)
 *	ドットに１文字挿入する。
 *	カレント文節が既に変換されていれば無変換の状態に戻る。
 *	カレント文節は大文節である。
 *
 * int jcDeleteChar(jcConvBuf *buf, int prev)
 *	ドットの前又は後ろの１文字を削除する。
 *	カレント文節が既に変換されていれば無変換の状態に戻る。
 *	カレント文節は大文節である。
 *
 * int jcConvert(jcConvBuf *buf, int small, int tan, int jump)
 *	カレント文節から後ろを変換する。
 *	引数 tan が 0 なら連文節変換、そうでなければカレント文節を
 *	単文節変換し、そのあとを連文節変換する。
 *	引数 small が 0 でなければ小文節が、そうでなければ大文節が
 *	カレント文節として使われる。
 *	引数 jump で、変換後のカレント文節の位置が決まる。jump が
 *	0 ならカレント文節の位置は変換しても変わらない (ただし
 *	カレント文節として大文節を指定した場合、変換後のカレント
 *	小文節はカレント大文節の最初の小文節になる) が、0 でなければ
 *	最後の文節の次 (空文節) に移動する。逐次変換していくような
 *	アプリケーションではこれを 1 にするとよいだろう。
 *
 * int jcUnconvert(jcConvBuf *buf)
 *	カレント大文節を無変換の状態に戻す。
 *	カレント大文節がいくつかの小文節からできていた場合、これらの
 *	小文節はまとめられ、一つの無変換状態の文節になる。
 *	カレント小文節を無変換に戻す機能は用意しない。なぜかというと、
 *	大文節の中の 1 小文節のみが無変換になってしまうと、その文節に
 *	関して jcMove() で移動を行なった時、どう移動すればよいのか
 *	よくわからない、つまり移動のセマンティクスが不明確になってしまう
 *	からである。
 *
 * int jcKana(jcConvBuf *buf, int small, int kind)
 *	カレント文節をかなにする。
 *	引数 kind が、JC_HIRAGANA ならひらがな、JC_KATAKANA ならカタカナに
 *	変わる。文節の変換状態は変化しない。つまり変換されていれば
 *	変換状態のまま、未変換の状態なら未変換のままである。
 *	引数 small が 0 でなければカレント小文節が、そうでなければ
 *	カレント大文節が変わる。
 *	カレント大文節をかなにする場合、その中の小文節は一つにまとめられる。
 *
 * int jcFix(jcConvBuf *buf)
 *	現在、バッファにはいっている変換文字列を確定させる。
 *
 * int jcExpand(jcConvBuf *buf, int small, int convf)
 *	カレント文節の長さを１文字伸ばす。引数 convf が 0 でなければ
 *	伸ばしたあと再変換する。
 *	引数 small が 0 でなければ小文節が、そうでなければ大文節が
 *	カレント文節として使われる。
 *
 * int jcShrink(jcConvBuf *buf, int small, int convf)
 *	カレント文節の長さを１文字縮める。引数 convf が 0 でなければ
 *	縮めたあと再変換する。
 *	引数 small が 0 でなければ小文節が、そうでなければ大文節が
 *	カレント文節として使われる。
 *
 * int jcNext(jcConvBuf *buf, int small, int prev)
 *	カレント文節を次候補又は前候補で置き換える。
 *	引数 small が 0 でなければ小文節が、そうでなければ大文節が
 *	カレント文節として使われる。
 *
 * int jcCandidateInfo(jcConvBuf *buf, int small, int *ncandp, int *curcandp)
 *	次候補の情報を返す。
 *	次候補一覧を出すためには最初にこの関数を呼ぶとよい。
 *
 * int jcGetCandidate(jcConvBuf *buf, int n, wchar *candstr)
 *	指定された候補番号の文字列を返す。カレント候補番号はこの番号に
 *	変わる。ディスプレイバッファは変化しない。
 *	昔の jclib は次候補が用意されていなければ用意したが、このバージョン
 *	ではエラーになる。jcNext や jcCandidateInfo を先に呼んでおかなければ
 *	ならない。
 *
 * int jcSelect(jcConvBuf *buf, int n)
 *	指定された番号の候補でディスプレイバッファを置き換える。
 *	カレント候補番号はこの番号に変わる。
 *
 * int jcDotOffset(jcConvBuf *buf)
 *	大文節の先頭からのドットのオフセットを返す。
 *	例えば 0 ならドットがカレント文節の先頭にあることになる。
 *
 * int jcIsConverted(jcConvBuf *buf, int cl)
 *	指定された文節が変換されているかどうかを返す
 *	0 なら無変換状態
 *	1 なら変換状態
 *	-1 なら エラー
 *
 * int jcMove(jcConvBuf *buf, int small, int dir)
 *	ドット・カレント文節を移動する。
 *	カレント文節が変換済みであれば文節移動し、そうでなければ
 *	ドットのみが移動する。
 *	文節移動時に、引数 small が 0 でなければ小文節単位で移動し、
 *	そうでなければ大文節単位に移動する。
 *
 * int jcTop(jcConvBuf *buf)
 *	ドット・カレント文節を文の先頭に移動する。カレント小文節・
 *	カレント大文節ともに移動する。
 *
 * int jcBottom(jcConvBuf *buf)
 *	ドット・カレント文節を文の最後に移動する。カレント小文節・
 *	カレント大文節ともに移動する。
 *	もし、最後の文節が無変換状態であればカレント文節はその文節になり、
 *	ドットはその文節の最後に来る。そうでなければカレント文節は
 *	最後の文節の次 (つまり空の文節) に来る。
 *
 * int jcChangeClause(jcConvBuf *buf, wchar *str)
 *	カレント大文節を指定された文字列で入れ換える。
 *	ディスプレイバッファだけではなく、かなバッファの内容も
 *	置き換わる。文節は無変換状態になる。
 *
 * int jcSaveDic(jcConvBuf *buf)
 *	使用中の環境で使われている全ての辞書並びに頻度ファイルを
 *	セーブする。
 *	このファンクションは常に 0 を返す。本当にセーブされたかの
 *	チェックはしない。
 *
 * これらのファンクションは特に書かれていなければ成功の場合には 0,
 * エラーの場合には -1 を返す。
 */

/*
 * グローバル変数
 *
 * jclib で使われるグローバル変数は jcErrno ただ一つである。
 *
 * extern int jcErrno
 *	エラーの際に、エラーコードが代入される。エラーコードは jclib.h で
 *	定義されている。
 */

/*
 * データ構造
 *
 * jclib の持つデータで、アプリケーションから直接アクセスしてよいのは
 * 変換バッファ jcConvBuf 型の public member と書かれた部分のみである。
 * 直接アクセスしてよいといっても、値を参照するだけで、値を変更することは
 * 許されない。アプリケーションが勝手に値を変更した場合の jclib の動作は
 * 保証されない。
 *
 * <変換バッファ>
 *
 * jcConvBuf 型は jclib.h で次のように定義されている。
 *
 * typedef struct {
 *    /-* public member *-/
 *	int		nClause;	文節数
 *	int		curClause;	カレント文節番号
 *	int		curLCStart;	カレント大文節開始文節番号
 *	int		curLCEnd;	カレント大文節終了文節番号
 *	wchar		*kanaBuf;	かなバッファ
 *	wchar		*kanaEnd;
 *	wchar		*displayBuf;	ディスプレイバッファ
 *	wchar		*displayEnd;
 *	jcClause	*clauseInfo;	文節情報
 *	struct wnn_env	*env;
 *    /-* private member *-/
 *	[ 省略 ]
 * } jcConvBuf;
 *
 * nClause は現在の文節数を表す。これは小文節の数である。
 * curClause はカレント小文節の番号である。
 * curLCStart と curLCEnd はカレント大文節の範囲を示す。curLCStart から
 * curLCEnd-1 の範囲の文節がカレント大文節である。つまり、curLCEnd は
 * 次の大文節の先頭の番号である。
 *
 * kanaBuf と displayBuf がかなバッファとディスプレイバッファである。
 * jcInsertChar() 等を使って入力された読みはかなバッファとディスプレイ
 * バッファに入る。これを変換すると、ディスプレイバッファの方だけが
 * 漢字の文字列になる。
 * kanaEnd および displayEnd はそれぞれのバッファに入れられた文字列の最後
 * の文字の次を指している。かなバッファ・ディスプレイバッファはどちらも
 * NULL ターミネートされない。
 *
 * clauseInfo は文節情報の入った配列である。これはあとで説明する。
 *
 * env はこの変換バッファの使用する環境である。
 *
 * <文節情報>
 *
 * 各文節の情報は clauseInfo という名前の jcClause 型の配列に入っている。
 * jcClause 型は jclib.h で次のように定義されている。
 *
 * typedef struct {
 *	wchar	*kanap;		読み文字列 (かなバッファの中を指す)
 *	wchar	*fzkp;		付属語の読み文字列 (かなバッファの中を指す)
 *	wchar	*dispp;		表示文字列 (ディスプレイバッファの中を指す)
 *	int	dicno;		辞書番号
 *	int	entry;		エントリ
 *	int	hinshi;		品詞番号
 *	int	kangovect;	前端の接続ベクトル
 *	char	conv;		変換済みか
 *				0: 未変換 1: 変換済 -1: jclibで疑似変換
 *	char	ltop;		大文節の先頭か?
 *	char	imabit;		今使ったよビット
 * } jcClause;
 *
 * kanap, fzkp はかなバッファ上の、その文節の読みの始まりと、付属語の
 * 始まりの位置を示すポインタである。また、dispp は、ディスプレイバッファ
 * 上で、その文節の始まりの位置を示す。従って、n 番の文節は、
 *	よみ:	clauseInfo[n].kanap から clauseInfo[n+1].kanap の前まで
 *	付属語:	clauseInfo[n].fzkp から clauseInfo[n+1].kanap の前まで
 *	    (付属語がなければ clauseInfo[n].fzkp == clauseInfo[n+1].kanap に
 *	     なる)
 *	漢字:	clauseInfo[n].dispp から clauseInfo[n+1].dispp の前まで
 * となる。このように n 番目の文節の範囲を示すのに n+1 番目の clauseInfo が
 * 必要なため、clauseInfo の配列の要素は常に先頭から文節数+1個が有効である。
 *
 * dicno, entry, hinshi, kangovect については Wnn のドキュメントを参照のこと。
 *
 * conv はその文節の変換状態を表す。0 なら未変換状態、1 なら変換状態、
 * -1 なら jcKana() によって疑似変換されたことを示す。
 *
 * ltop が 0 でなければその文節が大文節の先頭であることを示す。imabit は
 * その文節の幹語の今使ったよビットが入っている。
 *
 * kanap, dispp 等で、n 番目の文節の範囲を示すのに n+1 番目の文節情報が
 * 必要なため、clauseInfo の配列の要素は常に先頭から文節数+1個が有効である。
 * 文節数+1 個目の文節情報 (clauseInfo[nClause]) は
 *	kanap, dispp: それぞれ kanaEnd, displayEnd に等しい
 *	conv: 0 (未変換状態)
 *	ltop: 1
 *	残りのメンバは不定
 * である。
 *
 * 文節情報の kanap, fzkp, dispp を例を使って示しておく。
 *
 * 例文: これはデータ構造を示すための例文です (文節数 6)
 *
 * kanap:   ０    １    ２        ３    ４    ５          ６(=kanaEnd)
 *	    ↓    ↓    ↓        ↓    ↓    ↓          ↓
 * kanaBuf: これはでーたこうぞうをしめすためのれいぶんです
 *		↑      ↑      ↑    ↑    ↑        ↑
 * fzkp:	０      １      ２    ３    ４        ５
 *
 * dispp:      ０    １    ２    ３  ４    ５      ６(=displayEnd)
 *	       ↓    ↓    ↓    ↓  ↓    ↓      ↓
 * displayBuf: これはデータ構造を示すための例文です
 */

#ifndef lint
static char	*rcsid = "$Id: jclib.c,v 5.2 1992/12/24 04:43:05 ishisone Rel $";
#endif

#ifdef DEBUG_JCLIB
#include	<stdio.h>
#endif
#include	"commonhd.h"
#include	"jslib.h"
#include	"jclib.h"

#ifdef CHECK_PROTOTYPE
#include	"js.c.p"
#endif

#ifndef NULL
#define NULL	0
#endif

#define CHECKFIXED(buf)	\
	{ if ((buf)->fixed) { jcErrno = JE_ALREADYFIXED; return -1; } }
#define Free(p)		{if (p) free((char *)(p));}
#define DotSet(buf)	(buf)->dot = (buf)->clauseInfo[(buf)->curLCStart].kanap

#define KANABEG	0xa4a1	/* 'ぁ' */
#define KANAEND	0xa4f3	/* 'ん' */
#define KATAOFFSET	0x100	/* カタカナとひらがなのコード・オフセット */

/* デフォルトのバッファサイズ */
#define DEF_BUFFERSIZE	100	/* 100 文字 */
#define DEF_CLAUSESIZE	20	/* 20 文節 */
#define DEF_CANDSIZE	1024	/* 1K バイト */
#define DEF_RESETSIZE	10	/* 10 単語 */

/* buf->candKind の値 */
#define CAND_SMALL	0	/* 小文節候補 */
#define CAND_LARGE	1	/* 大文節候補 */

#define MAXFZK	LENGTHBUNSETSU

extern char	*malloc();
extern char	*realloc();
extern void	free();
#ifdef SVR4
extern char	*memmove();
extern char	*memset();
#define bcopy(p, q, l)	memmove(q, p, l)
#define bzero(p, l)	memset(p, 0, l)
#else
#if defined(SYSV) || defined(USG)
#define OVERLAP_BCOPY
extern char	*memset();
#define bzero(p, l)	memset(p, 0, l)
#else
extern int	bcopy();
extern int	bzero();
#endif
#endif

/*
 * 次候補バッファのデータタイプ (ライブラリの外からは見えないようにする)
 * buf->candBuf の指す先は、小文節候補の時は jcSCand の配列で、 大文節候補の
 * 時には jcLCand の配列である
 */
/* 小文節候補 */
typedef struct {
	wchar	*kanji;		/* 表示文字列 */
	int	kanalen;	/* かなの長さ */
	int	fzkoffset;
	int	dicno;
	int	entry;
	int	hinshi;
	int	kangovect;
	int	status;
#define	IMA_BIT		1	/* 今使ったよビット */
#define CONNECT_PREV	2	/* 前文節に接続できるか? */
#define CONNECT_NEXT	4	/* 後ろの文節に接続できるか? */
} jcSCand;

/* 大文節候補 */
typedef struct {
	int	nscand;		/* 含まれる小文節の数 */
	jcSCand	*scand;		/* 小文節の配列 */
} jcLCand;

/*
 * リセットバッファ (今使ったよビットを落す対象のエントリをのデータタイプ
 * (ライブラリの外からは見えないようにする)
 * buf->resetBuf の指す先は、jcEntry の配列である
 */
typedef struct {
	int	dicno;
	int	entry;
} jcEntry;

#ifdef __STDC__
/* ファンクションプロトタイプ宣言 */
static int wstrlen(wchar *);
static void setcinfo(jcClause *, struct wnn_sho_bunsetsu *,
		     wchar *, wchar *);
static void changecinfo(jcClause *, jcSCand *, wchar *, wchar *);
static int wstrlen(wchar *);
static void getFVec(jcClause *, int *, wchar *, int *);
static void getBVec(jcClause *, int *, int *, int *);
static void moveKBuf(jcConvBuf *, int, int);
static void moveDBuf(jcConvBuf *, int, int);
static void moveCInfo(jcConvBuf *, int, int);
static int resizeBuffer(jcConvBuf *, int);
static int resizeCInfo(jcConvBuf *, int);
static int resizeCandBuf(jcConvBuf *, int);
static void setCurClause(jcConvBuf *, int);
static int renConvert(jcConvBuf *, int);
static int tanConvert(jcConvBuf *, int);
static int doKanrenConvert(jcConvBuf *, int);
static int doKantanDConvert(jcConvBuf *, int, int);
static int doKantanSConvert(jcConvBuf *, int);
static int unconvert(jcConvBuf *, int, int);
static int expandOrShrink(jcConvBuf *, int, int, int);
static int getKanjiLenDbun(struct wnn_dai_bunsetsu *, int);
static int getKanjiLenSbun(struct wnn_sho_bunsetsu *, int);
static int getSCandidates(jcConvBuf *);
static int findSCand(jcSCand *, jcSCand *, struct wnn_sho_bunsetsu *);
static int setSCandData(jcConvBuf *, int, struct wnn_sho_bunsetsu *, int);
static int getSCandDataLen(jcClause *, int, struct wnn_sho_bunsetsu *);
static int getLCandidates(jcConvBuf *);
static int findLCand(jcLCand *, jcLCand *, struct wnn_dai_bunsetsu *);
static int setLCandData(jcConvBuf *, int, struct wnn_dai_bunsetsu *);
static int getLCandDataLen(jcClause *, jcClause *, int,
			   struct wnn_dai_bunsetsu *);
static int changeCand(jcConvBuf *, int);
static int setupCandBuf(jcConvBuf *, int);
static void checkAndResetCandidates(jcConvBuf *, int, int);
static void addResetClause(jcConvBuf *, int, int);
static void addResetCandidate(jcConvBuf *, int);
static void addResetEntry(jcConvBuf *, int, int);
static void saveDicAll(jcConvBuf *);
#else
static int wstrlen();
static void setcinfo();
static void changecinfo();
static int wstrlen();
static void getFVec();
static void getBVec();
static void moveKBuf();
static void moveDBuf();
static void moveCInfo();
static int resizeBuffer();
static int resizeCInfo();
static int resizeCandBuf();
static void setCurClause();
static int renConvert();
static int tanConvert();
static int doKanrenConvert();
static int doKantanDConvert();
static int doKantanSConvert();
static int unconvert();
static int expandOrShrink();
static int getKanjiLenDbun();
static int getKanjiLenSbun();
static int getSCandidates();
static int findSCand();
static int setSCandData();
static int getSCandDataLen();
static int getLCandidates();
static int findLCand();
static int setLCandData();
static int getLCandDataLen();
static int changeCand();
static int setupCandBuf();
static void checkAndResetCandidates();
static void addResetClause();
static void addResetCandidate();
static void addResetEntry();
static void saveDicAll();
#endif

/* エラー番号 */
int	jcErrno;

static struct wnn_ret_buf	jsbuf = { 0, NULL };

/*
 *	portability のためのファンクション
 */

#ifdef OVERLAP_BCOPY
#undef bcopy
static
bcopy(from, to, n)
register char *from;
register char *to;
register int n;
{
	if (n <= 0 || from == to) return;

	if (from < to) {
		from += n;
		to += n;
		while (n-- > 0)
			*--to = *--from;
	} else {
		while (n-- > 0)
			*to++ = *from++;
	}
}
#endif

/*
 *	jclib 内部で使われるファンクション
 */

/* wstrlen -- wchar 型文字列の strlen */
static int
wstrlen(s)
wchar *s;
{
	int	n = 0;

	while (*s++)
		n++;
	return n;
}

/* setcinfo -- clauesInfo に小文節の情報をセットする */
static void
setcinfo(clp, sbun, kanap, dispp)
jcClause *clp;
struct wnn_sho_bunsetsu *sbun;
wchar *kanap;
wchar *dispp;
{
	/* ltop フラグはとりあえず 0 にしておく */
	clp->conv = 1;
	clp->dicno = sbun->dic_no;
	clp->entry = sbun->entry;
	clp->hinshi = sbun->hinsi;
	clp->kangovect = sbun->kangovect;
	clp->ltop = 0;
	clp->kanap = kanap + sbun->start;
	clp->fzkp = kanap + sbun->jiriend + 1;
	clp->dispp = dispp;
	clp->imabit = sbun->ima;
}

/* changecinfo -- clauseInfo の情報を指定された候補のもので置き換える */
static void
changecinfo(clp, candp, kanap, dispp)
jcClause *clp;
jcSCand *candp;
wchar *kanap;
wchar *dispp;
{
	clp->kanap = kanap;
	clp->fzkp = clp->kanap + candp->fzkoffset;
	clp->dispp = dispp;
	clp->dicno = candp->dicno;
	clp->entry = candp->entry;
	clp->hinshi = candp->hinshi;
	clp->kangovect = candp->kangovect;
	clp->conv = 1;
	clp->imabit = (candp->status & IMA_BIT) != 0;
	clp->ltop = (candp->status & CONNECT_PREV) == 0;
}

/* getFVec -- 小文節の、前との接続を調べてパラメータを決める */
static void
getFVec(clp, hinship, fzk, connp)
jcClause *clp;
int *hinship;
wchar *fzk;
int *connp;
{
	wchar	*p, *q, *r;

	if (clp->ltop) {
		/* 大文節先頭 */
		*hinship = WNN_BUN_SENTOU;
		fzk[0] = 0;
	} else if ((clp - 1)->conv != 1) {
		/* 前の文節は変換されていない、あるいはかな変換されている */
		*hinship = WNN_ALL_HINSI;
		fzk[0] = 0;
	} else {
		*hinship = (clp - 1)->hinshi;
		if (connp) *connp |= CONNECT_PREV;
		/* 前の文節の付属語文字列をコピーする */
		p = fzk;
		q = (clp - 1)->fzkp;
		r = clp->kanap;
		while (q < r) {
			*p++ = *q++;
		}
		/* NULL ターミネートさせなければならない */
		*p = 0;
	}
}

/* getBVec -- 小文節の、後ろとの接続を調べてパラメータを決める */
static void
getBVec(clp, vecp, vec1p, connp)
jcClause *clp;
int *vecp;
int *vec1p;
int *connp;
{
#if WNN_VECT_KANTAN != WNN_VECT_KANZEN
	/* 同じ getBVec() を単文節変換と全候補取り出しに使うことができない */
	!! ERROR !!
#else
	if ((++clp)->ltop || clp->conv != 1) {
		*vecp = WNN_VECT_KANZEN;
		*vec1p = WNN_VECT_NO;
	} else {
		*vecp = clp->kangovect;
		*vec1p = WNN_VECT_KANZEN;
		if (connp) *connp |= CONNECT_NEXT;
	}
#endif
}

static int
getKanjiLenDbun(dbun, ndbun)
struct wnn_dai_bunsetsu *dbun;
int ndbun;
{
	struct wnn_sho_bunsetsu	*sbun;
	int	nsbun;
	int	nc = 0;

	while (ndbun-- > 0) {
		sbun = dbun->sbn;
		nsbun = dbun->sbncnt;
		while (nsbun-- > 0) {
			nc += wstrlen(sbun->kanji) + wstrlen(sbun->fuzoku);
			sbun++;
		}
		dbun++;
	}
	return nc;
}

static int
getKanjiLenSbun(sbun, nsbun)
struct wnn_sho_bunsetsu *sbun;
int nsbun;
{
	int	nc = 0;

	while (nsbun-- > 0) {
		nc += wstrlen(sbun->kanji) + wstrlen(sbun->fuzoku);
		sbun++;
	}

	return nc;
}

/* moveKBuf -- かなバッファの指定された文節の先頭からあとを動かす */
static void
moveKBuf(buf, cl, move)
jcConvBuf *buf;
int cl;
int move;
{
	jcClause	*clp = buf->clauseInfo + cl;
	jcClause	*clpend;
	int		movelen;

	if (move == 0) return;

	if ((movelen = buf->kanaEnd - clp->kanap) > 0) {
		/* かなバッファの内容を動かす */
		(void)bcopy((char *)clp->kanap, (char *)(clp->kanap + move),
			    movelen * sizeof(wchar));
	}

	/* かなバッファの変更に合わせて clauseInfo をアップデートする */
	clpend = buf->clauseInfo + buf->nClause;
	while (clp <= clpend) {
		clp->kanap += move;
		clp++->fzkp += move;
	}

	/* kanaEnd のアップデート */
	buf->kanaEnd += move;
}

/* moveDBuf -- ディスプレイバッファの指定された文節の先頭からあとを動かす */
static void
moveDBuf(buf, cl, move)
jcConvBuf *buf;
int cl;
int move;
{
	jcClause	*clp = buf->clauseInfo + cl;
	jcClause	*clpend;
	int		movelen;

	if (move == 0) return;

	if ((movelen = buf->displayEnd - clp->dispp) > 0) {
		/* ディスプレイバッファの内容を動かす */
		(void)bcopy((char *)clp->dispp, (char *)(clp->dispp + move),
			    movelen * sizeof(wchar));
	}

	/* ディスプレイバッファの変更に合わせて clauseInfo を
	 * アップデートする
	 */
	clpend = buf->clauseInfo + buf->nClause;
	while (clp <= clpend) {
		clp++->dispp += move;
	}

	/* displayEnd のアップデート */
	buf->displayEnd += move;
}

/* moveCInfo -- ClauseInfo の指定された文節の先頭からあとを動かす */
static void
moveCInfo(buf, cl, move)
jcConvBuf *buf;
int cl;
int move;
{
	jcClause	*clp = buf->clauseInfo + cl;
	int		len;

	/* move に正の数を指定すれば文節の挿入、負なら文節の削除になる */

	if (move == 0) return;

	if ((len = buf->nClause + 1 - cl) > 0) {
		(void)bcopy((char *)clp, (char *)(clp + move),
			    len * sizeof(jcClause));
	}
	buf->nClause += move;
	if (buf->candClause >= 0) {
		buf->candClause += move;
		buf->candClauseEnd += move;
	}
}

/* resizeBuffer -- かな/ディスプレイバッファの大きさを変える */
static int
resizeBuffer(buf, len)
jcConvBuf *buf;
int len;
{
	wchar	*kbufold, *dbufold;
	wchar	*kbufnew, *dbufnew;
	int	allocsize;
	jcClause	*clp, *clpend;

	kbufold = buf->kanaBuf;
	dbufold = buf->displayBuf;

	/* realloc する */
	allocsize = (len + 1) * sizeof(wchar);
	kbufnew = (wchar *)realloc((char *)kbufold, allocsize);
	dbufnew = (wchar *)realloc((char *)dbufold, allocsize);

	if (kbufnew == NULL || dbufnew == NULL) {
		Free(kbufnew);
		Free(dbufnew);
		jcErrno = JE_NOCORE;
		return -1;
	}

	buf->bufferSize = len;

	if (kbufnew == kbufold && dbufnew == dbufold) {
		/* ポインタは前と変わっていない */
		return 0;
	}

	/* 各種ポインタをつけ変える */

	buf->kanaBuf = kbufnew;
	buf->kanaEnd = kbufnew + (buf->kanaEnd - kbufold);
	buf->displayBuf = dbufnew;
	buf->displayEnd = dbufnew + (buf->displayEnd - dbufold);

	buf->dot = kbufnew + (buf->dot - kbufold);

	clp = buf->clauseInfo;
	clpend = clp + buf->nClause;
	while (clp <= clpend) {
		clp->kanap = kbufnew + (clp->kanap - kbufold);
		clp->fzkp = kbufnew + (clp->fzkp - kbufold);
		clp->dispp = dbufnew + (clp->dispp - dbufold);
		clp++;
	}

	return 0;
}

/* resizeCInfo -- clauseInfo バッファの大きさを変える */
static int
resizeCInfo(buf, size)
jcConvBuf *buf;
int size;
{
	jcClause	*cinfonew;

	/* realloc する */
	cinfonew = (jcClause *)realloc((char *)buf->clauseInfo,
				       (size + 1) * sizeof(jcClause));
	if (cinfonew == NULL) {
		jcErrno = JE_NOCORE;
		return -1;
	}

	buf->clauseSize = size;
	buf->clauseInfo = cinfonew;
	return 0;
}

/* resizeCandBuf -- 次候補バッファの大きさを変える */
static int
resizeCandBuf(buf, nbytes)
jcConvBuf *buf;
int nbytes;
{
	char	*p;

	if ((p = realloc((char *)buf->candBuf, nbytes)) == NULL) {
		jcErrno = JE_NOCORE;
		return -1;
	}

	buf->candSize = nbytes;
	buf->candBuf = p;
	return 0;
}

/* setCurClause -- カレント文節を設定する */
static void
setCurClause(buf, cl)
jcConvBuf *buf;
int cl;		/* カレント小文節番号 */
{
	jcClause	*clp = buf->clauseInfo;
	int		i;

	/* カレント小文節 */
	buf->curClause = cl;

	/* カレント大文節開始文節 */
	for (i = cl; i > 0 && !clp[i].ltop; i--)
		;
	buf->curLCStart = i;

	/* カレント大文節終了文節 (の次) */
	for (i = cl + 1; i <= buf->nClause && !clp[i].ltop; i++)
		;
	buf->curLCEnd = i;
}

/* renConvert -- カレント文節から後ろを連文節変換する */
static int
renConvert(buf, small)
jcConvBuf *buf;
int small;
{
	/* 連文節変換する */
	if (doKanrenConvert(buf,
			    small ? buf->curClause : buf->curLCStart) < 0) {
		return -1;
	}

	/*
	 * カレント文節の設定
	 * small が 0 なら、
	 *	カレント大文節の先頭は buf->curLCStart で変わらず
	 *	カレント大文節終りは ltop フラグをサーチして探す
	 *	カレント小文節はカレント大文節先頭に移動
	 * small が 0 でないなら、
	 *	カレント小文節は buf->curClause で変わらず
	 *	カレント大文節の先頭および終りは、カレント小文節の
	 *	前後を ltop フラグをサーチして探す
	 */
	setCurClause(buf, small ? buf->curClause : buf->curLCStart);

	/* ドットの設定 */
	DotSet(buf);

	return 0;
}

/* tanConvert -- カレント文節を単文節変換する */
static int
tanConvert(buf, small)
jcConvBuf *buf;
int small;
{
	/*
	 * 単文節変換の場合、基本的に 2 段階の処理を行なうことになる
	 * まず、カレント文節を単文節変換
	 * 次に、そのあとを連文節変換
	 */

	if (small) {
		/* まず単文節変換する */
		if (doKantanSConvert(buf, buf->curClause) < 0)
			return -1;

		/* カレント文節の設定
		 *	カレント小文節は buf->curClause で変わらず
		 *	カレント大文節の先頭と最後はカレント小文節の
		 *	前後に ltop フラグをサーチして探す
		 */
		setCurClause(buf, buf->curClause);
		/* ドットの設定 */
		DotSet(buf);

		/* 連文節変換 */
		if (buf->curClause + 1 < buf->nClause &&
		    buf->clauseInfo[buf->curClause + 1].conv == 0) {
			/* 小文節の単文節変換モードで、次の文節が
			 * 無変換だった場合、ltop フラグを 0 にして
			 * 前と接続できるようにする
			 */
			buf->clauseInfo[buf->curClause + 1].ltop = 0;
		}
		if (doKanrenConvert(buf, buf->curClause + 1) < 0)
			return -1;

		/* もう一度カレント文節の設定
		 * 連文節変換の結果によってはカレント大文節の最後が
		 * 移動することがある
		 */
		setCurClause(buf, buf->curClause);

		/* ドットは移動しないので再設定しなくてよい */
	} else {
		/* まず単文節変換する */
		if (doKantanDConvert(buf, buf->curLCStart, buf->curLCEnd) < 0)
			return -1;

		/* カレント文節の設定
		 *	カレント大文節の先頭は buf->curLCStart で変わらず
		 *	カレント大文節終りは ltop フラグをサーチして探す
		 *	カレント小文節はカレント大文節先頭に移動
		 */
		setCurClause(buf, buf->curLCStart);
		DotSet(buf);

		/* 連文節変換 */
		if (doKanrenConvert(buf, buf->curLCEnd) < 0)
			return -1;
		/* こちらは small の時と違って連文節変換の結果カレント文節が
		 * 移動することはない
		 */
	}

	return 0;
}

/* doKanrenConvert -- 指定された文節から後ろを連文節変換する */
static int
doKanrenConvert(buf, cl)
jcConvBuf *buf;
int cl;
{
	jcClause	*clp;
	int	hinshi;
	wchar	*kanap, *dispp;
	wchar	*wp;
	wchar	fzk[MAXFZK];
	struct wnn_dai_bunsetsu	*dbun;
	struct wnn_sho_bunsetsu	*sbun;
	int	ndbun, nsbun;
	int	len;
	int	i, j;

	/*
	 * 指定された文節から後ろを連文節変換する
	 * カレント文節の再設定などはしない
	 */

	if (cl >= buf->nClause) {
		/* 指定された文節はない
		 * エラーにはしない
		 * 空の文節を変換しようとした時に、それを事前にチェックして
		 * エラーにするのは上位の関数の責任である
		 */
		return 0;
	}

	clp = buf->clauseInfo + cl;

	/* 前の文節との接続を調べる */
	getFVec(clp, &hinshi, fzk, NULL);

	*(buf->kanaEnd) = 0;	/* NULL ターミネートさせておく */
	/* 連文節変換する */
	ndbun = js_kanren(buf->env, clp->kanap, hinshi, fzk,
			  WNN_VECT_KANREN, WNN_VECT_NO, WNN_VECT_BUNSETSU,
			  &jsbuf);

	if (ndbun < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	dbun = (struct wnn_dai_bunsetsu *)jsbuf.buf;

	/* とりあえず小文節の数を数える */
	nsbun = 0;
	for (i = 0; i < ndbun; i++) {
		nsbun += dbun[i].sbncnt;
	}

	/* clauseInfo のサイズのチェック */
	if (cl + nsbun > buf->clauseSize) {
		if (resizeCInfo(buf, cl + nsbun) < 0)
			return -1;
	}

	/* 次に変換文字列の長さのチェック */
	clp = buf->clauseInfo + cl;
	len = (clp->dispp - buf->displayBuf) + getKanjiLenDbun(dbun, ndbun);
	if (len > buf->bufferSize) {
		if (resizeBuffer(buf, len) < 0)
			return -1;
	}

	buf->nClause = cl + nsbun;

	/* では clauseInfo に変換結果を入れていく */
	clp = buf->clauseInfo + cl;
	kanap = clp->kanap;
	dispp = clp->dispp;
	for (i = 0; i < ndbun; i++) {
		int	connstate = dbun->sbn->status;

		sbun = dbun->sbn;
		nsbun = dbun->sbncnt;

		for (j = 0; j < nsbun; j++) {
			setcinfo(clp, sbun, kanap, dispp);

			/* ltop の設定 */
			clp->ltop = (j == 0 && connstate != WNN_CONNECT);

			/* ディスプレイバッファへの変換文字列のコピー */
			/* 自立語部分 */
			wp = sbun->kanji;
			while (*dispp++ = *wp++)
				;
			dispp--;
			/* 付属語部分 */
			wp = sbun->fuzoku;
			while (*dispp++ = *wp++)
				;
			dispp--;

			sbun++;
			clp++;
		}
		dbun++;
	}

	/* 最後の clauseInfo の設定 */
	clp->kanap = buf->kanaEnd;
	clp->dispp = buf->displayEnd = dispp;
	clp->conv = 0;
	clp->ltop = 1;

	return 0;
}

/* doKantanDConvert -- 指定された範囲の文節を大文節として単文節変換する */
static int
doKantanDConvert(buf, cls, cle)
jcConvBuf *buf;
int cls;
int cle;
{
	jcClause	*clps, *clpe;
	int	len, diff, newlen;
	int	cldiff, nclausenew;
	int	hinshi;
	wchar	*kanap, *dispp;
	wchar	*wp;
	wchar	fzk[MAXFZK];
	wchar	savechar;
	wchar	*savep;
	struct wnn_dai_bunsetsu	*dbun;
	struct wnn_sho_bunsetsu	*sbun;
	int	ndbun, nsbun;
	int	i;

	/*
	 * 指定された範囲の文節を大文節として単文節変換する
	 * カレント文節の再設定などはしない
	 */

	clps = buf->clauseInfo + cls;
	clpe = buf->clauseInfo + cle;
	/* 前の文節との接続を調べる */
	getFVec(clps, &hinshi, fzk, NULL);

	/* 読みを NULL ターミネートする
	 * 単に 0 を入れると次の文節が壊れるので、その前にセーブしておく
	 */
	savep = clpe->kanap;
	savechar = *savep;
	*savep = 0;

	/* 単文節変換する */
	ndbun = js_kantan_dai(buf->env, clps->kanap, hinshi, fzk,
			      WNN_VECT_KANTAN, WNN_VECT_NO, &jsbuf);
	/* すかさずセーブしてあった文字をもとに戻す */
	*savep = savechar;
	if (ndbun < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	dbun = (struct wnn_dai_bunsetsu *)jsbuf.buf;
	sbun = dbun->sbn;
	nsbun = dbun->sbncnt;

	cldiff = nsbun - (cle - cls);
	nclausenew = buf->nClause + cldiff;
	/* clauseInfo のサイズのチェック */
	if (nclausenew > buf->clauseSize) {
		if (resizeCInfo(buf, nclausenew) < 0)
			return -1;
	}

	/* 変換文字列の長さのチェック */
	len = getKanjiLenDbun(dbun, 1);
	diff = len - (clpe->dispp - clps->dispp);
	newlen = (buf->displayEnd - buf->displayBuf) + diff;
	if (newlen > buf->bufferSize) {
		if (resizeBuffer(buf, newlen) < 0)
			return -1;
	}

	/* 文節を挿入するので、ディスプレイバッファの内容を移動させる */
	/* どうせあとから連文節変換するからいいではないかという考え方もあるが、
	 * どこでエラーが起こっても一応の consistency が保たれるように
	 * するというのが目標である
	 */
	moveDBuf(buf, cle, diff);

	/* clauseInfo を動かす (同時に nClause もアップデートされる) */
	moveCInfo(buf, cle, cldiff);

	/* では clauseInfo に変換結果を入れる */
	clps = buf->clauseInfo + cls;
	kanap = clps->kanap;
	dispp = clps->dispp;
	for (i = 0; i < nsbun; i++) {
		setcinfo(clps, sbun, kanap, dispp);

		clps->ltop = i == 0 && sbun->status != WNN_CONNECT;

		/* ディスプレイバッファへの変換文字列のコピー */
		/* 自立語部分 */
		wp = sbun->kanji;
		while (*dispp++ = *wp++)
			;
		dispp--;
		/* 付属語部分 */
		wp = sbun->fuzoku;
		while (*dispp++ = *wp++)
			;
		dispp--;

		clps++;
		sbun++;
	}

	/* 次の clauseInfo の設定 */
	clps->ltop = 1;

	return 0;
}

/* doKantanSConvert -- 指定された文節を小文節として単文節変換する */
static int
doKantanSConvert(buf, cl)
jcConvBuf *buf;
int cl;
{
	jcClause	*clp;
	int	len, newlen, diff;
	int	hinshi;
	wchar	*dispp;
	wchar	fzk[MAXFZK];
	wchar	*wp;
	wchar	savechar;
	wchar	*savep;
	int	vec, vec1;
	struct wnn_sho_bunsetsu	*sbun;
	int	nsbun;

	/*
	 * 指定された文節を小文節として単文節変換する
	 * カレント文節の再設定などはしない
	 */

	clp = buf->clauseInfo + cl;

	/* 前の文節との接続を調べる */
	getFVec(clp, &hinshi, fzk, NULL);
	/* 後ろの小文節との接続を調べる */
	getBVec(clp, &vec, &vec1, NULL);

	/* 読みを NULL ターミネートする
	 * 単に 0 を入れると次の文節が壊れるので、その前にセーブしておく
	 */
	savep = (clp + 1)->kanap;
	savechar = *savep;
	*savep = 0;

	/* 単文節変換する */
	nsbun = js_kantan_sho(buf->env, clp->kanap, hinshi, fzk,
			      vec, vec1, &jsbuf);
	/* すかさずセーブしてあった文字をもとに戻す */
	*savep = savechar;
	if (nsbun < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	sbun = (struct wnn_sho_bunsetsu *)jsbuf.buf;

	/* 変換文字列の長さのチェック */
	clp = buf->clauseInfo + cl;
	len = getKanjiLenSbun(sbun, 1);
	diff = len - ((clp + 1)->dispp - clp->dispp);
	newlen = (buf->displayEnd - buf->displayBuf) + diff;
	if (newlen > buf->bufferSize) {
		if (resizeBuffer(buf, newlen) < 0)
			return -1;
	}

	/* 文節を挿入するので、ディスプレイバッファの内容を移動させる */
	/* どうせあとから連文節変換するからいいではないかという考え方もあるが、
	 * どこでエラーが起こっても一応の consistency が保たれるように
	 * するというのが目標である
	 */
	moveDBuf(buf, cl + 1, diff);

	/* では clauseInfo に変換結果を入れる */
	clp = buf->clauseInfo + cl;
	dispp = clp->dispp;

	setcinfo(clp, sbun, clp->kanap, dispp);
	clp->ltop = sbun->status != WNN_CONNECT;

	/* ディスプレイバッファへの変換文字列のコピー */
	/* 自立語部分 */
	wp = sbun->kanji;
	while (*dispp++ = *wp++)
		;
	dispp--;
	/* 付属語部分 */
	wp = sbun->fuzoku;
	while (*dispp++ = *wp++)
		;

	/* 次の clauseInfo の設定 */
	(++clp)->ltop = sbun->status_bkwd == WNN_NOT_CONNECT_BK ||
	    vec == WNN_VECT_KANTAN;

	return 0;
}

/* unconvert -- 指定された範囲の文節を一つの無変換の文節にする */
static int
unconvert(buf, start, end)
jcConvBuf *buf;
int start;
int end;
{
	jcClause	*clps, *clpe;
	int	diff, len;

	if (end <= start)
		return 0;

	if (start >= buf->nClause)
		return 0;

	clps = buf->clauseInfo + start;
	clpe = buf->clauseInfo + end;

	/*
	 * ディスプレイバッファの内容をカナバッファの内容で置き換える
	 * …といっても実際の動作はそれほど簡単ではない
	 *
	 * ・まず、置き換えた結果、ディスプレイバッファがあふれないか調べ、
	 *   あふれるようならバッファのサイズを大きくする
	 * ・ディスプレイバッファに、かなバッファからデータを移す
	 * ・clauseInfo を書き換えて、start から end-1 までの文節を
	 *   一つの無変換の文節にまとめる
	 * ・もちろん nClause も変える
	 * ・start+1 から最後までの文節の clauseInfo の dispp を
	 *   ディスプレイバッファのずれに応じて調整する
	 *
	 * その他に次のことも行なう必要があるが、この関数ではやらない
	 * 上位の関数で設定すること
	 * ・大文節フラグ (ltop) の設定
	 * ・カレント文節、および次候補文節の移動
	 *   次候補文節が無変換の文節になってしまった時の処理
	 * ・ドットの移動
	 */

	/* 読みの長さと漢字の長さの差を調べる */
	diff = (clpe->kanap - clps->kanap) - (clpe->dispp - clps->dispp);
	/* 置き換えた場合のディスプレイバッファの長さ */
	len = (buf->displayEnd - buf->displayBuf) + diff;
	/* バッファのサイズが足りなければサイズを大きくする */
	if (len > buf->bufferSize) {
		if (resizeBuffer(buf, len) < 0) {
			/* サイズが変えられなかった */
			return -1;
		}
	}

	/* 置き換え */
	/* まず後ろの部分を動かしてから */
	moveDBuf(buf, end, diff);
	/* 読みを入れる */
	(void)bcopy((char *)clps->kanap, (char *)clps->dispp,
		    (clpe->kanap - clps->kanap) * sizeof(wchar));

	/*
	 * start から end までの文節を一つにまとめる
	 */

	/* 無変換状態になった文節の clauseInfo の設定 */
	clps->conv = 0;

	/* end からあとの clauseInfo を'つめる' */
	moveCInfo(buf, end, start + 1 - end);

	return 0;
}

static int
expandOrShrink(buf, small, expand, convf)
jcConvBuf *buf;
int small;
int expand;
int convf;
{
	jcClause	*clp, *clpe;
	int	start, end;

	start = small ? buf->curClause : buf->curLCStart;
	end = small ? start + 1 : buf->curLCEnd;

	clp = buf->clauseInfo + start;
	clpe = buf->clauseInfo + end;

	/*
	 * 伸び縮みできるかのチェック
	 */
	if (expand) {
		/*
		 * カレント文節が最後の文節の時には
		 * もう広げられない
		 */
		if (end >= buf->nClause) {
			jcErrno = JE_CANTEXPAND;
			return -1;
		}
	} else {
		if (buf->curClause == buf->nClause ||
		    clpe->kanap - clp->kanap <= 1) {
			/* カレント文節が空か、あるいは長さが１以下 */
			jcErrno = JE_CANTSHRINK;
			return -1;
		}
	}

	/* カレント文節およびその次の単語をリセットリストに入れる */
	addResetClause(buf, start, (end >= buf->nClause) ? end : end + 1);

	/* 候補文節がカレント文節の後ろにあれば無効にする */
	checkAndResetCandidates(buf, start, buf->nClause);

	/* カレント文節が変換されていれば、とりあえず無変換にする */
	if (clp->conv) {
		if (unconvert(buf, start, end) < 0)
			return -1;
	}

	/* カレント文節の再設定 */
	if (small) {
		buf->curClause = start;
		buf->curLCEnd = start + 1;
	} else {
		buf->curClause = buf->curLCStart = start;
		buf->curLCEnd = start + 1;
	}
	DotSet(buf);
	buf->clauseInfo[start + 1].ltop = 1;

	/* さらにカレント文節の後ろを無変換にする
	 * この時にはすでに start から end までの文節が一つの
	 * 無変換の文節にまとめられていることに注意
	 */
	if (unconvert(buf, start + 1, buf->nClause) < 0)
		return -1;

	if (small) {
		if (start + 1 < buf->nClause)
			buf->clauseInfo[start + 1].ltop = 0;
		buf->curLCEnd = buf->nClause;
	}
	clp = buf->clauseInfo + start;
	/*
	 * カレント文節の長さを１文字伸び縮みさせる
	 */
	if (expand) {
		/* カレント文節の長さを伸ばす */
		(++clp)->kanap++;
		clp->dispp++;
		/* 広げた結果、後ろの文節がなくなることがある */
		if (clp->kanap == buf->kanaEnd) {
			buf->nClause--;
			clp->ltop = 1;
		}
	} else {
		/* カレント文節の長さを縮める */
		(++clp)->kanap--;
		clp->dispp--;
		/* カレント文節が最後の文節だった場合には、１文節増える */
		if (start == buf->nClause - 1) {
			if (buf->nClause >= buf->clauseSize) {
				if (resizeCInfo(buf, buf->nClause + 1) < 0) {
					/* 変更した所を元に戻す */
					clp = buf->clauseInfo + start + 1;
					clp->kanap++;
					clp->dispp--;
					return -1;
				}
			}

			/* 上で clauseInfo が realloc されたかもしれないので
			 * clp を再び設定
			 */
			clp = buf->clauseInfo + buf->nClause;

			/* 新しくできた文節の clauseInfo の設定
			 * kanap, dispp はすでに上でセットされている
			 */
			clp->ltop = small ? 0 : 1;
			clp->conv = 0;

			/* 最後の文節の次の (終端の) clauseInfo の設定 */
			(++clp)->kanap = buf->kanaEnd;
			clp->dispp = buf->displayEnd;
			clp->conv = 0;

			buf->nClause++;
		}
	}

	if (convf) {
		return tanConvert(buf, small);
	}
	return 0;
}

/* getSCandidates -- カレント小文節の全候補を取り出す */
static int
getSCandidates(buf)
jcConvBuf *buf;
{
	wchar	savechar;
	wchar	*savep;
	int	hinshi;
	wchar	fzk[MAXFZK];
	int	vec, vec1;
	int	ncand;
	jcClause	*clp;
	int	conn = 0;

	clp = buf->clauseInfo + buf->curClause;

	/* 前との接続を調べる */
	getFVec(clp, &hinshi, fzk, &conn);
	/* 後ろとの接続を調べる */
	getBVec(clp, &vec, &vec1, &conn);

	/* 読みを NULL ターミネートしておく */
	savep = (clp + 1)->kanap;
	savechar = *savep;
	*savep = 0;

	/* 全候補を取り出す */
	ncand = js_kanzen_sho(buf->env, clp->kanap, hinshi, fzk,
			  vec, vec1, &jsbuf);
	/* すかさず読みをもとに戻す */
	*savep = savechar;
	if (ncand < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	if (setSCandData(buf, ncand, (struct wnn_sho_bunsetsu *)jsbuf.buf,
			 conn) < 0)
		return -1;

	/* 候補文節の情報を入れる */
	buf->curCand = 0;
	buf->candKind = CAND_SMALL;
	buf->candClause = buf->curClause;
	buf->candClauseEnd = buf->curClause + 1;	/* 念のため */

	return 0;
}

/* getLCandidates -- カレント大文節の全候補を取り出す */
static int
getLCandidates(buf)
jcConvBuf *buf;
{
	wchar	savechar;
	wchar	*savep;
	int	ncand;
	jcClause	*clps, *clpe;

	clps = buf->clauseInfo + buf->curLCStart;
	clpe = buf->clauseInfo + buf->curLCEnd;

	/* 読みを NULL ターミネートしておく */
	savep = clpe->kanap;
	savechar = *savep;
	*savep = 0;

	/* 全候補を取り出す */
	ncand = js_kanzen_dai(buf->env, clps->kanap, WNN_BUN_SENTOU, NULL,
			      WNN_VECT_KANZEN, WNN_VECT_NO, &jsbuf);
	/* すかさず読みをもとに戻す */
	*savep = savechar;
	if (ncand < 0) {
		jcErrno = JE_WNNERROR;
		return -1;
	}

	if (setLCandData(buf, ncand, (struct wnn_dai_bunsetsu *)jsbuf.buf) < 0)
		return -1;

	/* 候補文節の情報を入れておく */
	buf->curCand = 0;
	buf->candKind = CAND_LARGE;
	buf->candClause = buf->curLCStart;
	buf->candClauseEnd = buf->curLCEnd;

	return 0;
}

/* findSCand -- 同じ候補がないかどうか調べる (小文節候補) */
static int
findSCand(scps, scpe, sbun)
jcSCand *scps;
jcSCand *scpe;
struct wnn_sho_bunsetsu *sbun;
{
	int	kangovect = sbun->kangovect;
	int	hinshi = sbun->hinsi;
	wchar	c = sbun->kanji[0];
	wchar	*p, *q;

	while (scps < scpe) {
		/* 品詞と、前への接続ベクトルと、先頭の１文字が同じかどうかで
		 * ふるいにかける
		 * jllib のソースを見ると kangovect の比較はコメントアウト
		 * されているが、なぜだろう?
		 */
		if (scps->hinshi == hinshi &&
		    scps->kangovect == kangovect &&
		    scps->kanji[0] == c) {
			/* 字面が同じか調べる */
			p = scps->kanji;
			q = sbun->kanji;
			while (*q) {
				if (*p++ != *q++) goto next;
			}
			q = sbun->fuzoku;
			while (*q) {
				if (*p++ != *q++) goto next;
			}
			if (*p) goto next;

			/* 同じ */
			return 1;
		}
	next:
		scps++;
	}
	return 0;
}

/* findSCand -- 同じ候補がないかどうか調べる (大文節候補) */
static int
findLCand(lcps, lcpe, dbun)
jcLCand *lcps;
jcLCand *lcpe;
struct wnn_dai_bunsetsu *dbun;
{
	struct wnn_sho_bunsetsu	*sbun;
	jcSCand	*scps, *scpe;
	int	nsbun = dbun->sbncnt;
	wchar	*p, *q;

	while (lcps < lcpe) {
		/* とりあえず小文節数でふるいにかける */
		if (lcps->nscand != nsbun) goto next;

		scps = lcps->scand;
		scpe = scps + nsbun;
		sbun = dbun->sbn;
		while (scps < scpe) {
			/* 品詞と、前への接続ベクトルと、先頭の１文字が
			 * 同じかどうかでふるいにかける
			 * jllib のソースを見ると kangovect の比較は
			 * コメントアウトされているが、なぜだろう?
			 */
			if (scps->hinshi == sbun->hinsi &&
			    scps->kangovect == sbun->kangovect &&
			    scps->kanji[0] == sbun->kanji[0]) {
				/* 字面が同じか調べる */
				p = scps->kanji;
				q = sbun->kanji;
				while (*q) {
					if (*p++ != *q++) goto next;
				}
				q = sbun->fuzoku;
				while (*q) {
					if (*p++ != *q++) goto next;
				}
				if (*p) goto next;
				
				scps++;
				sbun++;
			} else {
				goto next;	/* 違う */
			}
		}
		/* 同じだった */
		return 1;

	next:
		lcps++;
	}
	return 0;
}

/* setSCandData -- 取り出したカレント小文節の全候補を格納する */
static int
setSCandData(buf, ncand, sbun, conn)
jcConvBuf *buf;
int ncand;
struct wnn_sho_bunsetsu *sbun;
int conn;
{
	jcClause	*clp;
	jcSCand *scandp, *scp;
	wchar	*sp, *spend;
	wchar *wp;
	int	bytesneeded;
	int	i;

	clp = buf->clauseInfo + buf->curClause;

	/* buf->candBuf に必要な大きさを調べる */
	bytesneeded = getSCandDataLen(clp, ncand, sbun);
	if (bytesneeded > buf->candSize &&
	    resizeCandBuf(buf, bytesneeded) < 0) {
		return -1;
	}

	scandp = scp = (jcSCand *)buf->candBuf;
	wp = (wchar *)(scandp + (ncand + 1));

	/* まず、次候補の最初に、現在の小文節の内容を入れる */
	scp->kanji = wp;
	scp->kanalen = (clp + 1)->kanap - clp->kanap;
	scp->fzkoffset = clp->fzkp - clp->kanap;
	scp->dicno = clp->dicno;
	scp->entry = clp->entry;
	scp->hinshi = clp->hinshi;
	scp->kangovect = clp->kangovect;
	scp->status = clp->imabit ? IMA_BIT : 0;
	if (!clp->ltop) scp->status |= CONNECT_PREV;
	if (!(clp + 1)->ltop) scp->status |= CONNECT_NEXT;
	scp++;
	sp = clp->dispp;
	spend = (clp + 1)->dispp;
	while (sp < spend)
		*wp++ = *sp++;
	*wp++ = 0;

	/* 次に全候補の情報を入れる */
	for (i = 0; i < ncand; i++, sbun++) {
		if (findSCand(scandp, scp, sbun)) {
			continue;
		}
		scp->kanji = wp;
		scp->kanalen = sbun->end - sbun->start + 1;
		scp->fzkoffset = sbun->jiriend - sbun->start + 1;
		scp->dicno = sbun->dic_no;
		scp->entry = sbun->entry;
		scp->hinshi = sbun->hinsi;
		scp->kangovect = sbun->kangovect;
		scp->status = sbun->ima ? IMA_BIT : 0;
		/* 小文節の全候補取り出しの時には、前後の接続を見る
		 * 必要がある
		 */
		if ((conn & CONNECT_PREV) && sbun->status == WNN_CONNECT) {
			scp->status |= CONNECT_PREV;
		}
		if ((conn & CONNECT_NEXT) &&
		    sbun->status_bkwd == WNN_CONNECT_BK) {
			scp->status |= CONNECT_NEXT;
		}
		sp = sbun->kanji;
		while (*wp++ = *sp++)
			;
		wp--;
		sp = sbun->fuzoku;
		while (*wp++ = *sp++)
			;
		scp++;
	}

	/* 実際に candBuf に入れられた候補の数 */
	buf->nCand = scp - scandp;

	return 0;
}

/* setLCandData -- 取り出したカレント大文節の全候補を格納する */
static int
setLCandData(buf, ncand, dbun)
jcConvBuf *buf;
int ncand;
struct wnn_dai_bunsetsu *dbun;
{
	jcClause	*clp, *clps, *clpe;
	jcLCand *lcandp, *lcp;
	jcSCand *scandp, *scp;
	struct wnn_sho_bunsetsu *sbun;
	wchar	*sp, *spend;
	wchar *wp;
	int	nscand;
	int	bytesneeded;
	int	i, j;

	clps = buf->clauseInfo + buf->curLCStart;
	clpe = buf->clauseInfo + buf->curLCEnd;

	/* buf->candBuf に必要な大きさを調べる */
	bytesneeded = getLCandDataLen(clps, clpe, ncand, dbun);
	if (bytesneeded > buf->candSize &&
	    resizeCandBuf(buf, bytesneeded) < 0) {
		return -1;
	}

	/* 小文節の数を数える */
	nscand = 0;
	for (i = 0; i < ncand; i++) {
		nscand += dbun[i].sbncnt;
	}
	nscand += buf->curLCEnd - buf->curLCStart;
	lcandp = lcp = (jcLCand *)buf->candBuf;
	scandp = scp = (jcSCand *)(lcandp + (ncand + 1));
	wp = (wchar *)(scandp + nscand);

	/* まず、次候補の最初に、現在の大文節の内容を入れる */
	lcp->nscand = buf->curLCEnd - buf->curLCStart;
	lcp++->scand = scp;
	clp = clps;
	while (clp < clpe) {
		scp->kanji = wp;
		scp->kanalen = (clp + 1)->kanap - clp->kanap;
		scp->fzkoffset = clp->fzkp - clp->kanap;
		scp->dicno = clp->dicno;
		scp->entry = clp->entry;
		scp->hinshi = clp->hinshi;
		scp->kangovect = clp->kangovect;
		scp->status = clp->imabit ? IMA_BIT : 0;
		if (clp != clps) scp->status |= CONNECT_PREV;
		if (clp != clpe - 1) scp->status |= CONNECT_NEXT;
		sp = clp->dispp;
		spend = (clp + 1)->dispp;
		while (sp < spend)
			*wp++ = *sp++;
		*wp++ = 0;
		scp++;
		clp++;
	}

	/* 次に全候補の情報を入れる */
	for (i = 0; i < ncand; i++, dbun++) {
		if (findLCand(lcandp, lcp, dbun)) {
			continue;
		}
		sbun = dbun->sbn;
		lcp->nscand = dbun->sbncnt;
		lcp->scand = scp;
		for (j = 0; j < dbun->sbncnt; j++) {
			scp->kanji = wp;
			scp->kanalen = sbun->end - sbun->start + 1;
			scp->fzkoffset = sbun->jiriend - sbun->start + 1;
			scp->dicno = sbun->dic_no;
			scp->entry = sbun->entry;
			scp->hinshi = sbun->hinsi;
			scp->kangovect = sbun->kangovect;
			scp->status = sbun->ima ? IMA_BIT : 0;
			/* 前後の文節との接続情報を入れる
			 * 大文節の単文節変換なので、
			 * 最初の小文節以外は前につながるし、
			 * 最後の小文節以外は後ろにつながる
			 */
			if (j != 0) {
				scp->status |= CONNECT_PREV;
			}
			if (j != dbun->sbncnt - 1) {
				scp->status |= CONNECT_NEXT;
			}
			sp = sbun->kanji;
			while (*wp++ = *sp++)
				;
			wp--;
			sp = sbun->fuzoku;
			while (*wp++ = *sp++)
				;
			scp++;
			sbun++;
		}
		lcp++;
	}

	/* 実際に candBuf に入れられた候補の数 */
	buf->nCand = lcp - lcandp;

	return 0;
}

/* getSCandDataLen -- 全候補の情報を入れるのに必要なメモリの量を調べる (小文節) */
static int
getSCandDataLen(clp, ncand, sbun)
jcClause *clp;
int ncand;
struct wnn_sho_bunsetsu *sbun;
{
	int	nbyte = 0;
	int	i;

	/* 現在表示されている文節も全候補に含めるので、
	 * ncand + 1 個ぶん必要
	 */
	nbyte = sizeof(jcSCand) * (ncand + 1);

	nbyte += (((clp + 1)->dispp - clp->dispp) + 1) * sizeof(wchar);
	for (i = 0; i < ncand; i++, sbun++) {
		nbyte += (wstrlen(sbun->kanji) + wstrlen(sbun->fuzoku) + 1) *
		    sizeof(wchar);
	}

	return nbyte;
}

/* getLCandDataLen -- 全候補の情報を入れるのに必要なメモリの量を調べる (大文節) */
static int
getLCandDataLen(clps, clpe, ncand, dbun)
jcClause *clps;
jcClause *clpe;
int ncand;
struct wnn_dai_bunsetsu *dbun;
{
	int	ndbun;
	int	nsbun;
	int	nchar;
	int	i;

	/* 大文節の数 */
	ndbun = ncand;

	/* 小文節の数 */
	nsbun = 0;
	for (i = 0; i < ncand; i++) {
		nsbun += dbun[i].sbncnt;
	}

	/* 漢字の長さ */
	nchar = 0;
	for (i = 0; i < ncand; i++) {
		int	j = dbun[i].sbncnt;
		struct wnn_sho_bunsetsu *sbun = dbun[i].sbn;

		while (j-- > 0) {
			nchar += wstrlen(sbun->kanji) +
				wstrlen(sbun->fuzoku) + 1;
			sbun++;
		}
	}

	/* 現在表示されている文節も全候補に含める */
	ndbun += 1;
	nsbun += clpe - clps;
	nchar += (clpe->dispp - clps->dispp) + (clpe - clps);

	return ndbun * sizeof(jcLCand) + nsbun * sizeof(jcSCand) +
	    nchar * sizeof(wchar);
}

/* changeCand -- カレント文節を指定された番号の候補で置き換える */
static int
changeCand(buf, n)
jcConvBuf *buf;
int n;
{
	int	buflen, oldlen, newlen;
	int	oldclen, newclen;
	wchar	*p;
	wchar	*kanap, *dispp;

	if (buf->candKind == CAND_SMALL) {
		jcSCand	*candp = ((jcSCand *)buf->candBuf) + n;
		jcClause	*clp = buf->clauseInfo + buf->curClause;
		/* カレント小文節を変える */
		/* ディスプレイバッファのサイズのチェック */
		newlen = wstrlen(candp->kanji);
		oldlen = (clp + 1)->dispp - clp->dispp;
		buflen = (buf->displayEnd - buf->displayBuf) + newlen - oldlen;
		if (buflen > buf->bufferSize &&
		    resizeBuffer(buf, buflen) < 0) {
			return -1;
		}
		/* ディスプレイバッファを動かす */
		moveDBuf(buf, buf->curClause + 1, newlen - oldlen);
		/* 候補文字列をコピーする */
		(void)bcopy((char *)candp->kanji, (char *)clp->dispp,
			    newlen * sizeof(wchar));
		/* clauseInfo のアップデート */
		changecinfo(clp, candp, clp->kanap, clp->dispp);
		/* 後ろの文節の ltop の設定 */
		(clp + 1)->ltop = !(candp->status & CONNECT_NEXT);
		/* カレント大文節の再設定 */
		setCurClause(buf, buf->curClause);
	} else {
		jcLCand	*candp = ((jcLCand *)buf->candBuf) + n;
		jcSCand	*scandp;
		jcClause	*clps = buf->clauseInfo + buf->curLCStart;
		jcClause	*clpe = buf->clauseInfo + buf->curLCEnd;
		int	i;

		/* カレント大文節を変える */

		/* ディスプレイバッファのサイズのチェック */
		newlen = 0;
		scandp = candp->scand;
		for (i = 0; i < candp->nscand; i++) {
			newlen += wstrlen(scandp++->kanji);
		}
		oldlen = clpe->dispp - clps->dispp;
		buflen = (buf->displayEnd - buf->displayBuf) + newlen - oldlen;
		if (buflen > buf->bufferSize &&
		    resizeBuffer(buf, buflen) < 0) {
			return -1;
		}

		/* caluseInfo のサイズのチェック */
		oldclen = buf->curLCEnd - buf->curLCStart;
		newclen = candp->nscand;
		if (buf->nClause + newclen - oldclen > buf->clauseSize &&
		    resizeCInfo(buf, buf->nClause + newclen - oldclen) < 0) {
			return -1;
		}

		/* ディスプレイバッファを動かす */
		moveDBuf(buf, buf->curLCEnd, newlen - oldlen);

		/* clauseInfo を動かす */
		moveCInfo(buf, buf->curLCEnd, newclen - oldclen);

		/* 候補文字列をコピーし、同時に clauseInfo を
		 * アップデートする
		 */
		clps = buf->clauseInfo + buf->curLCStart;
		scandp = candp->scand;
		kanap = clps->kanap;
		dispp = clps->dispp;
		for (i = 0; i < candp->nscand; i++) {
			changecinfo(clps, scandp, kanap, dispp);
			clps++->ltop = (i == 0);
			kanap += scandp->kanalen;
			p = scandp++->kanji;
			while (*p)
				*dispp++ = *p++;
		}

		/* カレント文節のセット */
		setCurClause(buf, buf->curLCStart);

		/* 候補文節のセット
		 * この入れ換えによって、カレント大文節の終りが
		 * 移動することがあるので、ここで候補文節を
		 * 再設定する
		 */
		buf->candClause = buf->curLCStart;
		buf->candClauseEnd = buf->curLCEnd;
	}
	return 0;
}

/* setupCandBuf -- 次候補バッファに次候補の情報を用意する
 *		   既にセットされていれば何もしない
 */
static int
setupCandBuf(buf, small)
jcConvBuf *buf;
int small;
{
	if (small) {
		if (buf->candKind != CAND_SMALL ||
		    buf->curClause != buf->candClause) {
			/* カレント小文節の候補を取り出す */
			buf->candClause = -1;
			if (getSCandidates(buf) < 0)
				return -1;
		}
	} else {
		if (buf->candKind != CAND_LARGE ||
		    buf->curLCStart != buf->candClause ||
		    buf->curLCEnd != buf->candClauseEnd) {
			/* カレント大文節の候補を取り出す */
			buf->candClause = -1;
			if (getLCandidates(buf) < 0)
				return -1;
		}
	}

	return 0;
}

/* checkAndResetCandidates -- 次候補バッファの内容が有効かどうかチェックして、
 *			      必要な処理を行なう
 */
static void
checkAndResetCandidates(buf, cls, cle)
jcConvBuf *buf;
int cls;
int cle;
{
	/* 文節番号 cls から cle - 1 までの文節が変更される
	 * 次候補バッファにはいっている候補文節がこの中に含まれていれば
	 * 次候補バッファの内容を無効にしなくてはならない
	 *
	 * どのような場合かというと、
	 * 1. buf->candKind が CAND_SMALL で、
	 *	cls <= buf->candClause < cle
	 * 2. buf->candKind が CAND_LARGE で、
	 *	buf->candClause < cle かつ cls < buf->candClauseEnd 
	 */
	if (buf->candKind == CAND_SMALL)
		buf->candClauseEnd = buf->candClause + 1;
	if (buf->candClause < cle && cls < buf->candClauseEnd) {
		/* 無効にする */
		buf->candClause = buf->candClauseEnd = -1;
	}
}

/* addResetClause -- 指定された文節の単語をリセットエントリのリストに入れる */
static void
addResetClause(buf, cls, cle)
jcConvBuf *buf;
int cls;
int cle;
{
	jcClause	*clp;

	for (clp = buf->clauseInfo + cls; cls < cle; cls++, clp++) {
		if (clp->conv == 1 && clp->imabit) {
			addResetEntry(buf, clp->dicno, clp->entry);
		}
	}
}

/* addResetCandidate -- 指定された番号以外の候補をリセットリストに入れる */
static void
addResetCandidate(buf, n)
jcConvBuf *buf;
int n;
{
	int	i, j;
	int	ncand = buf->nCand;

	if (buf->candClause < 0) return;

	if (buf->candKind == CAND_SMALL) {
		jcSCand	*scp = (jcSCand *)buf->candBuf;
		for (i = 0; i < ncand; i++, scp++) {
			if (i != n && scp->status & IMA_BIT)
				addResetEntry(buf, scp->dicno, scp->entry);
		}
	} else {	/* CAND_LARGE */
		jcLCand	*lcp = (jcLCand *)buf->candBuf;
		for (i = 0; i < ncand; i++, lcp++) {
			jcSCand	*scp;
			if (i == n) continue;
			for (j = 0, scp = lcp->scand; j < lcp->nscand;
			     j++, scp++) {
				if (scp->status & IMA_BIT)
					addResetEntry(buf, scp->dicno,
						      scp->entry);
			}
		}
	}
}

/* addResetEntry -- 今使ったよビットを落す対象のエントリをリストに付け加える */
static void
addResetEntry(buf, dicno, entry)
jcConvBuf *buf;
int dicno;
int entry;
{
	jcEntry	*ep;
	int	i;

	/*
	 * このファンクションを呼ぶのは、
	 *
	 * 1. jcNext() が呼ばれた時に、カレント文節に入っていた候補
	 * 2. jcSelect() が呼ばれた時に、選ばれなかった候補
	 * 3. jcExpand()/jcShrink() が呼ばれた時に、
	 *    カレント文節とその次の文節に入っていた候補
	 *
	 * の３種類にする
	 * 3. はちょっとやり過ぎ (jllib はもっと賢いことをしている)
	 * だが、まあ落さないよりは落す方がいいのではないかということで
	 */

	/* すでにリストにはいっていないかチェックする */
	ep = (jcEntry *)buf->resetBuf;
	i = buf->nReset;
	while (i-- > 0) {
		if (ep->entry == entry && ep->dicno == dicno) {
			/* 入っていた */
			return;
		}
		ep++;
	}

	/* バッファの大きさのチェック */
	if (buf->nReset >= buf->resetSize) {
		char	*p;
		p = realloc(buf->resetBuf,
			    (buf->resetSize + 10) * sizeof(jcEntry));
		if (p == NULL) return;
		buf->resetSize += 10;
		buf->resetBuf = p;
	}
	ep = (jcEntry *)buf->resetBuf + buf->nReset++;
	ep->dicno = dicno;
	ep->entry = entry;
}

/* saveDicAll -- 環境で使用されている辞書をセーブする */
static void
saveDicAll(buf)
jcConvBuf *buf;
{
	int	ndic, i;
	WNN_DIC_INFO	*diclist;
	struct wnn_env	*env = buf->env;
	char	hname[256];
	int	hlen;

	if ((ndic = js_dic_list(env, &jsbuf)) < 0)
		return;

	/*
	 * クライアント側のファイルをセーブする時には、そのホストにある
	 * ファイルかどうかを hostname でチェックする
	 */
	(void)gethostname(hname, sizeof(hname));
	hname[sizeof(hname) - 2] = '\0';
	(void)strcat(hname, "!");
	hlen = strlen(hname);

	diclist = (WNN_DIC_INFO *)jsbuf.buf;
	for (i = 0; i < ndic; i++) {
		/* 登録可能形式でない辞書をセーブする必要はないだろう */
		if (diclist->rw == WNN_DIC_RW && diclist->body >= 0) {
			/* 辞書本体のセーブ */
			if (diclist->localf) {
				(void)js_file_write(env, diclist->body,
						    diclist->fname);
			} else if (!strncmp(diclist->fname, hname, hlen)) {
				(void)js_file_receive(env, diclist->body,
						      NULL);
			}
		}
		if (diclist->hindo >= 0) {
			/* 頻度ファイル */
			if (diclist->hlocalf) {
				(void)js_file_write(env, diclist->hindo,
						    diclist->hfname);
			} else if (!strncmp(diclist->hfname, hname, hlen)) {
				(void)js_file_receive(env, diclist->hindo,
						      NULL);
			}
		}
		diclist++;
	}
}

/*
 *	ここから Public なファンクション
 */

/* jcCreateBuf -- 変換バッファの作成 */
jcConvBuf *
jcCreateBuffer(env, nclause, buffersize)
struct wnn_env *env;
int nclause;
int buffersize;
{
	jcConvBuf	*buf;

	/* まず jcConvBuf の確保 */
	if ((buf = (jcConvBuf *)malloc(sizeof(jcConvBuf))) == NULL) {
		jcErrno = JE_NOCORE;
		return NULL;
	}
	(void)bzero((char *)buf, sizeof(jcConvBuf));
	buf->env = env;

	/* 次に各種バッファの確保 */

	/* まず、かなバッファとディスプレイバッファ */
	buf->bufferSize = (buffersize <= 0) ? DEF_BUFFERSIZE : buffersize;
	/* バッファの最後を NULL ターミネートすることがあるので、
	 * 1文字文大きくしておく
	 */
	buf->kanaBuf = (wchar *)malloc((buf->bufferSize + 1) *
					 sizeof(wchar));
	buf->displayBuf = (wchar *)malloc((buf->bufferSize + 1) *
					    sizeof(wchar));

	/* 次に clauseInfo バッファ */
	buf->clauseSize = (nclause <= 0) ? DEF_CLAUSESIZE : nclause;
	/* clauseInfo バッファは nclause + 1 個アロケートする
	 * なぜかというと clauseinfo はデリミタとして要素を
	 * 1個使うので nclause 個の文節を扱うためには nclause + 1 個の
	 * 大きさを持たなければならないからである
	 */
	buf->clauseInfo = (jcClause *)malloc((buf->clauseSize + 1)
					     * sizeof(jcClause));

	/* 次候補バッファ */
	buf->candSize = DEF_CANDSIZE;
	buf->candBuf = (char *)malloc(buf->candSize);

	/* リセットバッファ */
	buf->resetSize = DEF_RESETSIZE;
	buf->resetBuf = (char *)malloc(buf->resetSize * sizeof(jcEntry));

	if (buf->kanaBuf == NULL || buf->displayBuf == NULL ||
	    buf->clauseInfo == NULL || buf->candBuf == NULL ||
	    buf->resetBuf == NULL) {
		/* malloc() できなかった */
		Free(buf->kanaBuf);
		Free(buf->displayBuf);
		Free(buf->clauseInfo);
		Free(buf->candBuf);
		Free(buf->resetBuf);
		Free(buf);
		jcErrno = JE_NOCORE;
		return NULL;
	}

	(void)jcClear(buf);
	return buf;
}

/* jcDestroyBuffer -- 変換バッファの消去 */
int
jcDestroyBuffer(buf, savedic)
jcConvBuf *buf;
int savedic;
{
	/* アロケートしたメモリの解放 */
	Free(buf->kanaBuf);
	Free(buf->displayBuf);
	Free(buf->clauseInfo);
	Free(buf->candBuf);
	Free(buf->resetBuf);

	/* savedic が 0 でなければ、環境にロードされている全てのファイルを
	 * save する
	 */
	if (savedic)
		saveDicAll(buf);

	Free(buf);

	return 0;
}

/* jcClear -- jclib のクリア 新たな変換を始める毎に呼ばなければならない */
int
jcClear(buf)
jcConvBuf *buf;
{
	/* 初期値の設定 */
	buf->nClause = buf->curClause = buf->curLCStart = 0;
	buf->curLCEnd = 1;
	buf->kanaEnd = buf->kanaBuf;
	buf->displayEnd = buf->displayBuf;
	buf->clauseInfo[0].kanap = buf->kanaBuf;
	buf->clauseInfo[0].fzkp = buf->kanaBuf;
	buf->clauseInfo[0].dispp = buf->displayBuf;
	buf->clauseInfo[0].conv = 0;
	buf->clauseInfo[0].ltop = 1;
	buf->dot = buf->kanaBuf;
	buf->fixed = 0;
	buf->candClause = buf->candClauseEnd = -1;
	buf->nReset = 0;
	jcErrno = JE_NOERROR;

	return 0;
}

/* jcConvert -- カレント文節以降をかな漢字変換する */
int
jcConvert(buf, small, tan, jump)
jcConvBuf *buf;
int small;
int tan;
int jump;
{
	int	ret;

	CHECKFIXED(buf);

	if (buf->curClause == buf->nClause) {
		/* カレント文節が最後の文節でしかも空 */
		jcErrno = JE_CLAUSEEMPTY;
		return -1;
	}

	/* もし次候補バッファの内容がカレント文節以降だと無意味になる */
	checkAndResetCandidates(buf,
				small ? buf->curClause : buf->curLCStart,
				buf->nClause);

	if (tan) {
		ret = tanConvert(buf, small);
	} else {
		ret = renConvert(buf, small);
	}

	if (ret < 0)
		return ret;

	if (jump) {
		/* ドットとカレント文節を文の最後に移動させる */
		buf->curClause = buf->curLCStart = buf->nClause;
		buf->curLCEnd = buf->nClause + 1;
		buf->dot = buf->kanaEnd;
	}
	return 0;
}

/* jcUnconvert -- カレント大文節を無変換の状態に戻す */
int
jcUnconvert(buf)
jcConvBuf *buf;
{
	jcClause	*clp = buf->clauseInfo + buf->curClause;

	CHECKFIXED(buf);

	if (buf->curClause == buf->nClause) {
		/* カレント文節が最後の文節でしかも空 */
		jcErrno = JE_CLAUSEEMPTY;
		return -1;
	}

	if (!clp->conv) {
		/* カレント文節は変換されていない */
		/* 無変換の文節は jclib 内部では常に大文節として
		 * 扱われるので、カレント小文節の変換状態を見て、
		 * それが変換状態ならカレント大文節内の
		 * 全ての小文節は変換状態、そうでなければ無変換状態、
		 * になる
		 */
		jcErrno = JE_NOTCONVERTED;
		return -1;
	}

	checkAndResetCandidates(buf, buf->curLCStart, buf->curLCEnd);

	if (unconvert(buf, buf->curLCStart, buf->curLCEnd) < 0)
		return -1;

	/* 大文節の設定 */
	clp = buf->clauseInfo + buf->curLCStart;
	clp->ltop = 1;
	(++clp)->ltop = 1;

	/* カレント文節の再設定 */
	buf->curClause = buf->curLCStart;
	buf->curLCEnd = buf->curLCStart + 1;

	/* ドットの設定 */
	DotSet(buf);

	return 0;
}

/* jcExpand -- カレント文節を１文字広げる */
int
jcExpand(buf, small, convf)
jcConvBuf *buf;
int small;
int convf;
{
	CHECKFIXED(buf);

	return expandOrShrink(buf, small, 1, convf);
}

/* jcShrink -- カレント文節を１文字縮める */
int
jcShrink(buf, small, convf)
jcConvBuf *buf;
int small;
int convf;
{
	CHECKFIXED(buf);

	return expandOrShrink(buf, small, 0, convf);
}

/* jcKana -- カレント文節をかなにする */
int
jcKana(buf, small, kind)
jcConvBuf *buf;
int small;
int kind;
{
	jcClause	*clp;
	wchar		*kanap, *kanaendp, *dispp;
	int		start, end;
	int		conv;
	int		c;

	CHECKFIXED(buf);

	/* 文節番号のチェック */
	if (buf->curClause >= buf->nClause) {
		/* カレント文節が最後の文節でしかも空だった場合
		 * この場合エラーにしてもよいが...
		 */
		return 0;
	}

	/*
	 * カレント文節が変換されていればいったん無変換にする
	 */

	/* あとで変換状態をもとに戻すため、変換状態をセーブしておく */
	conv = buf->clauseInfo[buf->curClause].conv;

	if (small) {
		start = buf->curClause;
		end = start + 1;
	} else {
		start = buf->curLCStart;
		end = buf->curLCEnd;
	}

	/* 次候補バッファの内容のチェック */
	checkAndResetCandidates(buf, start, end);

	if (unconvert(buf, start, end) < 0) {
		return -1;
	}

	/* small が 0、つまりカレント文節として大文節を選択した場合、
	 * その中の小文節は一つにまとめられるので、curClause と
	 * curLCEnd を変える必要がある
	 */
	if (!small) {
		buf->curClause = buf->curLCStart;
		buf->curLCEnd = buf->curLCStart + 1;
	}

	/* かな変換する */
	/* ディスプレイバッファだけではなく、かなバッファも変換する */
	/* これにはさしたる理由はないが、まあ、Ver3 版の jclib が
	 * そうだったので…
	 */
	clp = buf->clauseInfo + buf->curClause;
	kanap = clp->kanap;
	kanaendp = (clp + 1)->kanap;
	dispp = clp->dispp;

	if (kind == JC_HIRAGANA) {	/* カタカナ→ひらがな */
		/* カタカナをひらがなに変換する際にはひらがなにない字
		 * "ヴヵヶ" があるのでいきおいで変換してしまわないように
		 * 気を付けなければならない
		 * (まあ実際は気をつけるというほどのものではないが)
		 */
		while (kanap < kanaendp) {
			c = *kanap;
			if ((KANABEG + KATAOFFSET) <= c &&
					c <= (KANAEND + KATAOFFSET)) {
				*kanap = *dispp = c - KATAOFFSET;
			}
			kanap++, dispp++;
		}
	} else {	/* ひらがな→カタカナ */
		while (kanap < kanaendp) {
			c = *kanap;
			if (KANABEG <= c && c <= KANAEND) {
				*kanap = *dispp = c + KATAOFFSET;
			}
			kanap++, dispp++;
		}
	}

	/*
	 * 変換状態をもとに戻しておく
	 */

	/* とはいっても既に変換された文節の場合、これの頻度情報を
	 * サーバに送るとまずいので、あとでかな変換したことがわかるように
	 * jcClause.conv は -1 にセットする
	 */
	if (conv) {
		clp->conv = -1;
		clp->hinshi = WNN_ALL_HINSI;
		clp->fzkp = (clp + 1)->kanap;
	} else {
		clp->conv = 0;
	}
	return 0;
}

/* jcFix -- 確定する */
int
jcFix(buf)
jcConvBuf *buf;
{
	struct wnn_env	*env = buf->env;
	jcEntry		*ep;
	jcClause	*clp;
	int		i;

	if (buf->fixed) {
		/* 既に確定されている
		 * エラーにしてもよいが…
		 */
		return 0;
	}

	/* 頻度情報をセットする */

	/* まずは今使ったよビットを落す */
	ep = (jcEntry *)buf->resetBuf;
	for (i = 0; i < buf->nReset; i++, ep++) {
		if (ep->dicno >= 0) {
			(void)js_hindo_set(env, ep->dicno, ep->entry,
					   WNN_IMA_OFF, WNN_HINDO_NOP);
		}
	}
	buf->nReset = 0;

	/* 次に、頻度を上げ、今使ったよビットを立てる */
	clp = buf->clauseInfo;
	for (i = 0; i < buf->nClause; i++) {
		/* 変換フラグ clp->conv は -1, 0, 1 の３種類の値をとる
		 * このうち、-1 は jclib による疑似文節、0 は未変換の
		 * 文節なので、これらに関しては無視する
		 */
		if (clp->conv == 1) {
			(void)js_hindo_set(buf->env, clp->dicno, clp->entry,
					   WNN_IMA_ON, WNN_HINDO_INC);
		}
		clp++;
	}

	/* 確定フラグを立てる */
	buf->fixed = 1;

	return 0;
}

/* jcNext -- カレント文節を次候補/前候補で置き換える */
int
jcNext(buf, small, prev)
jcConvBuf *buf;
int small;
int prev;
{
	int	curcand;

	CHECKFIXED(buf);

	if (!buf->clauseInfo[buf->curClause].conv) {
		/* まだ変換されていない */
		jcErrno = JE_NOTCONVERTED;
		return -1;
	}

	/* 今使われている単語をリセットバッファに加える */
	if (small) {
		addResetClause(buf, buf->curClause, buf->curClause + 1);
	} else {
		addResetClause(buf, buf->curLCStart, buf->curLCEnd);
	}

	/* 次候補の情報を次候補バッファに用意する */
	if (setupCandBuf(buf, small) < 0)
		return -1;

	if (buf->nCand == 1) {
		/* 次候補がない */
		jcErrno = JE_NOCANDIDATE;
		return -1;
	}

	if (prev) {
		curcand = buf->curCand - 1;
		if (curcand < 0)
			curcand = buf->nCand - 1;
	} else {
		curcand = buf->curCand + 1;
		if (curcand >= buf->nCand)
			curcand = 0;
	}

	/* ディスプレイバッファを変更 */
	if (changeCand(buf, curcand) < 0)
		return -1;

	buf->curCand = curcand;

	return 0;
}

/* jcCandidateInfo -- 次候補の数と現在の候補番号を調べる
 *		      もし次候補がまだバッファに入っていなければ用意する
 */
int
jcCandidateInfo(buf, small, ncandp, curcandp)
jcConvBuf *buf;
int small;
int *ncandp;
int *curcandp;
{
	CHECKFIXED(buf);

	if (!buf->clauseInfo[buf->curClause].conv) {
		/* まだ変換されていない */
		jcErrno = JE_NOTCONVERTED;
		return -1;
	}

	/* 次候補の情報を次候補バッファに用意する */
	if (setupCandBuf(buf, small) < 0)
		return -1;

	*ncandp = buf->nCand;
	*curcandp = buf->curCand;

	return 0;
}

/* jcGetCandidate -- 指定された番号の候補を取り出す */
int
jcGetCandidate(buf, n, candstr)
jcConvBuf *buf;
int n;
wchar *candstr;
{
	int	ns;
	wchar	*p;
	jcLCand *lcandp;
	jcSCand *scandp;

	CHECKFIXED(buf);

	/* 文節のチェック */
	if (buf->candClause < 0) {
		jcErrno = JE_NOCANDIDATE;
		return -1;
	}

	/* 候補番号のチェック */
	if (n < 0 || n >= buf->nCand) {
		jcErrno = JE_NOSUCHCANDIDATE;
		return -1;
	}

	/* 文字列をコピー */
	if (buf->candKind == CAND_SMALL) {
		scandp = (jcSCand *)buf->candBuf;
		p = scandp[n].kanji;
		while (*candstr++ = *p++)
			;
	} else {
		lcandp = (jcLCand *)buf->candBuf;
		scandp = lcandp[n].scand;
		ns = lcandp[n].nscand;
		while (ns-- > 0) {
			p = scandp++->kanji;
			while (*candstr++ = *p++)
				;
			candstr--;
		}
	}

	return 0;
}

/* jcSelect -- ディスプレイバッファを指定された候補と置き換える */
int
jcSelect(buf, n)
jcConvBuf *buf;
int n;
{
	CHECKFIXED(buf);

	/* 文節のチェック */
	if (buf->candClause < 0) {
		jcErrno = JE_NOCANDIDATE;
		return -1;
	}

	/* 候補番号のチェック */
	if (n < 0 || n >= buf->nCand) {
		jcErrno = JE_NOSUCHCANDIDATE;
		return -1;
	}

	/* 指定された候補以外のものをリセットリストに入れる */
	addResetCandidate(buf, n);

	/* ディスプレイバッファを変更 */
	if (changeCand(buf, n) < 0)
		return -1;

	/* CurCand の変更 */
	buf->curCand = n;

	return 0;
}

/* jcDotOffset -- 大文節の先頭からのドットのオフセットを返す */
int
jcDotOffset(buf)
jcConvBuf *buf;
{
	return buf->dot - buf->clauseInfo[buf->curLCStart].kanap;
}

/* jcIsConverted -- 指定された文節が変換されているかどうかを返す */
int
jcIsConverted(buf, cl)
jcConvBuf *buf;
int cl;
{
	if (cl < 0 || cl > buf->nClause) {
		/* cl == jcNClause のときをエラーにしてもいいのだけれど
		 * カレント文節が jcNClause のときがあるので
		 * エラーとはしないことにした
		 */
		return -1;
	}
	return (buf->clauseInfo[cl].conv != 0);
}

/* jcMove -- ドット・カレント文節を移動する */
int
jcMove(buf, small, dir)
jcConvBuf *buf;
int small;
int dir;
{
	jcClause	*clp = buf->clauseInfo + buf->curClause;
	int		i;

	if (!clp->conv) {
		/* カレント文節が変換されていないので、ドットの移動になる */
		if (dir == JC_FORWARD) {
			if (buf->curClause == buf->nClause) {
				/* すでに一番最後にいる */
				jcErrno = JE_CANTMOVE;
				return -1;
			} else if (buf->dot == (clp + 1)->kanap) {
				/* ドットがカレント文節の最後にあるので
				 * 文節移動する
				 */
				goto clausemove;
			} else {
				buf->dot++;
			}
		} else {
			if (buf->dot == clp->kanap) {
				/* ドットがカレント文節の先頭にあるので
				 * 文節移動する
				 */
				goto clausemove;
			} else
				buf->dot--;
		}
		return 0;
	}

clausemove:	/* 文節移動 */
	clp = buf->clauseInfo;

	if (small) {
		/* 小文節単位の移動 */
		if (dir == JC_FORWARD) {
			if (buf->curClause == buf->nClause) {
				jcErrno = JE_CANTMOVE;
				return -1;
			}
			buf->curClause++;
			if (buf->curClause >= buf->curLCEnd) {
				/* 大文節も移動する */
				buf->curLCStart = buf->curLCEnd;
				for (i = buf->curLCStart + 1;
				     i <= buf->nClause && !clp[i].ltop; i++)
					;
				buf->curLCEnd = i;
			}
		} else {	/* JC_BACKWARD */
			if (buf->curClause == 0) {
				jcErrno = JE_CANTMOVE;
				return -1;
			}
			buf->curClause--;
			if (buf->curClause < buf->curLCStart) {
				/* 大文節も移動する */
				buf->curLCEnd = buf->curLCStart;
				for (i = buf->curClause; !clp[i].ltop; i--)
					;
				buf->curLCStart = i;
			}
		}
	} else {
		/* 大文節単位の移動 */
		if (dir == JC_FORWARD) {
			if (buf->curLCStart == buf->nClause) {
				jcErrno = JE_CANTMOVE;
				return -1;
			}
			buf->curLCStart = buf->curClause = buf->curLCEnd;
			for (i = buf->curLCStart + 1;
			     i <= buf->nClause && !clp[i].ltop; i++)
				;
			buf->curLCEnd = i;
		} else {
			if (buf->curLCStart == 0) {
				jcErrno = JE_CANTMOVE;
				return -1;
			}
			buf->curLCEnd = buf->curLCStart;
			for (i = buf->curLCEnd - 1; !clp[i].ltop; i--)
				;
			buf->curLCStart = buf->curClause = i;
		}
	}

	/* 文節移動したらドットはその文節の先頭に移動する */
	buf->dot = clp[buf->curClause].kanap;

	return 0;
}

/* jcTop -- ドット・カレント文節を文の先頭に移動する */
int
jcTop(buf)
jcConvBuf *buf;
{
	/* カレント文節を 0 にしてドットを先頭に持ってくる */
	setCurClause(buf, 0);
	buf->dot = buf->kanaBuf;

	return 0;
}

/* jcBottom -- ドット・カレント文節を文の最後に移動する */
int
jcBottom(buf)
jcConvBuf *buf;
{
	/*
	 * Ver3 対応の jclib では、カレント文節を jcNClause にして
	 * ドットを最後に持ってくるだけだった
	 * これだと、最後の文節にかなを入れていて、カーソルを動かして
	 * jcBottom() で元に戻って再びかなを入れると、別の文節に
	 * なってしまう
	 * そこで、最後の文節が無変換状態の時には、カレント文節は
	 * buf->nClause ではなく、buf->nClause - 1 にすることにする
	 */
	if (buf->nClause > 0 && !buf->clauseInfo[buf->nClause - 1].conv) {
		buf->curClause = buf->curLCStart = buf->nClause - 1;
		buf->curLCEnd = buf->nClause;
	} else {
		buf->curClause = buf->curLCStart = buf->nClause;
		buf->curLCEnd = buf->nClause + 1;
	}
	buf->dot = buf->kanaEnd;
	return 0;
}

/* jcInsertChar -- ドットの位置に一文字挿入する */
int
jcInsertChar(buf, c)
jcConvBuf *buf;
int c;
{
	jcClause	*clp;
	wchar	*dot, *dispdot;
	int	ksizenew, dsizenew;

	CHECKFIXED(buf);

	/* 候補文節がカレント文節にあれば無効にする */
	checkAndResetCandidates(buf, buf->curLCStart, buf->curLCEnd);

	/*
	 * ・カレント文節番号が buf->nClause である場合
	 *	- これはドットが最後の文節の次にあるということなので
	 *	  新しい文節を作る
	 * ・変換済みの文節の場合
	 *	- 無変換の状態に戻してから挿入
	 * ・その他
	 *	- 単に挿入すればよい
	 */
	clp = buf->clauseInfo + buf->curLCStart;
	if (buf->curLCStart == buf->nClause) {
		/* 新たに文節を作る */
		/* clauseInfo のサイズのチェック */
		if (buf->nClause >= buf->clauseSize &&
		    resizeCInfo(buf, buf->nClause + 1) < 0) {
			return -1;
		}
		/* buf->nClause のアップデートと clauseInfo の設定 */
		clp = buf->clauseInfo + ++(buf->nClause);
		clp->conv = 0;
		clp->ltop = 1;
		clp->kanap = buf->kanaEnd;
		clp->fzkp = buf->kanaEnd;
		clp->dispp = buf->displayEnd;
	} else if (clp->conv) {
		/* 無変換状態にする */
		if (unconvert(buf, buf->curLCStart, buf->curLCEnd) < 0)
			return -1;
		buf->curClause = buf->curLCStart;
		buf->curLCEnd = buf->curLCStart + 1;
		DotSet(buf);
	}

	clp = buf->clauseInfo + buf->curLCStart;

	/* バッファの大きさのチェック */
	ksizenew = (buf->kanaEnd - buf->kanaBuf) + 1;
	dsizenew = (buf->displayEnd - buf->displayBuf) + 1;
	if ((ksizenew > buf->bufferSize || dsizenew > buf->bufferSize) &&
	    resizeBuffer(buf, ksizenew > dsizenew ? ksizenew : dsizenew) < 0) {
		    return -1;
	}

	/* かなバッファをアップデート */
	dot = buf->dot;
	/* カレント文節の後ろを一文字ずらす */
	moveKBuf(buf, buf->curLCStart + 1, 1);
	/* カレント文節内のドット以降を一文字ずらす */
	(void)bcopy((char *)dot, (char *)(dot + 1),
		    ((clp + 1)->kanap - dot) * sizeof(wchar));
	/* 挿入 */
	*dot = c;

	/* ディスプレイバッファをアップデート */
	dispdot = clp->dispp + (dot - clp->kanap);
	/* カレント文節の後ろを一文字ずらす */
	moveDBuf(buf, buf->curLCStart + 1, 1);
	/* カレント文節内のドット以降を一文字ずらす */
	(void)bcopy((char *)dispdot, (char *)(dispdot + 1),
	      ((clp + 1)->dispp - dispdot) * sizeof(wchar));
	/* 挿入 */
	*dispdot = c;

	/* ドットを更新 */
	buf->dot++;

	return 0;
}

/* jcDeleteChar -- ドットの前または後ろの一文字を削除する */
int
jcDeleteChar(buf, prev)
jcConvBuf *buf;
int prev;
{
	jcClause	*clp;
	wchar		*dot, *dispdot;

	CHECKFIXED(buf);

	clp = buf->clauseInfo;
	if (buf->nClause == 0) {
		/* 文節数が 0、つまり何も入っていない時:
		 *	- エラー
		 */
		jcErrno = JE_CANTDELETE;
		return -1;
	} else if (buf->curClause >= buf->nClause) {
		/* カレント文節が最後の文節の次にある時:
		 *	- prev であれば、前の文節の最後の文字を削除
		 *	  カレント文節は前の文節に移動する
		 *	  必要ならば前の文節を無変換に戻してから削除する
		 *	  前の文節がないことはあり得ない
		 *	- !prev ならばエラー
		 */
		if (!prev) {
			jcErrno = JE_CANTDELETE;
			return -1;
		}
		(void)jcMove(buf, 0, JC_BACKWARD);
	} else if (clp[buf->curLCStart].conv) {
		/* カレント文節が変換されている時:
		 *	- prev であれば前の文節の最後の文字を削除
		 *	  カレント文節は前の文節に移動する
		 *	  必要ならば前の文節を無変換に戻してから削除する
		 *	  カレント文節が先頭ならばエラー
		 *	- !prev ならカレント文節を無変換に戻して、文節の
		 *	  最初の文字を削除
		 */
		if (prev) {
			if (buf->curLCStart == 0) {
				jcErrno = JE_CANTDELETE;
				return -1;
			}
			(void)jcMove(buf, 0, JC_BACKWARD);
		}
	} else {
		/* カレント文節が変換されていない時:
		 *	- prev であればドットの前の文字を削除
		 *	  ただしドットが文節の先頭にあれば前の文節の
		 *	  最後の文字を削除
		 *	  その時にはカレント文節は前の文節に移動する
		 *	  必要ならば前の文節を無変換に戻してから削除する
		 *	  カレント文節が先頭ならばエラー
		 *	- !prev ならドットの次の文字を削除
		 *	  ドットが文節の最後の文字の次にあればエラー
		 */
		if (prev) {
			if (buf->dot == clp[buf->curLCStart].kanap) {
				if (buf->curLCStart == 0) {
					jcErrno = JE_CANTDELETE;
					return -1;
				}
				(void)jcMove(buf, 0, JC_BACKWARD);
			}
		} else {
			if (buf->dot == clp[buf->curLCEnd].kanap) {
				jcErrno = JE_CANTDELETE;
				return -1;
			}
		}
	}

	/* 候補文節がカレント文節にあれば無効にする */
	checkAndResetCandidates(buf, buf->curLCStart, buf->curLCEnd);

	if (buf->clauseInfo[buf->curLCStart].conv) {
		/* カレント文節が変換済みであれば無変換に戻す */
		if (jcUnconvert(buf) < 0)
			return -1;
		/* prev であれば文節の最後の文字、そうでなければ文節の
		 * 先頭の文字を削除する
		 */
		if (prev) {
			buf->dot = buf->clauseInfo[buf->curLCEnd].kanap - 1;
		} else {
			buf->dot = buf->clauseInfo[buf->curLCStart].kanap;
		}
	} else {
		/* prev ならドットを１文字戻しておく
		 * こうすればドットの後ろの文字を削除することになる
		 * 削除し終わったときにドットを動かす必要もない
		 */
		if (prev)
			buf->dot--;
	}

	clp = buf->clauseInfo + buf->curLCStart;

	/* かなバッファをアップデート */
	dot = buf->dot;
	/* カレント文節内のドット以降を一文字ずらす */
	(void)bcopy((char *)(dot + 1), (char *)dot,
		    ((clp + 1)->kanap - (dot + 1)) * sizeof(wchar));
	/* カレント文節の後ろを一文字ずらす */
	moveKBuf(buf, buf->curLCEnd, -1);

	/* ディスプレイバッファをアップデート */
	dispdot = clp->dispp + (dot - clp->kanap);
	/* カレント文節内のドット以降を一文字ずらす */
	(void)bcopy((char *)(dispdot + 1), (char *)dispdot,
		   ((clp + 1)->dispp - (dispdot + 1)) * sizeof(wchar));
	/* カレント文節の後ろを一文字ずらす */
	moveDBuf(buf, buf->curLCEnd, -1);

	/* カレント文節の長さが１だった場合には文節が１減ることになる */
	if (clp->kanap == (clp + 1)->kanap) {
		/* 文節がなくなってしまった */
		moveCInfo(buf, buf->curLCEnd, -1);
		setCurClause(buf, buf->curLCStart);
		DotSet(buf);
	}

	return 0;
}

/* jcChangeClause -- カレント大文節を指定された文字列で置き換える */
int
jcChangeClause(buf, str)
jcConvBuf *buf;
wchar *str;
{
	jcClause	*clps, *clpe;
	wchar	*p;
	int	newlen;
	int	oklen, odlen;
	int	ksize, dsize;

	CHECKFIXED(buf);

	checkAndResetCandidates(buf, buf->curLCStart, buf->curLCEnd);

	clps = buf->clauseInfo + buf->curLCStart;
	clpe = buf->clauseInfo + buf->curLCEnd;

	newlen = 0;
	p = str;
	while (*p++)
		newlen++;

	/* かなバッファとディスプレイバッファのサイズを調べて、
	 * 入らなかったら大きくする
	 */
	if (buf->curLCStart < buf->nClause) {
		oklen = clpe->kanap - clps->kanap;
		odlen = clpe->dispp - clps->dispp;
	} else {
		oklen = odlen = 0;
	}
	ksize = (buf->kanaEnd - buf->kanaBuf) + newlen - oklen;
	dsize = (buf->displayEnd - buf->displayBuf) + newlen - odlen;
	if (ksize > buf->bufferSize || dsize > buf->bufferSize) {
		if (resizeBuffer(buf, ksize > dsize ? ksize : dsize) < 0)
			return -1;
	}

	/* curLCStart が nClause に等しい時だけ、新たに文節が作られる */
	if (buf->curLCStart == buf->nClause) {
		/* clauseInfo の大きさを調べる*/
		if (buf->nClause + 1 > buf->clauseSize) {
			if (resizeCInfo(buf, buf->nClause + 1) < 0)
				return -1;
		}
		/* 新たにできた clauseInfo には、nClause 番目
		 * (つまり最後の clauseInfo) の内容をコピーしておく
		 */
		clpe = buf->clauseInfo + buf->nClause + 1;
		*clpe = *(clpe - 1);

		buf->nClause++;
	}

	clps = buf->clauseInfo + buf->curLCStart;
	clpe = buf->clauseInfo + buf->curLCEnd;

	/* かなバッファの変更 */
	/* まずは後ろを移動させる */
	moveKBuf(buf, buf->curLCEnd, newlen - oklen);
	/* str をコピー */
	(void)bcopy((char *)str, (char *)clps->kanap,
		    newlen * sizeof(wchar));
	/* ディスプレイバッファの変更 */
	/* まずは後ろを移動させる */
	moveDBuf(buf, buf->curLCEnd, newlen - odlen);
	/* str をコピー */
	(void)bcopy((char *)str, (char *)clps->dispp,
		    newlen * sizeof(wchar));

	/* clauseInfo の変更 */
	/* まずは後ろを移動させる */
	if (clpe > clps + 1) {
		(void)bcopy((char *)clpe, (char *)(clps + 1),
			    (buf->nClause + 1 - buf->curLCEnd) *
			    sizeof(jcClause));
	}
	clps->conv = 0;
	clps->ltop = 1;
	(clps + 1)->ltop = 1;

	return 0;
}

/* jcSaveDic -- 辞書・頻度ファイルをセーブする */
int
jcSaveDic(buf)
jcConvBuf *buf;
{
	saveDicAll(buf);
	return 0;
}

#ifdef DEBUG_JCLIB

#include "WStr.h"

#define UL	"\033[4m"	/* アンダーライン */
#define MD	"\033[1m"	/* xterm ボールド */
#define SO	"\033[7m"	/* スタンドアウト */
#define ST	"\033[m"	/* ノーマル */

static void
wputstr(ss, se)
register wchar	*ss, *se;
{
	wchar	wline[256];
	wchar	*p;
	unsigned char	line[256];

	p = wline;
	while (ss < se)
		*p++ = *ss++;
	*p = 0;
	(void)convJWStoSJIS(wline, line);
	fputs(line, stdout);
}

void
jcPrintKanaBuf(buf)
jcConvBuf *buf;
{
	int	i;

	for (i = 0; i < buf->nClause; i++) {
		wputstr(buf->clauseInfo[i].kanap,
			buf->clauseInfo[i + 1].kanap);
		if (i < buf->nClause - 1) {
			putchar('-');
		}
	}
	putchar('\n');
}

void
jcPrintDisplayBuf(buf)
jcConvBuf *buf;
{
	int	i;
	jcClause	*clp = buf->clauseInfo;

	for (i = 0; i < buf->nClause; i++) {
		if (buf->curLCStart <= i && i < buf->curLCEnd) {
			fputs(MD, stdout);
		}
		if (i == buf->curClause) {
			fputs(SO, stdout);
		}
		if (!clp->conv) {
			fputs(UL, stdout);
		}
		wputstr(clp->dispp, (clp + 1)->dispp);
		fputs(ST, stdout);
		if (i < buf->nClause - 1) {
			if ((clp + 1)->ltop) {
				putchar('|');
			} else {
				putchar('-');
			}
		}
		clp++;
	}
	putchar('\n');
}

void
jcPrintDetail(buf)
jcConvBuf *buf;
{
	int	i;
	jcClause	*clp = buf->clauseInfo;
	int	jlen;

	printf("NofClauses: %d, CurClause: %d, CurLClause: %d-%d\n",
	       buf->nClause, buf->curClause, buf->curLCStart, buf->curLCEnd);
	if (buf->candClause >= 0) {
		if (buf->candKind == CAND_SMALL) {
			printf("CandClause: %d, ", buf->candClause);
		} else {
			printf("CandClause: %d-%d, ",
			       buf->candClause, buf->candClauseEnd);
		}
		printf("NofCandidates: %d\n", buf->nCand);
	}
	for (i = 0; i < buf->nClause; i++) {
		printf("cl[%d]: %c ", i, clp->ltop ? '*' : ' ');
		if (clp->conv > 0) {
			printf("<converted> dic=%d,entry=%d,hinshi=%d ",
			       clp->dicno, clp->entry, clp->hinshi);
			if (clp->imabit) putchar('*');
			printf("kanji: ");
			jlen = ((clp + 1)->dispp - clp->dispp) -
			    ((clp + 1)->kanap - clp->fzkp);
			wputstr(clp->dispp, clp->dispp + jlen);
			if (clp->fzkp != (clp + 1)->kanap) {
				putchar('-');
				wputstr(clp->dispp + jlen, (clp + 1)->dispp);
			}
		} else if (clp->conv < 0) {
			printf("<pseudo> kanji: ");
			wputstr(clp->dispp, (clp + 1)->dispp);
		} else {
			printf("<unconverted> kanji: ");
			wputstr(clp->dispp, (clp + 1)->dispp);
		}
		putchar('\n');
		clp++;
	}
	printf("cl[%d]: %c (last)\n", i, clp->ltop ? '*' : ' ');
	/* consistency check */
	if (clp->conv || clp->kanap != buf->kanaEnd || !clp->ltop) {
		printf("inconsistency found in last cinfo! - ");
		if (clp->conv) printf("<conv flag> ");
		if (clp->kanap != buf->kanaEnd) printf("<kanap> ");
		if (!clp->ltop) printf("<ltop flag>");
		putchar('\n');
	}
}

void
jcPrintCandBuf(buf)
jcConvBuf *buf;
{
	int	i;

	if (buf->candClause < 0) {
		printf("not ready\n");
		return;
	}

	if (buf->candKind == CAND_SMALL) {
		jcSCand	*scp;

		printf("<SMALL> CandClause: %d NofCandidates: %d CurCand: %d\n",
		       buf->candClause, buf->nCand, buf->curCand);
		scp = (jcSCand *)buf->candBuf;
		for (i = 0; i < buf->nCand; i++) {
			printf("%ccand[%d]%c: ",
			       i == buf->curCand ? '+' : ' ', i,
			       scp->status & IMA_BIT ? '*' : ' ');
			printf("dic=%d,entry=%d,hinshi=%d kanji: ",
			       scp->dicno, scp->entry, scp->hinshi);
			if (scp->status & CONNECT_PREV) putchar('<');
			wputstr(scp->kanji, scp->kanji + wstrlen(scp->kanji));
			if (scp->status & CONNECT_NEXT) putchar('>');
			putchar('\n');
			scp++;
		}
	} else {
		jcLCand	*lcp;
		jcSCand	*scp;
		int	j;

		printf("<LARGE> CandClause: %d-%d NofCandidates: %d CurCand: %d\n",
		       buf->candClause, buf->candClauseEnd,
		       buf->nCand, buf->curCand);
		lcp = (jcLCand *)buf->candBuf;
		for (i = 0; i < buf->nCand; i++) {
			printf("%ccand[%d](%d): ",
			       i == buf->curCand ? '+' : ' ', i, lcp->nscand);
			scp = lcp->scand;
			for (j = lcp->nscand; j > 0; j--) {
				wputstr(scp->kanji,
					scp->kanji + wstrlen(scp->kanji));
				if (j > 1) putchar('-');
				scp++;
			}
			putchar('\n');
			lcp++;
		}
	}
}
#endif
