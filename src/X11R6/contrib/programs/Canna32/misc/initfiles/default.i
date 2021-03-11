# Copyright 1994 NEC Corporation, Tokyo, Japan.
#
# Permission to use, copy, modify, distribute and sell this software
# and its documentation for any purpose is hereby granted without
# fee, provided that the above copyright notice appear in all copies
# and that both that copyright notice and this permission notice
# appear in supporting documentation, and that the name of NEC
# Corporation not be used in advertising or publicity pertaining to
# distribution of the software without specific, written prior
# permission.  NEC Corporation makes no representations about the
# suitability of this software for any purpose.  It is provided "as
# is" without express or implied warranty.
#
# NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
# INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
# NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
# CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
# USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
# OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
# PERFORMANCE OF THIS SOFTWARE. 

# $Id: default.i,v 1.3 1994/06/01 07:03:21 kuma Exp $

romkanaTable	"default.rdic"

dictionary	"iroha"
dictionary	"fuzokugo"
dictionary	"hojomwd"
dictionary	"hojoswd"

bushudic	"bushu"

userdic		"user"

initialMode	       alphaMode # 起動時のモードの設定
cursorWrap	       on	 # 読み入力時の右端と左端でのカーソル移動
numericalKeySelect     on	 # 候補一覧表示状態で候補を数字キーで選択
selectDirect	       off	 # 数字キーで候補一覧表示状態を終了
bunsetsuKugiri	       off	 # 文節を空白で区切る
characterBasedMove     on	 # 読み入力時でのカーソル移動を文字単位で移動
reverseWidely	       off	 # 読み入力時のカーソル反転範囲の拡大
quitIfEndOfIchiran     off	 # 候補一覧で最終候補から次候補操作で読み表示
breakIntoRoman	       off	 # ＢＳキーで直前の文字がローマ字に戻らない
gakushu		       on	 # 学習
stayAfterValidate      on	 # 候補一覧で候補選択してカレント文節を次に移動
kakuteiIfEndOfBunsetsu off	 # 最右文節で次文節移動時に確定
gramaticalQuestion     on	 # 単語登録時に詳細な質問を受ける
nHenkanForIchiran      2	 # 変換キーを押した時候補一覧になるまでの回数
