/*
 *	jilib -- かな漢字変換用ライブラリ (Wnn Version4.0 対応版)
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
 *	If you use SysVR2, you should define NO_MKDIRSYS in order to
 *	use /bin/mkdir for making directories rather than mkdir()
 *	system call.
 */

/*
 * 概要
 *
 * jilib は jslib, jclib の初期化を行うためのライブラリである。
 * 主な目的は環境初期化ファイル (wnnevnrc) を読んで環境の設定を
 * 行なうことである。
 * jllib を使うつもりなら、同様の (というか、もっと高度な) 機能は
 * jllib に入っているので、このライブラリを使う必要はないだろう。
 */

/*
 * Wnn Version 4 対応にあたって
 *
 * jilib はもともと Wnn Version 3 の jclib と共に使うように作られた
 * ライブラリである。jclib を Wnn ver4 対応にするにあたって、jilib も
 * 大幅に書き換えた。
 * 従来のものとの互換性は考えられていない。
 */

/*
 * メモ
 *
 * ver 0.0	89/08/10
 *	とりあえず作りはじめる
 * ver 0.1	89/08/14
 *	一応できた
 * ver 0.2	89/08/22
 *	SYSV だったら index/rindex を strchr/strrchr にする
 * ver 0.3	89/08/26
 *	サーバ、および環境が共有されるようにする
 *	jiCreateEnv() で js_dead_env_flg の値がリストアされない場合があった
 *	のでそれを修正
 * ver 0.4	89/08/29
 *	エラーとコンファームのコールバックに client_data の引数を追加
 * ver 0.5	89/09/01
 *	サーバと環境を linked list で持つように変更
 * ver 0.6	89/09/08
 *	Wnn-V4 の新しいバージョンが来たらライブラリのインターフェイスが
 *	違っていたのでそれに合わせて修正
 *	・js_env_exist() が追加されていたので envExist() をそれを使うように
 *	  修正
 *	・js_dic_add() に rev 引数、js_dic_file_create(_client)() に
 *	  type 引数が追加され、ついでに wnnenvrc の setdic に
 *	  かな漢字変換 / 漢字かな変換 のフラグがついたのでそれに合わせて
 *	  修正
 * ver 0.7	89/10/16
 *	@WNN_DIC_DIR と @ENV の展開が追加されているのに気づかず、
 *	インプリメントしていなかったのを修正
 *	さらに、@の展開にもともとバグがあることが判明したのでそれを修正
 * ver 0.8	89/10/16
 *	wnn-4.0.1 で commonheader.h -> commonhd.h になったので
 *	それの修正
 * ver 4.0	89/10/27
 *	バージョン番号の修正
 * ver 4.1	89/11/02
 *	SysV でうまくコンパイルできないのを修正
 * --- kinput を R4 に contribute ---
 * ver 4.2	89/12/12
 *	jiOpenServer() で、server に NULL または "" を指定し、環境変数
 *	JSERVER が定義されていない時にコアダンプするバグを修正
 * ver 4.3	90/02/20
 *	頻度ファイルが辞書とマッチしなかった時に、単にエラーにする
 *	代わりに頻度ファイルを作り直すように変更
 * ver 4.4	90/03/26
 *	ディレクトリを作るのに /bin/mkdir を system() で起動する時、
 *	(普通は mkdir() システムコールを使うので SYSVR2 の時だけだが)
 *	SIGCHLD (SIGCLD) のハンドラが定義されているとそのリターン値が
 *	おかしくなってエラーになることがあるというバグを修正
 * ver 4.5	90/03/26
 *	4.4 の修正で signal.h を include するのを忘れていた
 * ver 4.6	90/04/02
 *	wnnenvrc ファイルの setdic 行の処理が間違っていた。最後の引数を
 *	評価しないというバグを修正 (逆変換の指定が効かなかった)
 * ver 4.7	91/09/18
 *	bcopy の代わりに mybcopy を使うように変更。普通の bcopy よりは
 *	遅いがパフォーマンスには全く関係のないところなので。
 *	SVR4 の定義を追加
 *	もう mkdir() がないマシンなんてほとんどないだろうから mkdir() が
 *	ない場合には explicit に NO_MKDIRSYS を define してもらうようにする
 * ver 4.8	91/09/23
 *	DEBUG を DEBUG_JILIB に変更
 * ver 4.9	91/09/23
 *	unuseServer() と deleteEnv() の戻り値が不定になることがあるという
 *	バグを修正
 * ver 4.10	91/09/26
 *	X11R5 についてくる Wnn だと ENVRCFILE が "/wnnenvrc" になって
 *	いて、ディレクトリ部分は LIBDIR に入るようになっているので
 *	それに合わせて修正
 * ver 4.11	91/09/30
 *	4.10 の修正が完全に間違っていることがわかったのでそれの修正
 *	結局、commonhd.h の JSERVER_VERSION を見てバージョンをチェックする
 *	ようにする
 * ver 5.0	91/10/01
 *	kinput2 リリース向けにバージョン番号を修正して 5.0 にする。
 * --- kinput2 を R5 に contribute ---
 * ver 5.1	92/01/27
 *	Wnn 4.1 では wnnenvrc のデフォルトが LIBDIR/ja_JP/wnnenvrc に
 *	なったのに合わせて従来の LIBDIR/wnnenvrc と両方見るように修正
 * ver 5.2	94/06/06
 *	error() の引数の見直し。本来は varargs を使うべきだが、とりあえず
 *	問題を少なくするため、可変個の引数の型を int から char * に修正
 */

/*
 * ファンクション
 *
 * jilib が提供するファンクションは次の4つである。
 *
 * WNN_JSERVER_ID *jiOpenServer(char *servername, int timeout)
 *	servername で指定されたサーバと接続する。timeout で
 *	秒単位で接続のタイムアウトを指定することができる。
 *	接続できた場合はサーバの ID が、できなかった時には NULL が返される。
 *
 *	ほとんど js_open() と同じだが、servername が NULL あるいは
 *	空文字列 ("") であった場合には環境変数 JSERVER の値を使用する
 *	ところ、およびすでに同じサーバに接続されていればそれを共有
 *	するところが js_open() と異なる。このため、jiOpenServer() を
 *	使用して作った接続を切るためには、jiCloseServer() を用いなくては
 *	ならない。
 *
 * int jiCloseServer(WNN_JSERVER_ID *server)
 *	指定されたサーバとの接続を切る。とはいってもサーバ資源は
 *	共有されているので、リファレンスカウントが 0 にならない限り
 *	実際に js_close() が呼ばれることはない。
 *
 *	指定されたサーバが jiOpenServer() によって接続されたものでは
 *	ない場合、および js_close() が失敗した時には -1 が、そうで
 *	ない時には 0 が返される。
 *
 * WNN_ENV *jiCreateEnv(WNN_JSERVER_ID *server, char *envname,
 *			int override, char *wnnrcfile,
 *			void (*errmsgfunc)(), int (*confirmfunc)(),
 *			caddr_t client_data)
 *	server で指定されたサーバに、envname という名前の環境を作る。
 *	envname が空文字列の時は、デフォルトの環境名としてユーザ名が
 *	使われる。(envname が NULL の時は、環境名としてそのまま NULL が
 *	サーバに渡され、js_connect() のマニュアルにあるように他と共有
 *	されることの無い環境が作られる)
 *
 *	jiOpenServer() と同じように、同じサーバで同じ環境名の環境は
 *	共有される。そのため、このファンクションを使って作った環境を
 *	消すには、jiDeleteEnv() を使用しなくてはならない。
 *
 *	wnnrcfile には環境初期化ファイル名を指定する。jiCreateEnv() は
 *	その内容にしたがって辞書や付属語ファイルなどの設定をする。
 *	もし wnnrcfile が NULL ならば初期化の処理は行なわない。また、
 *	wnnrcfile が空文字列であった場合には、まず環境変数 WNNENVRC の
 *	値が使用される。それもなければデフォルトの /usr/local/lib/wnn/wnnenvrc
 *	が使用される。
 *
 *	フラグ override が 0 の時には、指定された環境がすでに存在して
 *	いれば初期化の処理を行なわない。
 *
 *	errmsgfunc および confirmfunc はともにコールバック関数である。
 *	errmsgfunc はエラーや警告のメッセージを表示する時に次の形式で呼ばれる。
 *		(*errmsgfunc)(int type, char *message, caddr_t client_data);
 *	type はメッセージのタイプで、TYPE_WARNING と TYPE_ERROR のどちらか
 *	である。TYPE_WARNING は単なる警告で、処理は続けられる。それに対し
 *	TYPE_ERROR の場合、そのエラーが起きた時点で処理は中止される。
 *	message は表示されるべきメッセージである。この文字列の最後に
 *	改行コードは入っていない。
 *	client_data には jiCreateEnv() の引数で指定された値がそのまま
 *	渡される。
 *	errmsgfunc が NULL の時は、メッセージは標準エラー出力に書かれる。
 *
 *	confirmfunc は、指定された辞書がなかった時に、ユーザに対して
 *	新たに作成するかどうかをたずねるために次の形式で呼び出される。
 *		(*confirmfunc)(int type, char *file, caddr_t client_data);
 *	type は作られるファイルの形式で、TYPE_DIC なら辞書、TYPE_HINDO なら
 *	頻度ファイルである。file はパス名である。
 *	confirmfunc は 0 または 1 の値を返さなくてはならない。返された
 *	値が 0 ならファイルは作られず、その辞書は使われない。1 ならば
 *	ファイルは新たに作られる。
 *	client_data には jiCreateEnv() の引数で指定された値がそのまま
 *	渡される。
 *	confirmfunc が NULL の時は、常に新たにファイルが作られる。
 *
 *	jiCreateEnv() はエラーの時には NULL を、そうでなければ 環境の ID を
 *	返す。
 *
 * int jiDeleteEnv(WNN_ENV *env)
 *	指定された環境を消す。環境もサーバと同じく共有されるので
 *	リファレンスカウントが 0 にならない限り実際に js_disconnect()
 *	でサーバ内の環境が消去されることはない。
 *
 *	指定された環境が jiCreateEnv() によって作られたものでは
 *	ない場合、および js_disconnect() が失敗した時には -1 が、そうで
 *	ない時には 0 が返される。
 */

#ifndef lint
static char	*rcsid = "$Id: jilib.c,v 5.2 1994/06/06 05:29:28 ishisone Rel $";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <pwd.h>
#include "commonhd.h"
#include "config.h"
#include "jslib.h"
#include "jilib.h"

#ifdef CHECK_PROTOTYPE
#include "js.c.p"
#endif

#ifdef DEBUG_JILIB
#define PRINT(a)	printf a
#else
#define PRINT(a)
#endif

#if defined(SYSV) || defined(USG) || defined(SVR4)
#define index	strchr
#define rindex	strrchr
#endif

/* used by access(2) */
#ifndef R_OK
#define R_OK	4
#endif

#if defined(USG) && !defined(SYSV)
#define NO_MKDIRSYS
#endif

#ifdef NO_MKDIRSYS
#include <signal.h>	/* for SIGCLD */
#endif

#define ERROR_LOAD	-2
#define NO_LOAD		-1

#define MAXARG	20

#define CONFIRM			1
#define CONFIRM1		2
#define CREATE_WITHOUT_CONFIRM	3
#define NO_CREATE		4

typedef struct _servrec {
	struct _servrec	*next;
	WNN_JSERVER_ID	*server;
	int		refcnt;
} ServerRec;

static ServerRec	*servers;

typedef struct _envrec {
	struct _envrec	*next;
	WNN_ENV		*env;
	char		envname[WNN_ENVNAME_LEN + 1];
	int		refcnt;
} EnvRec;

static EnvRec		*environs;

typedef struct {
	void	(*errorcall)();
	int	(*confirmcall)();
	caddr_t	client_data;
} CallbackRec;

#ifdef __STDC__
static WNN_JSERVER_ID *useServer(char *, int);
static void addServer(WNN_JSERVER_ID *);
static int unuseServer(WNN_JSERVER_ID *);
static WNN_ENV *findEnv(WNN_JSERVER_ID *, char *);
static char *getEnvName(WNN_ENV *);
static void addEnv(char *, WNN_ENV *);
static int deleteEnv(WNN_ENV *);
static int envExist(WNN_JSERVER_ID *, char *);
static int fileExist(WNN_ENV *, char *);
static int makeDir(WNN_ENV *, char *);
static int mkdirproc(WNN_ENV *, int, char *);
static int createDicFile(WNN_ENV *, char *, char *, char *);
static int createHindoFile(WNN_ENV *, char *, int, char *);
static int removeFile(WNN_ENV *, char *, int, char *);
static char *getdicpasswd(char *);
static int fileLoad(WNN_ENV *, CallbackRec *, int,
		    char *, char *, char *, int, int *);
static char *expandDicPath(char *, WNN_ENV *);
static void expandTop(char *);
static void expandUSR(char *, WNN_ENV *);
static char *strinc(char *, char *);
static void error();
static int doconfirm(CallbackRec *, int, char *);
static int readEnvFile(WNN_ENV *, char *, CallbackRec *, int *);
static int procInclude(WNN_ENV *, CallbackRec *, int *, int, char **);
static int procSetDic(WNN_ENV *, CallbackRec *, int *, int, char **);
static int procSetFuzokugo(WNN_ENV *, CallbackRec *, int, char **);
static int procSetParam(WNN_ENV *, CallbackRec *, int, char **);
#else
static WNN_JSERVER_ID *useServer();
static void addServer();
static int unuseServer();
static WNN_ENV *findEnv();
static char *getEnvName();
static void addEnv();
static int deleteEnv();
static int envExist();
static int fileExist();
static int makeDir();
static int mkdirproc();
static int createDicFile();
static int createHindoFile();
static char *getdicpasswd();
static int fileLoad();
static char *expandDicPath();
static void expandTop();
static void expandUSR();
static char *strinc();
static void error();
static int doconfirm();
static int readEnvFile();
static int procInclude();
static int procSetDic();
static int procSetFuzokugo();
static int procSetParam();
#endif

extern char	*index();
extern char	*rindex();
extern char	*getenv();
extern char	*malloc();
extern char	*realloc();
extern void	free();
extern char	*strcpy();
extern char	*strncpy();
extern char	*strcat();
extern int	strcmp();
extern int	strncmp();

/* mybcopy -- my own version of bcopy() which can handle overlapped regions */
static
mybcopy(from, to, n)
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

/* useServer -- 指定された名前のサーバを使用できるようにする */
static WNN_JSERVER_ID *
useServer(servername, timeout)
char *servername;
int timeout;
{
	ServerRec	*srp;
	WNN_JSERVER_ID	*server;

	for (srp = servers; srp; srp = srp->next) {
		if (!strcmp(servername, srp->server->js_name)) {
			srp->refcnt++;
			return srp->server;
		}
	}

	PRINT(("useServer(): do js_open()\n"));
	if ((server = js_open(servername, timeout)) != NULL) {
		addServer(server);
	}

	return server;
}

/* addServer -- 指定されたサーバをリストに登録する */
static void
addServer(server)
WNN_JSERVER_ID *server;
{
	ServerRec	*srp;

	if ((srp = (ServerRec *)malloc(sizeof(ServerRec))) != NULL) {
		srp->next = servers;
		servers = srp;
		srp->server = server;
		srp->refcnt = 1;
	}
}

/* unuseServer -- 使われなくなったサーバの処理をする */
static int
unuseServer(server)
WNN_JSERVER_ID *server;
{
	ServerRec	*srp, *srp0;
	int		ret = 0;

	for (srp = servers, srp0 = NULL; srp; srp0 = srp, srp = srp->next) {
		if (server == srp->server) {
			if (--srp->refcnt <= 0) {
				PRINT(("unuseServer(): do js_close()\n"));
				ret = js_close(server);
				if (srp0 == NULL) {
					servers = srp->next;
				} else {
					srp0->next = srp->next;
				}
				(void)free((char *)srp);
			}
			return ret;
		}
	}
	return -1;
}

/* findEnv -- 指定された名前の環境があるか、リストを調べる */
static WNN_ENV *
findEnv(server, envname)
WNN_JSERVER_ID *server;
char *envname;
{
	EnvRec	*envp;

	if (envname == NULL) return NULL;

	for (envp = environs; envp; envp = envp->next) {
		if (server == envp->env->js_id &&
		    !strcmp(envname, envp->envname)) {
			envp->refcnt++;
			return envp->env;
		}
	}

	return NULL;
}

/* getEnvName -- 環境名を返す */
static char *
getEnvName(env)
WNN_ENV *env;
{
	EnvRec	*envp;

	if (env == NULL) return NULL;

	for (envp = environs; envp; envp = envp->next) {
		if (env == envp->env)
			return envp->envname;
	}

	return NULL;
}

/* addEnv -- 指定された環境をリストに登録する */
static void
addEnv(envname, env)
char *envname;
WNN_ENV *env;
{
	EnvRec	*envp;

	if ((envp = (EnvRec *)malloc(sizeof(EnvRec))) != NULL) {
		envp->next = environs;
		environs = envp;
		envp->env = env;
		if (envname != NULL) {
			(void)strncpy(envp->envname, envname, WNN_ENVNAME_LEN);
		} else {
			envp->envname[0] = '\0';
		}
		envp->refcnt = 1;
	}
}

/* deleteEnv -- 使われなくなった環境の処理をする */
static int
deleteEnv(env)
WNN_ENV *env;
{
	EnvRec	*envp, *envp0;
	int	ret = 0;

	for (envp0 = NULL, envp = environs; envp; envp0 = envp, envp = envp->next) {
		if (env == envp->env) {
			if (--envp->refcnt <= 0) {
				PRINT(("deleteEnv(): do js_disconnect()\n"));
				ret = js_disconnect(env);
				if (envp0 == NULL) {
					environs = envp->next;
				} else {
					envp0->next = envp->next;
				}
				(void)free((char *)envp);
			}
			return ret;
		}
	}
	return -1;
}

/* envExist -- 指定された名前の環境があるかどうか調べる */
static int
envExist(server, envname)
WNN_JSERVER_ID *server;
char *envname;
{
	return js_env_exist(server, envname);
}

/* fileExist -- 指定された名前のファイルがあるかどうか調べる */
static int
fileExist(env, file)
WNN_ENV *env;
char *file;
{
	int	ret;

	if (*file == C_LOCAL) {
		ret = access(file + 1, R_OK);
	} else {
		ret = js_access(env, file, R_OK);
	}

	return ret >= 0;
}

/* makeDir -- 指定されたディレクトリを作る */
static int
makeDir(env, dir)
WNN_ENV *env;
char *dir;
{
	char	c;
	char	*p, *q;
	int	local = 0;

	if (*dir == C_LOCAL) {
		local++;
		dir++;
	}

	/* すでにあるかどうか調べる */
	if (local) {
		if (access(dir, R_OK) >= 0)
			return 0;
	} else {
		if (js_access(env, dir, R_OK) >= 0)
			return 0;
		else if (wnn_errorno == WNN_JSERVER_DEAD)
			return -1;
	}

	/* 上から作っていく */
	/* さすがに / は作る必要がないと思われる */
	q = (*dir == '/') ? dir + 1 : dir;

	while (p = index(q, '/')) {
		c = *p; *p = '\0';
		if (mkdirproc(env, local, dir) < 0)
			return -1;
		*p = c;
		q = p + 1;
	}
	if (*q != '\0' && mkdirproc(env, local, dir) < 0)
		return -1;

	return 0;
}

static int
mkdirproc(env, local, dir)
WNN_ENV *env;
int local;
char *dir;
{
	if (local) {
		/* まずはあるかどうか調べる */
		if (access(dir, R_OK) >= 0)
			return 0;
#ifdef NO_MKDIRSYS
		{
		char	buf[1024];	/*enough?*/
		int	(*ohandle)() = signal(SIGCLD, SIG_DFL);
		int	ret;
		(void)sprintf(buf, "/bin/mkdir %s", dir);
		ret = system(buf);
		(void)signal(SIGCLD, ohandle);
		return (ret != 0) ? -1 : 0;
		}
#else
		return mkdir(dir, 0777);
#endif
	} else {
		if (js_access(env, dir, R_OK) >= 0)
			return 0;
		return js_mkdir(env, dir);
	}
}

static int
createDicFile(env, file, dpasswd, hpasswd)
WNN_ENV *env;
char *file;
char *dpasswd;
char *hpasswd;
{
	PRINT(("createDicFile(env, %s, %s, %s)\n", file,
	       dpasswd ? dpasswd : "-", hpasswd ? hpasswd : "-"));

	if (*file == C_LOCAL) {
		return js_dic_file_create_client(env, file + 1, WNN_REV_DICT,
						 NULL, dpasswd, hpasswd);
	} else {
		return js_dic_file_create(env, file, WNN_REV_DICT,
					  NULL, dpasswd, hpasswd);
	}
}

static int
createHindoFile(env, file, dicfid, hpasswd)
WNN_ENV *env;
char *file;
int dicfid;
char *hpasswd;
{
	PRINT(("createHindoFile(env, %s, %d, %s)\n", file, dicfid,
	       hpasswd ? hpasswd : "-"));

	if (*file == C_LOCAL) {
		return js_hindo_file_create_client(env, dicfid, file + 1,
						   NULL, hpasswd);
	} else {
		return js_hindo_file_create(env, dicfid, file, NULL, hpasswd);
	}
}

static int
removeFile(env, file, fid, passwd)
WNN_ENV *env;
char *file;
int fid;
char *passwd;
{
	if (fid >= 0) (void)js_file_discard(env, fid);

	if (*file == C_LOCAL) {
		return js_file_remove_client(env->js_id, file + 1, passwd);
	} else {
		return js_file_remove(env->js_id, file, passwd);
	}
}

/* getdicpasswd -- 指定されたファイルからパスワードを取り出す */
static char *
getdicpasswd(file)
char *file;	/* ファイル名 ただしパスワードが上書きされる */
{
	FILE	*fp = fopen(file, "r");
	int	len;

	PRINT(("getdicpasswd(%s)\n", file));
	/* file名の引数にパスワードを上書きする
	 * 汚いけど、まあいいか
	 */
	if (fp == NULL) return NULL;
	if (fgets(file, 256, fp) == NULL) {
		(void)fclose(fp);
		return NULL;
	}

	len = strlen(file);
	if (file[len - 1] == '\n') file[len - 1] = '\0';

	(void)fclose(fp);
	PRINT(("getdicpasswd(): password=%s\n", file));
	return file;
}

/* fileLoad -- ファイルをロードする もし必要があれば新たに作成する */
static int
fileLoad(env, cbrec, type, file, dpasswd, hpasswd,
	 dicfid, confirmp)
WNN_ENV *env;
CallbackRec *cbrec;
int type;
char *file;
char *dpasswd;
char *hpasswd;
int dicfid;
int *confirmp;
{
	int	fid;
	char	*filep;

	if (!fileExist(env, file)) {
		switch (*confirmp) {
		case NO_CREATE:
			error(cbrec, TYPE_WARNING, "setdic: %s doesn't exist",
			      file);
			return NO_LOAD;
		case CONFIRM:
			if (!doconfirm(cbrec, type, file)) {
				return NO_LOAD;
			}
			break;
		case CONFIRM1:
			if (!doconfirm(cbrec, type, file)) {
				*confirmp = NO_CREATE;
				return NO_LOAD;
			}
			*confirmp = CREATE_WITHOUT_CONFIRM;
			break;
		}
		/* ファイルを作る */
		if (filep = rindex(file, '/')) {
			char	c = *filep;
			/* ディレクトリを作る */
			*filep = '\0';
			if (makeDir(env, file) < 0) {
				error(cbrec, TYPE_ERROR,
				      "setdic: can't make directory %s",
				      file);
				return ERROR_LOAD;
			}
			*filep++ = c;
		}

		if (type == TYPE_DIC) {
			if (createDicFile(env, file, dpasswd, hpasswd) < 0) {
				error(cbrec, TYPE_ERROR,
				      "setdic: can't create dic (%s)",
				      file);
				return ERROR_LOAD;
			}
		} else {
			if (createHindoFile(env, file, dicfid, hpasswd) < 0) {
				error(cbrec, TYPE_ERROR,
				      "setdic: can't create hindo (%s)",
				      file);
				return ERROR_LOAD;
			}
		}
	}

	if (*file == C_LOCAL) {
		fid = js_file_send(env, file + 1);
	} else {
		fid = js_file_read(env, file);
	}

	if (fid < 0) {
		error(cbrec, TYPE_ERROR, "setdic: can't load %s", file);
		return ERROR_LOAD;
	}

	return fid;
}

/* expandDicPath -- 辞書のパス名の展開をする */
static char *
expandDicPath(path, env)
char *path;
WNN_ENV *env;
{
	/* 始めに先頭を展開して */
	expandTop(path);
	/* あとは @USR を展開する */
	expandUSR(path, env);
	return path;
}

static void
expandTop(path)
char *path;
{
	char	namebuf[256];
	char	tmp[256];
	char	*name;
	char	*p;
	char	*svpath = path;
	struct passwd	*pwp;
	int	local = 0;

	if (*path == C_LOCAL) {
		local = 1;
		path++;
	}

	p = NULL;

	switch (*path) {
	case '@':
		path++;
		if (!strncmp(path, "HOME", 4)) {
			if ((p = getenv("HOME")) == NULL)
				p = "";
			path += 4;
		} else if (!strncmp(path, "LIBDIR", 6)) {
			p = LIBDIR;
			path += 6;
		} else if (!strncmp(path, "WNN_DIC_DIR", 11)) {
			if ((p = getenv("HOME")) == NULL) {
				p = "";
			} else {
				(void)strcpy(tmp, p);
				(void)strcat(tmp, "/");
				if ((p = getenv("WNN_DIC_DIR")) == NULL) {
					(void)strcat(tmp, "Wnn");
				} else {
					(void)strcat(tmp, p);
				}
				p = tmp;
			}
			path += 11;
		}
		break;
	case '~':
		if (*++path == '/') {
			pwp = getpwuid(getuid());
		} else {
			char	*cp;
			char	c;

			cp = path;
			while (*cp && *cp != '/')
				cp++;
			c = *cp;
			*cp = '\0';
			pwp = getpwnam(path);
			*cp = c;
			path = cp;
		}
		if (pwp == NULL)
			p = "";
		else
			p = pwp->pw_dir;
		break;
	}

	if (p != NULL) {
		if (strlen(path) + strlen(p) + local > 255) {
			return;
		}
		name = namebuf;
		if (local) *name++ = C_LOCAL;
		(void)strcpy(name, p);
		(void)strcat(name, path);
		(void)strcpy(svpath, namebuf);
	}
}

static void
expandUSR(path, env)
char *path;
WNN_ENV *env;
{
	static char	myname[10];
	register char	*t;
	int		mynamelen;
	char		*envname;
	int		envnamelen;

	/* @USR の展開をする */
	if (myname[0] == '\0') {
		(void)strcpy(myname, getpwuid(getuid())->pw_name);
	}
	mynamelen = strlen(myname);
	t = path;
	while (t = strinc(t, "@USR")) {
		if (strlen(path) + mynamelen - 4 > 255) return;

		(void)mybcopy(t + 4, t + mynamelen, strlen(t + 4) + 1);
		(void)mybcopy(myname, t, mynamelen);
	}

	/* @ENV の展開をする */
	envname = NULL;
	t = path;
	while (t = strinc(t, "@ENV")) {
		if (envname == NULL) {
			envname = getEnvName(env);
			if (envname == NULL) envname = "";
			envnamelen = strlen(envname);
		}
		if (strlen(path) + envnamelen - 4 > 255) return;

		(void)mybcopy(t + 4, t + envnamelen, strlen(t + 4) + 1);
		(void)mybcopy(envname, t, envnamelen);
	}
}

static char *
strinc(s1, s2)
char *s1;
char *s2;
{
	int	c = *s2;
	int	len = strlen(s2);

	while (s1 = index(s1, c)) {
		if (!strncmp(s1, s2, len))
			return s1;
		s1++;
	}
	return NULL;
}

/* parseLine -- コマンド行のパージング */
static int
parseLine(line, argv, argsize)
char *line;
char **argv;
int argsize;
{
	int	argc = 0;

	while (argc < argsize) {
		/* 空白のスキップ */
		while (*line <= ' ') {
			if (*line == '\0' || *line == '\n')
				return argc;
			line++;
		}
		argv[argc++] = line++;
		while (*line++ > ' ')
			;
		line--;
		if (*line == '\0' || *line == '\n') {
			*line = '\0';
			return argc;
		}
		*line++ = '\0';
	}
	return argc;
}


/* VARARGS3 */
static void
error(cbrec, level, s, a, b)
CallbackRec *cbrec;
int level;
char *s;
char *a, *b;
{
	char	tmp[1024];
	void (*callback)() = cbrec->errorcall;

	(void)sprintf(tmp, s, a, b);

	if (callback) {
		(*callback)(level, tmp, cbrec->client_data);
	} else {
		fprintf(stderr, "%s: %s\n",
			level == TYPE_ERROR ? "Error" : "Warning", tmp);
	}
}

static int
doconfirm(cbrec, type, file)
CallbackRec *cbrec;
int type;
char *file;
{
	int	(*callback)() = cbrec->confirmcall;

	return (callback ? (*callback)(type, file, cbrec->client_data) : 1);
}

/*
 *	パブリックなファンクション
 */

/* jiOpenServer -- サーバをオープンする */
WNN_JSERVER_ID *
jiOpenServer(servername, timeout)
char *servername;
int timeout;
{
	extern char	*getenv();

	/*
	 * servername が NULL または空文字列だった場合は、
	 * 環境変数 'JSERVER' を使用する
	 */
	if (servername == NULL || *servername == '\0') {
		servername = getenv(WNN_JSERVER_ENV);
	}
	PRINT(("jiOpenServer(): servername=%s\n",
	       servername ? servername : "<NULL>"));
	if (servername == NULL) servername = "unix";	/* unix domain */
	return useServer(servername, timeout);
}

/* jiCloseServer -- サーバをクローズする */
int
jiCloseServer(server)
WNN_JSERVER_ID *server;
{
	PRINT(("jiCloseServer(): servername=%s\n",
	       server->js_name));
	return unuseServer(server);
}

/* jiCreateEnv -- 指定された環境を作る */
WNN_ENV *
jiCreateEnv(server, envname, override, wnnrcfile, errmsgfunc, confirmfunc, client_data)
WNN_JSERVER_ID *server;
char *envname;
int override;
char *wnnrcfile;
void (*errmsgfunc)();
int (*confirmfunc)();
caddr_t client_data;
{
	int		noInitialize = 0;
	WNN_ENV		*env;
	int		confirm;
	int		saveflag;
	CallbackRec	callbackrec;
	char		buf[256];

	callbackrec.errorcall = errmsgfunc;
	callbackrec.confirmcall = confirmfunc;
	callbackrec.client_data = client_data;

	/*
	 * envname が空文字列 (i.e. *envname == '\0') だった場合には
	 * ユーザ名を使う (この辺は uum に合わせてある)
	 * envname が NULL の場合には NULL のまま
	 */
	if (envname != NULL && *envname == '\0') {
		envname = getpwuid(getuid())->pw_name;
	}
	PRINT(("jiCreateEnv(): envname=%s\n", envname ? envname : "<NULL>"));

	/* すでに作られているかどうか調べる */
	env = findEnv(server, envname);
	if (env != NULL && !override) {
		return env;
	}

	/* とりあえず jserver が死んでいるか調べる */
	if (server->js_dead) {
		error(&callbackrec, TYPE_ERROR, "JSERVER is DEAD");
		wnn_errorno = WNN_JSERVER_DEAD;
		return NULL;
	}

	/* jserver が死んだ時の処理
	 * longjmp で戻ってくるところを設定する
	 * こうしておくことで、以降 jserver が死んだかどうかのチェックが
	 * 不要になる
	 * ただし、上位の関数ですでに設定されているときにはそれに任せる
	 */
	if ((saveflag = server->js_dead_env_flg) == 0) {
		server->js_dead_env_flg = 1;
		if (setjmp(server->js_dead_env)) {
			error(&callbackrec, TYPE_ERROR, "JSERVER is DEAD");
			server->js_dead_env_flg = 0;
			wnn_errorno = WNN_JSERVER_DEAD;
			return NULL;
		}
	}

	if (!override && envExist(server, envname) == 1) {
		/* すでに指定された名前の環境が存在する */
		PRINT(("jcCreateEnv(): %s already exists\n", envname));
		noInitialize = 1;
	}

	/* 環境を作る */
	if ((env = js_connect(server, envname)) == NULL) {
		error(&callbackrec, TYPE_ERROR, "can't conenct to environment");
		server->js_dead_env_flg = saveflag;
		return NULL;
	}

	/*
	 * wnnrcfile が空文字列 (i.e. *wnnrcfile == '\0') だった場合には
	 * まず環境変数 'WNNENVRC' を使う
	 * それもなければデフォルトの /usr/local/lib/wnn/wnnenvrc を
	 * 使う
	 * wnnrcfile が NULL の場合には初期化は行なわない
	 */

	if (noInitialize || wnnrcfile == NULL) {
		/* 環境リストに入れておく */
		addEnv(envname, env);
		server->js_dead_env_flg = saveflag;
		return env;
	}

	if (*wnnrcfile == '\0') {
		if ((wnnrcfile = getenv("WNNENVRC")) == NULL ||
		    access(wnnrcfile, R_OK) < 0) {
			/* use default file defined in 'config.h' */
#if JSERVER_VERSION > 0x4030
			(void)strcpy(buf, LIBDIR);
			(void)strcat(buf, "/ja_JP");
			(void)strcat(buf, ENVRCFILE);
			if (access(buf, R_OK) < 0) {
				(void)strcpy(buf, LIBDIR);
				(void)strcat(buf, ENVRCFILE);
			}
#else
			(void)strcpy(buf, ENVRCFILE);
#endif
			wnnrcfile = buf;
		}
	}
	PRINT(("jiCreateEnv(): wnnrcfile=%s\n", wnnrcfile));

	/* 初期化する */
	confirm = CONFIRM;
	if (readEnvFile(env, wnnrcfile, &callbackrec, &confirm) < 0) {
		(void)js_disconnect(env);
		server->js_dead_env_flg = saveflag;
		return NULL;
	}

	/* 環境リストに入れておく */
	addEnv(envname, env);

	/* 元に戻しておく */
	server->js_dead_env_flg = saveflag;

	return env;
}

/* jiDeleteEnv -- 環境を消す */
int
jiDeleteEnv(env)
WNN_ENV *env;
{
	PRINT(("jiDeleteEnv()\n"));
	return deleteEnv(env);
}

/*
 *	環境設定のファンクション
 */

static int
readEnvFile(env, file, cbrec, confirmp)
WNN_ENV *env;
char *file;
CallbackRec *cbrec;
int *confirmp;
{
	char	line[512];
	FILE	*fp;
	FILE	*fopen();
	int	argc;
	char	*argv[MAXARG];
	int	abort;

	if ((fp = fopen(file, "r")) == NULL) {
		error(cbrec, TYPE_ERROR, "can't open %s", file);
		return -1;
	}

	while (fgets(line, sizeof(line), fp)) {
		if (*line == '\0' || *line == '\n' || *line == ';')
			continue;
		PRINT(("readEnvFile(): doing line %s", line));

		/* parsing */
		argc = parseLine(line, argv, MAXARG);
		if (argc < 1)
			continue;

		abort = 0;
		if (!strcmp(argv[0], "include")) {
			abort = procInclude(env, cbrec, confirmp, argc, argv);
		} else if (!strcmp(argv[0], "setdic")) {
			abort = procSetDic(env, cbrec, confirmp, argc, argv);
		} else if (!strcmp(argv[0], "setfuzokugo")) {
			abort = procSetFuzokugo(env, cbrec, argc, argv);
		} else if (!strcmp(argv[0], "setparam")) {
			abort = procSetParam(env, cbrec, argc, argv);
		} else if (!strcmp(argv[0], "confirm")) {
			*confirmp = CONFIRM;
		} else if (!strcmp(argv[0], "confirm1")) {
			*confirmp = CONFIRM1;
		} else if (!strcmp(argv[0], "create_without_confirm")) {
			*confirmp = CREATE_WITHOUT_CONFIRM;
		} else if (!strcmp(argv[0], "no_create")) {
			*confirmp = NO_CREATE;
		}
		if (abort) {
			(void)fclose(fp);
			return -1;
		}
	}
	(void)fclose(fp);
	return 0;
}

/*
 * コマンド処理ルーチン
 *
 *	procInclude()		include の処理
 *	procSetDic()		setdic の処理
 *	procSetFuzokugo()	setfuzokugo の処理
 *	procSetParam()		setparam の処理
 */

static int
procInclude(env, cbrec, confirmp, ac, av)
WNN_ENV *env;
CallbackRec *cbrec;
int *confirmp;
int ac;
char **av;
{
	char	tmp[256];

	if (ac < 2) {
		error(cbrec, TYPE_ERROR, "include: filename expected");
		return -1;
	}

	(void)strcpy(tmp, av[1]);
	expandTop(tmp);
	return readEnvFile(env, tmp, cbrec, confirmp);
}

static int
procSetDic(env, cbrec, confirmp, ac, av)
WNN_ENV *env;
CallbackRec *cbrec;
int *confirmp;
int ac;
char **av;
{
	char	dicfilebuf[256], hindofilebuf[256];
	char	dicpasswdbuf[256], hindopasswdbuf[256];
	char	*dicfilename, *hindofilename;
	int	prio, dicro, hindoro, rev;
	int	dicid, hindoid;
	char	*dicpasswd, *hindopasswd;

	if (ac < 2) {
		error(cbrec, TYPE_ERROR, "setdic: dic filename expected");
		return -1;
	}
	
	(void)strcpy(dicfilebuf, av[1]);
	dicfilename = expandDicPath(dicfilebuf, env);

	/* デフォルトの値 */
	hindofilename = NULL;
	prio = 5;
	dicro = hindoro = 0;
	rev = 0;
	dicpasswd = hindopasswd = NULL;

	if (ac >= 3 && strcmp(av[2], "-")) {
		(void)strcpy(hindofilebuf, av[2]);
		hindofilename = expandDicPath(hindofilebuf, env);
	}
	if (ac >= 4) {
		prio = atoi(av[3]);
	}
	if (ac >= 5) {
		dicro = atoi(av[4]);
	}
	if (ac >= 6) {
		hindoro = atoi(av[5]);
	}
	if (ac >= 7 && strcmp(av[6], "-")) {
		(void)strcpy(dicpasswdbuf, av[6]);
		dicpasswd = getdicpasswd(expandDicPath(dicpasswdbuf, env));
		/* パスワードが読めなくても、エラーにはしない */
		if (dicpasswd == NULL) {
			error(cbrec, TYPE_WARNING,
			      "can't read password for %s", dicfilename);
		}
	}
	if (hindofilename && ac >= 8 && strcmp(av[7], "-")) {
		(void)strcpy(dicpasswdbuf, av[7]);
		hindopasswd = getdicpasswd(expandDicPath(hindopasswdbuf, env));
		/* パスワードが読めなくても、エラーにはしない */
		if (hindopasswd == NULL) {
			error(cbrec, TYPE_WARNING,
			      "can't read password for %s", hindofilename);
		}
	}
	if (ac >= 9) {
		rev = atoi(av[8]);
	}

	dicid = hindoid = -1;

	if ((dicid = fileLoad(env, cbrec, TYPE_DIC, dicfilename,
			      dicpasswd, hindopasswd, -1, confirmp)) == ERROR_LOAD) {
		return -1;
	} else if (dicid == NO_LOAD) {
		return 0;
	}

 re_create:
	if (hindofilename) {
		/* 頻度ファイルが指定されている時、
		 * confirm されないために頻度ファイルが作られなかったならば
		 * 単に辞書を使わないだけ
		 * confirm されたにもかかわらず作られなかったならば
		 * エラーでアボートする
		 */
		hindoid = fileLoad(env, cbrec, TYPE_HINDO,
				   hindofilename, NULL, hindopasswd,
				   dicid, confirmp);
		if (hindoid == ERROR_LOAD) {
			(void)js_file_discard(env, dicid);
			return -1;
		} else if (hindoid == NO_LOAD) {
			return 0;
		}
	}

	if (js_dic_add(env, dicid, hindoid, rev, prio, dicro, hindoro,
		       dicpasswd, hindopasswd) < 0) {
		/*
		 * もし、辞書と頻度ファイルがマッチしなかったときには、
		 * いきなりエラーにせず、頻度ファイルを作り直すかどうか
		 * 尋ねるようにする
		 */
		if (wnn_errorno == WNN_HINDO_NO_MATCH) {
			int	retry = 0;

			error(cbrec, TYPE_WARNING,
			      "setdic: hindo (%s) doesn't match with dic (%s)",
			      hindofilename, dicfilename);

			switch (*confirmp) {
			case NO_CREATE:
				break;
			case CONFIRM:
				if (doconfirm(cbrec, TYPE_HINDO,
					      hindofilename)) {
					retry = 1;
				}
				break;
			case CONFIRM1:
				if (doconfirm(cbrec, TYPE_HINDO,
					      hindofilename)) {
					*confirmp = CREATE_WITHOUT_CONFIRM;
					retry = 1;
				} else {
					*confirmp = NO_CREATE;
				}
				break;
			default:	/* CREATE_WITHOUT_CONFIRM */
				retry = 1;
				break;
			}

			/* 一度頻度ファイルを消して、もう一度ロードする */
			if (retry) {
				if (removeFile(env, hindofilename,
					       hindoid, hindopasswd) < 0) {
					error(cbrec, TYPE_ERROR,
					      "setdic: can't remove hindo (%s)",
					      hindofilename);
				} else {
					goto re_create;
				}
			}
		}
		error(cbrec, TYPE_ERROR, "setdic: can't add dic (%s)",
		      dicfilename);
		return -1;
	}

	return 0;
}

static int
procSetFuzokugo(env, cbrec, ac, av)
WNN_ENV *env;
CallbackRec *cbrec;
int ac;
char **av;
{
	char	filebuf[256];
	int	fid;

	if (ac < 2) {
		error(cbrec, TYPE_ERROR, "setfuzokugo: filename expected");
		return -1;
	}
	
	(void)strcpy(filebuf, av[1]);
	(void)expandDicPath(filebuf, env);

	if (*filebuf == C_LOCAL) {
		fid = js_file_send(env, filebuf + 1);
	} else {
		fid = js_file_read(env, filebuf);
	}
	if (fid < 0) {
		error(cbrec, TYPE_ERROR, "setfuzokugo: can't load %s", filebuf);
		return -1;
	}

	if (js_fuzokugo_set(env, fid) < 0) {
		error(cbrec, TYPE_ERROR, "setfuzokugo: can't set fuzokugo-file");
		return -1;
	}

	return 0;
}

static int
procSetParam(env, cbrec, ac, av)
WNN_ENV *env;
CallbackRec *cbrec;
int ac;
char **av;
{
	struct wnn_param	param;

	if (ac < 2) {
		error(cbrec, TYPE_ERROR, "setparam: no parameter");
		return -1;
	}

	/* 指定されたパラメータの数が少ないときは、指定されてない
	 * パラメータの値は現在のパラメータと同じにする
	 */
	if (ac < 18 && js_param_get(env, &param) < 0) {
		error(cbrec, TYPE_ERROR,
		      "setparam: can't get default value");
		return -1;
	}

	/* パラメータの値の代入 */
	switch (ac) {
	case 18: param.p15 = atoi(av[17]);
	case 17: param.p14 = atoi(av[16]);
	case 16: param.p13 = atoi(av[15]);
	case 15: param.p12 = atoi(av[14]);
	case 14: param.p11 = atoi(av[13]);
	case 13: param.p10 = atoi(av[12]);
	case 12: param.p9 = atoi(av[11]);
	case 11: param.p8 = atoi(av[10]);
	case 10: param.p7 = atoi(av[9]);
	case  9: param.p6 = atoi(av[8]);
	case  8: param.p5 = atoi(av[7]);
	case  7: param.p4 = atoi(av[6]);
	case  6: param.p3 = atoi(av[5]);
	case  5: param.p2 = atoi(av[4]);
	case  4: param.p1 = atoi(av[3]);
	case  3: param.nsho = atoi(av[2]);
	case  2: param.n = atoi(av[1]);
	}

	if (js_param_set(env, &param) < 0) {
		error(cbrec, TYPE_ERROR, "setparam: can't set parameters");
		return -1;
	}

	return 0;
}
