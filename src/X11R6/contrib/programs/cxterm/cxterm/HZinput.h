/*
 *	$Id: HZinput.h,v 3.0 1994/06/04 07:39:38 ygz Exp $
 */

/***********************************************************
Copyright 1991, 1992, 1994 by Yongguang Zhang.  All Rights Reserved.

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of the authors not
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

THE AUTHOR(S) DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
THE AUTHOR(S) BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

******************************************************************/

#ifndef _HZINPUT_H_
#define _HZINPUT_H_

/*
 * types imported from xterm (ptyx.h):
 *	typedef	unsigned char Char;
 *	typedef	struct { ... } TScreen;
 *	typedef	struct { ... } Misc;
 *
 * types imported from X window:
 *	typedef ... Boolean;
 */

#include "ptyx.h"
#include "HZtable.h"
#include <X11/Xos.h>

#ifndef X_NOT_STDC_ENV
#include <stdlib.h>
#else
extern char *malloc(), *calloc(), *realloc();
extern char *getenv();
#endif


#define MAX_HZIM	24	/* maxinum number of input methods allowed */
#define MAX_INBUF	32	/* maximum length of input conversion buffer */

/******************** INPUT METHOD ********************/

typedef enum {
    im_type_None,	/* no type */
    im_type_Builtin,	/* Builtin */
    im_type_Simple,	/* Simple input method (e.g. trie) */
} HZimType;

typedef struct _HZInputMethod {

    HZimType type;
    HZinputTable *hztbl;	/* for key definition */
    Char *prompt;		/* hztbl will be NULL in builtin case */
    int lenPrompt;
    union {
	struct {
	    HZinputTable *im_InputTable;
	} im_Simple;
    } u;

} HZInputMethod;

#define	im_hztbl	u.im_Simple.im_InputTable

/******************** ASSOCIATION LIST ********************/

#define MAX_NUM_HZ	(1 << 15)

typedef	struct _AssocList {
    Char prompt[MAX_PROMPT+1];	/* to indicate the association input */
    XChar2b *phrases;		/* the long string of hanzi phrases */
    int idxtbl[ MAX_NUM_HZ ];	/* index to the subset of phrases */
} AssocList;

/******************** INPUT CONTEXT ********************/

/*
 * An input context is a data structure used in matching the keystroke against
 * a certain input method.
 */

typedef enum {
    ic_type_None,	/* no type */
    ic_type_Builtin,	/* Builtin */
    ic_type_Trie,	/* TRIE */
    ic_type_Assoc,	/* Association List */
} HZicType;

/* A simple input method can be implemented as either trie or rep-code list */
#define	IC_PREFER	ic_type_Trie	/* prefered implementation */

/* wildcard sub-context */
typedef  struct _HZwildcardContext {
    char wildcard[MAX_INBUF+1];	/* inbuf suffix with wildcard */
    char repcode[MAX_INBUF+1];	/* the repcode suffix */

    int depth;			/* the current depth in the DFS traversal */
    trieNode *tNstack[MAX_INBUF+1];	/* the trieNode stack */
    short int tNnumSb[MAX_INBUF+1];	/* number of sibling trieNode */

} HZwildcardContext;


typedef struct _HZInputContext {

    HZicType type;

    HZInputMethod *im;

    char *keysq;	/* the input key-stroke sequence buffer */
    int matched;	/* how many of the keysq have been matched */
    int pending;	/* how many have to be search later */

    union {

/* Trie */

	struct {
	    HZinputTable *ic_HZtable;
	    trieNode *ic_TrieNodePtr;	/* trieNode of the matched inbuf */
	    XChar2b *ic_hzListPtr;	/* HZ choices to be selected */
	    HZwildcardContext* ic_WcContext;
	} ic_Trie;
#	define	tHZtbl		u.ic_Trie.ic_HZtable
#	define	tCurTNptr	u.ic_Trie.ic_TrieNodePtr
#	define	tCurHZptr	u.ic_Trie.ic_hzListPtr
#	define	tWcCntx		u.ic_Trie.ic_WcContext

/* Association */

	struct {
	    XChar2b *ic_BeginList;
	    XChar2b *ic_PtrList;
	} ic_Assoc;
#	define	aBeginList	u.ic_Assoc.ic_BeginList
#	define	aPtrList	u.ic_Assoc.ic_PtrList

    } u;

    int totalChoice;		/* total number of choices found */
    int availChoice;		/* number of choices given */

    struct _HZInputContext *next_free;		/* free list */

} HZInputContext;

#define IC_FAIL 0
#define IC_OK   1

/******************** CHOICE LIST ********************/

/*
 * A choice list is a list of pointers to HZ (phrase) candidates.
 * A continuous segment of the list is call the on-screen selection window.
 * It is a sliding window of the choice list, which is displayed in the
 * input area.  A user can move it forward (to the right) or backward
 * (to the left) using some keys.  The size of the selection window
 * depends on the width of the input area and the length of each choice.
 * When the selection window reaches the right end of the choice list,
 * cxterm tries to expend the choice list by getting new choices from
 * the current input context.  A choice list is exhausted if there
 * will be no more choice in the current input context.
 */

struct hz_choice {
    XChar2b *hzc;		/* pointer to the HZ choice */
    short int nhz;		/* number of HZ in the choice (if a phrase) */
};

typedef struct _HZChoiceList {

    int numChoices;		/* the number of choices in the choice list */
    Boolean exhaust;		/* the list is exhausted, no more choice */
    struct hz_choice *choices;	/* the list of choices (hz or hz phrases) */
    int num_alloc;		/* the maximum number of records in the list */

    int selPos;			/* position of on-screen selection window */
    int selNum;			/* the number of on-screen selections */

} HZChoiceList;

#define	HZclEmpty(cl)	((cl)->numChoices == 0)

/******************** INPUT AREA ********************/

/*
 * The input area has 4 portions: prompt, input buffer, status,
 * and selection list, in the following layout:
 *
 * 		------------------------------------------
 * line 1:	<prompt><input_buffer>            <status>
 * line 2:	<selection_list or message>
 *
 * The prompt, status, and selection list are usually redrawed,
 * but the input buffer can be partially updated.
 */

enum iaRefreshType { IA_Unchange, IA_Redraw, IA_Update, IA_Increment };

#define	MAX_IA_WIDTH	256	/* the input area can be at most this wide */
#define MAX_DPYINBUF    ( MAX_IA_WIDTH - MAX_PROMPT )
#define	MAX_STATUS	10	/* the input area can be at most this wide */

typedef struct _HZInputArea {

    Char *prompt;			/* prompt of the input method */
    int lenPrompt;			/* length of prompt */
    Char dpyInbuf[MAX_DPYINBUF];	/* user input keys to be displayed */
    int dpyInbufLen;			/* length of dpyInbuf[] */
    Char dpyStatus[MAX_STATUS];		/* status string to be displayed */
    int dpyStatusLen;			/* length of dpyStatus[] */

    int maxchoice;		/* the maximum number of selections allowed */
    char *choicelb;		/* the labels for the selections */
    keyPrompt *keyprompt;	/* the echo labels for the keystroke */

    Char dpySelStr[MAX_IA_WIDTH];	/* selection string to be displayed */
    int dpySelStrLen;			/* length of dpySelStr */
    int maxlen;				/* maximum dpySelStrLen (== max_col) */

    enum iaRefreshType prompt_mark;	/* refresh flag for prompt */
    enum iaRefreshType inbuf_mark;	/* refresh flag for input buffer */
    enum iaRefreshType status_mark;	/* refresh flag for status */
    enum iaRefreshType selection_mark;	/* refresh flag for selection list */

    int last_dpyInbufLen;	/* previous value of dpyInbufLen */
    int last_dpyStatusLen;	/* previous value of dpyStatusLen */
    int last_dpySelStrLen;	/* previous value of dpySelStrLen */

    char inbuf[MAX_INBUF];	/* current input buffer */
    int inbufCount;		/* current input buffer length */

} HZInputArea;

/* width of an n-HZ (phrase) selection on screen (3 is the len of " l.") */
#define	SelWidthOnScr(n)	(n)*2+3

/* the width "w" of the selection string for display is too long? */
#define	OverflowSelStr(w,ia,cl)	(((cl)->selNum >= 1) && ((w) > (ia)->maxlen))


/******************** INPUT MODULE ********************/

typedef struct _CXtermInputModule {

    TScreen *screen;
    Misc *misc;
    int encode;			/* hanzi encoding */

    int numHZim;		/* number of HZ input methods loaded */
    struct {
	char *name;		/* name of the input method */
	HZInputMethod hzim;	/* input method definition */
	int (*hzif)();		/* input filter (builtin or generic) */
    } imtbl[MAX_HZIM];

    int mode;			/* current HZ mode (HZ input method number) */
    HZInputMethod *chzim;	/* current HZ input method pointer */
    int (*chzif)();		/* current HZ input filter function pointer */

    HZChoiceList  hzcl;		/* the choice-list */
    HZInputArea  hzia;		/* input area data structure */
				/* (including the on-screen selection list) */

    AssocList *assocList;

    char hzinbuf[MAX_INBUF];	/* input buffer for keystrokes */
    int hzinbufCount;		/* lenght of keystroks buffer */

    /* cache to facilitate incremental input search */
    HZInputContext *cache_ics[MAX_INBUF];
    HZInputContext *cur_ic;	/* the current input context */

    /* history */
    struct {
	int	inbuflen;
	char	inbuf[MAX_INBUF];
	HZInputContext *ics[MAX_INBUF];
    } history;

} CXtermInputModule;

/******************** EXTERNAL FUNCTIONS ********************/

extern CXtermInputModule cxtermInput;

/* HZinMthd.c */
extern  void HZimInit();
extern  void HZimReset();
extern  void HZimCleanUp();
extern	void HZsaveHistory();
extern	void HZrestoreHistory();
extern  void HZswitchModeByNum();
extern  int HZLoadInputMethod();

/* HZchList.c */
extern	void HZclInit();
extern	void HZclReset();
extern	void HZclCleanUp();
extern	short int HZclSelect();
extern	 int HZclMakeSelection();
extern	 int HZclMoveForward();
extern	 int HZclMoveBackward();

/* HZinArea.c */
extern  void HZiaInit();
extern  void HZiaCleanUp();
extern  void HZiaSetFrame();
extern  void HZiaSet();
extern  void HZiaSetChoiceList();
extern  void HZiaSetInBuf();
extern  void HZiaSetStatus();
extern  void HZiaResize();
extern  void HZiaRedraw();
extern  void HZiaRefresh();
extern  void HZiaShowMesg();

/* HZfilter.c */
extern	int hzTableFilter();
extern  void HZLoadBuiltin();

/* HZinSrch.c */
extern HZInputContext *HZinputSearch();
extern HZInputContext *HZassocSearch();
extern int HZrestartIC();
extern short int HZgetNextIC();
extern void InitICpool();
extern void FreeIC();

/* HZtrie.c */
extern  int Trie_Match();
extern  int Trie_Restart();
extern  short int Trie_GetNext();

/* HZassoc.c */
extern int Assoc_Match();
extern int Assoc_Restart();
extern short int Assoc_GetNext();

/* HZload.c */
extern int LoadCIT();
extern void UnloadCIT();
extern AssocList *HZassocLoad();

#endif /* _HZINPUT_H_ */
