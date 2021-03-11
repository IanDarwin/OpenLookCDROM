/* $Id: HZtable.h,v 3.2 1994/06/06 12:28:18 ygz Exp $ */

/***********************************************************************
* Copyright 1994 by Yongguang Zhang.

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the names of the authors not
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

THE AUTHOR(S) DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN
NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
USE OR PERFORMANCE OF THIS SOFTWARE.

******************************************************************/

/* HZtable.h */

/*
 * Data Structure of the External Chinese Input Table
 *
 * An external input table (CIT) contains:
 *	1) interface definition (keys definition, prompts, ...)
 *	2) input lookup data structure
 *
 * Trie Table:
 *	list of Trie nodes.
 *
 * HZ Table:
 *	2 bytes character sets.
 *
 */

#ifndef	_HZTABLE_H_
#define	_HZTABLE_H_

/*
 * The only imported definition is XChar2b, from <X11/Xlib.h>.
 * To advoid including the tremendous amount of X11 include files,
 * this definition is simply copied here.
 */
#ifndef	_XLIB_H_
typedef struct {		/* normal 16 bit characters are two bytes */
    unsigned char byte1;
    unsigned char byte2;
} XChar2b;
#endif

#define	MAX_PROMPT	24	/* maximum prompt in HZ input erea */
#define MAX_KEYPROMPT	4	/* maximum bytes to display each input key */
#define MAX_CHOICE	16	/* maximum number of choices on one screen */

typedef	struct _keyPrompt {
    unsigned char	prompt[MAX_KEYPROMPT];	/* prompt string */
    unsigned short	ptlen;			/* len of the prompt */
} keyPrompt;

/* Trie Node.  (so organized to optimize access speed) */
typedef	struct _trieNode {
    unsigned char	tn_key;		/* this input key */
    unsigned char	tn_numNextKeys;	/* number of possible next input key */
    unsigned short int	tn_numHZchoice;	/* number of HZ choices */
    unsigned int	tn_nextKeys;	/* table for further input key */
    unsigned int	tn_hzidx;	/* index to HZ list */
} trieNode;

/****************** The external input table ******************/

typedef struct _HZinputTable {
    unsigned char	version;	/* version num of this .cit format */
    char		encode;		/* Chinese encoding: GB or BIG5 */
    unsigned char	builtin;	/* this table is builtin? */

    unsigned int	sizeTrieList;	/* size of the whole TRIE list */
    unsigned int	sizeHZList;	/* size of the whole HZ list */
    trieNode		*trieList;	/* start address of the Trie list */
    XChar2b		*hzList;	/* start address of the HZ list */

    unsigned char	autoSelect;	/* auto selection if single choice? */
    unsigned short	keytype[128];	/* valid type of the key */
    keyPrompt		keyprompt[128];	/* display this for the key */

    char		choicelb[MAX_CHOICE];	/* HZ choice label */
    int			maxchoice;	/* maximum number of HZ choice */
    unsigned char	prompt[MAX_PROMPT];
    int			lenPrompt;		/* len of the Prompt */

} HZinputTable;

/*
 * valid type or type mask of a key
 * mask can be OR -- possible for a key to be both input key and selection key
 *
 *	........
 *	   1#### <--- SELECTION_MASK
 *	  1      <--- INPUT_MASK
 *
 *	  11#### <--- both INPUT and SELECTION
 *	  100000 <--- INPUT only
 *	  1010   <--- CONJOIN
 *	   0 1   <--- WILD_MASK
 *	  1001 0 <--- WILDCARD ('*')
 *	  1001 1 <--- WILDCHAR ('?')
 *
 *	01     0 <--- BACKSPACE
 *	01     1 <--- KILL
 *	10     0 <--- RIGHT
 *	10     1 <--- LEFT
 *	11     0 <--- REPEAT
 *
 * HZ_KEY_SELECTION_NUM must be (MAX_CHOICE - 1)
 */
#define	HZ_KEY_INVALID		0	/* not used to input HZ */
#define	HZ_KEY_SELECTION_MASK	0x10	/* used to select a choice */
#define	HZ_KEY_SELECTION_NUM	0x0f	/* num to select a choice */
#define	HZ_KEY_INPUT_MASK	0x20	/* used to convert to HZ */
#define	HZ_KEY_CONJOIN		0x28	/* conjoin key for continuous input */
#define	HZ_KEY_WILD_MASK	0x04	/* used to identify a wild key */
#define	HZ_KEY_WILDCARD		0x24	/* wildcard (*) for global matching */
#define	HZ_KEY_WILDCHAR		0x25	/* wildchar (?) for global matching */
#define HZ_KEY_BACKSPACE	0x40	/* edit key: backspace */
#define HZ_KEY_KILL		0x41	/* edit key: clear input buffer */
#define HZ_KEY_RIGHT		0x80	/* select key: more at right */
#define HZ_KEY_LEFT		0x81	/* select key: more at left */
#define HZ_KEY_REPEAT		0xc0	/* repeat key: repeat previous input */

/*
 * HZ_PHRASE_TAG -- if the first byte of a HZ (2-bytes) is this value,
 *	the second byte is an integer n and means that the following n HZ
 *	are a multi-HZ phrase.
 */
#define	HZ_PHRASE_TAG	'\001'	/* if a HZ's first byte is this => a phrase */

/*
 * The magic number and the version number of a .cit file.
 */
#define	MAGIC_CIT	"HZ"
#define	CIT_VERSION	2
#define	CIT_SUFFIX	".cit"
#define	TIT_SUFFIX	".tit"

/*
 * Encodings that are understood by cxterm.
 */
#define	GB_ENCODE	0
#define	BIG5_ENCODE	1
#define	JIS_ENCODE	2
#define	KS_ENCODE	3
#define	UNKNOWN_ENCODE	(-1)

#endif /* _HZTABLE_H_ */
