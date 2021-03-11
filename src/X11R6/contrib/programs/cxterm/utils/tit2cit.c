/*
 *	$Id: tit2cit.c,v 2.1 1994/06/06 06:46:32 ygz Exp $
 */

/***********************************************************************
Copyright 1994 Yongguang Zhang.  All Rights Reserved

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

************************************************************************/

/* tit2cit.c */

/* TIT constructs:
 *
 *	ENCODE: [GB|BIG5|JIS|KS]			encoding
 *	AUTOSELECT: [YES|NO]
 *	PROMPT: <string>
 *	VALIDINPUTKEY: <keys>
 *	WILDCARDKEY: <keys>				key *
 *	WILDCHARKEY: <keys>				key ?
 *	SELECTKEY: <keys> <keys> ...
 *	DELETEALL: <keys>				key kill
 *	MOVERIGHT: <keys>				prev choices
 *	MOVELEFT: <keys>				next choices
 *	REPEATKEY: <keys>				previous selection
 *	KEYPROMPT(<key>): <string>
 *	COMMENT <anything>
 *	BEGINDICTIONARY
 *	BEGINPHRASE
 */

#include <X11/Xos.h>	/* OS dependent stuff */
#include <ctype.h>
#include <stdio.h>

extern char *malloc(), *calloc(), *realloc();

extern int HZencode();		/* from HZutil.o */

#include "HZtable.h"

/*
 * Dynamic structure dynTrie is different with the TRIE structures
 * defined in HZtable.h, which is static.
 *
 * When we read the file line by line, keys-HZs mapping are inserted
 * into the dynTrie.  After all lines are finished, the dynTrie is
 * reorganized (optimized and linearized) into a static TRIE structures,
 * which can be loaded easily by cxterm.
 *
 * Phrase (string of multiple hanzi) is embedded into hanzi strings.
 * A phrase is start with 2 bytes: (1, n), where n is the length
 * of the phrase in byte.  { The tag (0, n) is not good for strcpy, etc. }
 */


typedef struct _dynTrie {
    unsigned char key;		/* the input key */
    char *hzptr;		/* HZ strings */
    struct _dynTrie *next;	/* next node in the same level */
    struct _dynTrie *son;	/* next node down one level */
} dynTrie;

static dynTrie rootTrie = {
    '\0', NULL, (struct _dynTrie *)NULL, (struct _dynTrie *)NULL
};
static dynTrie *root = &rootTrie;	/* root pointer to the dynTrie */

trieNode *trieList;		/* all Tries will be linearized to put here */
XChar2b *hzList;		/* all HZ codes will be put here */
unsigned int pTrieList;		/* pointer to Trie List */
unsigned int pHZList;		/* pointer to HZ List */

HZinputTable hzInputTable;

static char buffer[256];
static char keysbuf[32];
static char strbuf[256];
static int selection = 0;
static int lineno = 0;
static int totalHZbytes = 0;	/* total bytes of HZs */
static int totalTrieNode = 1;	/* total # of Trie nodes (including root) */

static int wd = 0;		/* explicitly define wildcard keys? */
static int wr = 0;		/* explicitly define wildchar keys? */
static int sl = 0;		/* explicitly define selection keys? */
static int bs = 0;		/* explicitly define backspace keys? */
static int kl = 0;		/* explicitly define kill keys? */
static int mr = 0;		/* explicitly define moveR keys? */
static int ml = 0;		/* explicitly define moveL keys? */
static int rp = 0;		/* explicitly define repeat keys? */

#define MAXCLEN	10240		/* 10K comment, large enough? */
static char comment[MAXCLEN];
static unsigned int clen = 0;

main(argc, argv)
    int argc;
    char *argv[];
{
    FILE *ifile, *ofile;

    if (argc == 1) {
	ifile = stdin;
	ofile = stdout;
    } else {
	ifile = fopen (argv[1], "r");
	if (! ifile) {
	    perror (argv[1]);
	    exit (1);
	}
	ofile = stdout;
    }

    InitTable ();

    ReadInput (ifile);
    if (ifile != stdin)
	fclose (ifile);

    Predefine ();

    BuildTrie ();

    Output (ofile);
    exit (0);
}

void Warning (str)
    char *str;
{
    fprintf (stderr, "Warning: %s (at line %d)\n", str, lineno);
}

Error (str)
    char *str;
{
    fprintf (stderr, "%s (at line %d)\n", str, lineno);
    exit (1);
}

/*
 * ReadInput -- read and parse input .tit format file
 */
ReadInput (ifile)
    FILE *ifile;
{
#define	TIT_DEF_SECTION		0
#define	TIT_DICT_SECTION	1
#define	TIT_PHRASE_SECTION	2

    int titsection = TIT_DEF_SECTION;

    while (fgets (buffer, 255, ifile) != NULL) {

	char key[256];
	register char *kptr;
	register char *ptr, *sptr;

	lineno++;

	switch (titsection) {

	case TIT_DEF_SECTION :

	    if ((sscanf (buffer, "%s", key) != 1) || (key[0] == '#'))
		continue;	/* empty line or comment line */

	    if (strcmp (key, "BEGINDICTIONARY") == 0) {

		/* BEGINDICTIONARY */
		titsection = TIT_DICT_SECTION;

	    } else if (strcmp (key, "BEGINPHRASE") == 0) {

		/* BEGINPHRASE */
		titsection = TIT_PHRASE_SECTION;

	    } else if (strcmp (key, "VALIDINPUTKEY:") == 0) {

		/* VALIDINPUTKEY:  <keys> */
		getprompt (buffer, strbuf);
		ParseKey (strbuf);
		for (kptr = strbuf; *kptr; )
		    hzInputTable.keytype[*kptr++] |= HZ_KEY_INPUT_MASK;

	    } else if (strcmp (key, "WILDCARDKEY:") == 0) {

		/* WILDCARDKEY:  <keys> */
		getprompt (buffer, strbuf);
		ParseKey (strbuf);
		for (kptr = strbuf; *kptr; )
		    hzInputTable.keytype[*kptr++] = HZ_KEY_WILDCARD;
		wd = 1;

	    } else if (strcmp (key, "WILDCHARKEY:") == 0) {

		/* WILDCHARKEY:  <keys> */
		getprompt (buffer, strbuf);
		ParseKey (strbuf);
		for (kptr = strbuf; *kptr; )
		    hzInputTable.keytype[*kptr++] = HZ_KEY_WILDCHAR;
		wr = 1;

	    } else if (strcmp (key, "BACKSPACE:") == 0) {

		/* BACKSPACE:  <keys> */
		getprompt (buffer, strbuf);
		ParseKey (strbuf);
		for (kptr = strbuf; *kptr; )
		    hzInputTable.keytype[*kptr++] = HZ_KEY_BACKSPACE;
		bs = 1;

	    } else if (strcmp (key, "DELETEALL:") == 0) {

		/* DELETEALL:  <keys> */
		getprompt (buffer, strbuf);
		ParseKey (strbuf);
		for (kptr = strbuf; *kptr; )
		    hzInputTable.keytype[*kptr++] = HZ_KEY_KILL;
		kl = 1;

	    } else if (strcmp (key, "MOVERIGHT:") == 0) {

		/* MOVERIGHT:  <keys> */
		getprompt (buffer, strbuf);
		ParseKey (strbuf);
		for (kptr = strbuf; *kptr; )
		    hzInputTable.keytype[*kptr++] = HZ_KEY_RIGHT;
		mr = 1;

	    } else if (strcmp (key, "MOVELEFT:") == 0) {

		/* MOVELEFT:  <keys> */
		getprompt (buffer, strbuf);
		ParseKey (strbuf);
		for (kptr = strbuf; *kptr; )
		    hzInputTable.keytype[*kptr++] = HZ_KEY_LEFT;
		ml = 1;

	    } else if (strcmp (key, "REPEATKEY:") == 0) {

		/* REPEATKEY:  <keys> */
		getprompt (buffer, strbuf);
		ParseKey (strbuf);
		for (kptr = strbuf; *kptr; )
		    hzInputTable.keytype[*kptr++] = HZ_KEY_REPEAT;
		rp = 1;

	    } else if (strcmp (key, "SELECTKEY:") == 0) {

		/* SELECTKEY:  <keys> <keys> ... */
		register char *ptr = buffer;

		sl = 1;
		while (isspace (*ptr++))  /* skip spaces */;
		while ((*ptr) && (! isspace (*ptr++)))  ;
		/* skip "SELECTKEY:" */
		while (sscanf (ptr, "%s", strbuf) == 1) {
		    if (strbuf[0] == '#')
			break;		/* ignore comment */

		    /* one selection */
		    if (selection >= MAX_CHOICE) {
			char errstr[80];

			sprintf (errstr,
				 "Too many selection choices (maximum %d)",
				 MAX_CHOICE);
			Error (errstr);
		    }

		    ParseKey (strbuf);
		    for (kptr = strbuf; *kptr; ) {
			hzInputTable.keytype[*kptr++] |= 
				( (selection & HZ_KEY_SELECTION_NUM) |
				  HZ_KEY_SELECTION_MASK );
		    }

		    hzInputTable.choicelb[selection] = strbuf[0];
		    selection ++;

		    /* move ptr to the next token */
		    while (isspace (*ptr++))  ;	/* skip spaces */
		    while ((*ptr) && (! isspace (*ptr++)))  ;
		}

	    } else if (strncmp (key, "KEYPROMPT", strlen("KEYPROMPT")) == 0) {
			/* Beware of these two identical strings! */

		/* KEYPROMPT(<key>): <str> */
		unsigned char ch;

		if (sscanf (key, "KEYPROMPT(%[^)]):", keysbuf) != 1) {
		    Error ("Bad input format");
		}
		ParseKey (keysbuf);
		ch = keysbuf[0];

		if (getprompt (buffer, strbuf) != 1) {
		    Error ("Bad prompt format");
		}
		ParseKey (strbuf);
		if (strlen (strbuf) > MAX_KEYPROMPT) {
		    char errstr[80];

		    sprintf (errstr, "KEYPROMPT too long (maximum %d bytes)",
			     MAX_KEYPROMPT);
		    Error (errstr);
		}
		hzInputTable.keyprompt[ch].ptlen = strlen (strbuf);
		strncpy ((char *)(hzInputTable.keyprompt[ch].prompt), strbuf,
			 (int) hzInputTable.keyprompt[ch].ptlen);

	    } else if (strcmp (key, "PROMPT:") == 0) {

		/* PROMPT:  <str> */
		getprompt (buffer, strbuf);
		ParseKey (strbuf);
		if (strlen (strbuf) > MAX_PROMPT) {
		    char errstr[80];

		    sprintf (errstr, "PROMPT too long (maximum %d bytes)",
			     MAX_PROMPT);
		    Error (errstr);
		}
		hzInputTable.lenPrompt = strlen (strbuf);
		strncpy ((char *)(hzInputTable.prompt), strbuf,
			 hzInputTable.lenPrompt);

	    } else if (strcmp (key, "ENCODE:") == 0) {

		/* ENCODE: [GB|BIG5|JIS|KS] */
		if (getprompt (buffer, strbuf) != 1) {
		    Error ("Bad prompt format");
		}
		hzInputTable.encode = HZencode(strbuf);
		if (hzInputTable.encode < 0)
		    Error ("Unknown ENCODE");

	    } else if (strcmp (key, "AUTOSELECT:") == 0) {

		/* AUTOSELECT: [YES|NO] */
		if (getprompt (buffer, strbuf) != 1) {
		    Error ("Bad prompt format");
		}
		if ((strcmp (strbuf, "YES") == 0) ||
		    (strcmp (strbuf, "yes") == 0) ||
		    (strcmp (strbuf, "TRUE") == 0) ||
		    (strcmp (strbuf, "true") == 0))
		{
		    hzInputTable.autoSelect = 1;
		} else if ((strcmp (strbuf, "NO") == 0) ||
			   (strcmp (strbuf, "no") == 0) ||
			   (strcmp (strbuf, "FALSE") == 0) ||
			   (strcmp (strbuf, "false") == 0))
		{
		    hzInputTable.autoSelect = 0;
		} else
		    Error ("Unknown AUTOSELECT");

	    } else if (strcmp (key, "MULTICHOICE:") == 0) {

		/* For compatibility only, will go away in the future! */
		/* MULTICHOICE: [YES|NO] */
		Warning ("Old construct, replaced by AUTOSELECT");

		if (getprompt (buffer, strbuf) != 1) {
		    Error ("Bad prompt format");
		}
		if ((strcmp (strbuf, "YES") == 0) ||
		    (strcmp (strbuf, "yes") == 0) ||
		    (strcmp (strbuf, "TRUE") == 0) ||
		    (strcmp (strbuf, "true") == 0))
		{
		    hzInputTable.autoSelect = 0;
		} else if ((strcmp (strbuf, "NO") == 0) ||
			   (strcmp (strbuf, "no") == 0) ||
			   (strcmp (strbuf, "FALSE") == 0) ||
			   (strcmp (strbuf, "false") == 0))
		{
		    hzInputTable.autoSelect = 1;
		} else
		    Error ("Unknown MULTICHOICE");

	    } else if (strcmp (key, "COMMENT") == 0) {

		/* COMMENT  <any> */
		register char *ptr = buffer;

		while (isspace (*ptr))  ptr++;	/* skip leading spaces */
		while ((*ptr) && (! isspace (*ptr)))
		    ptr++;			/* skip word COMMENT */

		/* unlike '#', COMMENT body must be saved */
		while ((*ptr) && (clen < MAXCLEN))
		    comment[clen++] = *ptr++ ;

	    } else	/* unknown key */
		Error ("Unknown input");

	    break;

	case TIT_DICT_SECTION :

	    if ((sscanf (buffer, "%s", key) != 1) || (key[0] == '#'))
		continue;	/* empty line or comment line */

	    if (strcmp (key, "BEGINPHRASE") == 0) {

		/* BEGINPHRASE */
		titsection = TIT_PHRASE_SECTION;
		continue;

	    }

	    /* now, the DICTIONARY part */
	    /* <input keys>	<HANZIs> */

	    ptr = buffer;

	    while (isspace (*ptr))  ptr++;	/* skip leading spaces */
	    if (*ptr == '#' || *ptr == '\0')
		continue;	/* comment or empty line */

	    /* here is the input field */
	    kptr = ptr;
	    while ((*ptr) && (! isspace (*ptr)))
		ptr++;
	    if (*ptr == '\0')
	        Error ("Wrong input format");
	    *ptr++ = '\0';
	    ParseKey (kptr);
	    for (sptr = kptr; *sptr; sptr++) {
		if (*sptr & 0x80)
		    Error ("Unacceptable key for Chinese input");
		if (! (hzInputTable.keytype[ *sptr ] & HZ_KEY_INPUT_MASK))
		    Error ("Input key is not specified to be valid");
	    }

	    while (isspace(*ptr) && (!(*ptr & 0x80)))
		ptr++;		/* skip leading spaces */

	    /* here is the HANZI field */

	    if ( (!(*ptr & 0x80)) && (*ptr != '(') ) {
		/* empty hanzi */
		InsertTrie (kptr, "");
		continue;
	    }

	    while (1) {

		if (*ptr & 0x80) {

		    if (*(ptr+1) == '\0')
			Error ("Unacceptable HZ code");
		    /* 2nd byte not check against 0x80 */
		    strbuf[0] = *ptr;
		    strbuf[1] = *(ptr+1);
		    strbuf[2] = '\0';
		    InsertTrie (kptr, strbuf);	/* a HZ char */
		    ptr += 2;

		} else if (*ptr == '(') {	/* begin of phrase */

		    register char *hzptr = ++ptr;

		    while (*ptr != ')') {
			if (*ptr & 0x80) {
			    if (*(++ptr) == '\0')
				Error ("Unacceptable HZ code");
			    ptr++;
			} else
			    Error ("Unacceptable HZ code, or unbalancing '(' and ')'");
		    }
		    *ptr++ = '\0';
		    strbuf[0] = HZ_PHRASE_TAG;
		    strbuf[1] = (char) (strlen (hzptr) / 2);
		    strcpy (strbuf+2, hzptr);
		    InsertTrie (kptr, strbuf);
		    continue;

		} else
		    break;

	    } /* while (1) */

	    break;

	case TIT_PHRASE_SECTION :

	    if ((sscanf (buffer, "%s", key) != 1) || (key[0] == '#'))
		continue;	/* empty line or comment line */

	    if (strcmp (key, "BEGINDICTIONARY") == 0) {

		/* BEGINDICTIONARY */
		titsection = TIT_DICT_SECTION;

	    }

	    /* now, the PHRASE dictionary part */
	    /* <input keys>	<HANZIs>, <HANZIs>, ... */

	    ptr = buffer;

	    while (isspace (*ptr))  ptr++;	/* skip leading spaces */
	    if (*ptr == '#' || *ptr == '\0')
		continue;	/* comment or empty line */

	    /* here is the input field */
	    kptr = ptr;
	    while ((*ptr) && (! isspace (*ptr)))
		ptr++;
	    if (*ptr == '\0')
	        Error ("Wrong input format");
	    *ptr++ = '\0';
	    ParseKey (kptr);
	    for (sptr = kptr; *sptr; sptr++) {
		if (*sptr & 0x80)
		    Error ("Unacceptable key for Chinese input");
		if (! (hzInputTable.keytype[ *sptr ] & HZ_KEY_INPUT_MASK))
		    Error ("Input key is not specified to be valid");
	    }

	    while (isspace(*ptr) && (!(*ptr & 0x80)))
		ptr++;		/* skip leading spaces */

	    /* here is the HANZI PHRASE field */

	    if ( !(*ptr & 0x80) ) {
		/* empty hanzi */
		InsertTrie (kptr, "");
		continue;
	    }

	    while (*ptr & 0x80) {
		char eos;
		register char *hzptr = ptr;
		int numhz;

		while (*ptr & 0x80) {
		    if (*(++ptr) == '\0')
			Error ("Unacceptable HZ code");
		    ptr++;
		}
		eos = *ptr;	/* save the end-of-string mark */

		numhz = (ptr - hzptr) / 2;
		*ptr++ = '\0';

		if (numhz == 1)			/* a single hz */
		    InsertTrie (kptr, hzptr);
		else {
		    strbuf[0] = HZ_PHRASE_TAG;
		    strbuf[1] = (char) (strlen (hzptr) / 2);
		    strcpy (strbuf+2, hzptr);
		    InsertTrie (kptr, strbuf);
		}

		if (eos == '\n' || eos == '\0')
		   break;	/* done with this line */
		else if (isspace(eos) || (eos == ','))	/* deliminator */
		    while ( (isspace(*ptr) && (!(*ptr & 0x80)))
			    || (*ptr == ',') )
			ptr++;	/* skip leading spaces/deliminators */
		else
		    Error ("Unrecognized deliminator in HZ phrases");

	    } /* while (1) */

	    break;

	} /* switch (titsection) */
    }
}

/*
 * BuildTrie -- reorganize the dynTrie into static TRIE structure
 */
BuildTrie ()
{
    trieList = (trieNode *) calloc (totalTrieNode, sizeof(trieNode));
    hzList = (XChar2b *) malloc (totalHZbytes);
    if ((! hzList) || (! trieList)) {
	perror ("BuildTrie");
	exit (1);
    }

    pTrieList = 1;	/* 0 is the root, start from index 1 */
    pHZList = 0;

    (void) Linearize (0, root);
    trieList[0].tn_hzidx = 0;
    trieList[0].tn_numHZchoice = 0;	/* no choice for the upmost node */

    /* ASSERT: pTrieList = totalTrieNode, pHZList = totalHZbytes/2 */
    hzInputTable.sizeTrieList = pTrieList;
    hzInputTable.sizeHZList = pHZList;
}

/*
 * Linearize -- make the trie tree into a linear array, for I/O.
 *		returns the number of choices under the node.
 */
int Linearize (idxTrie, trie)
    unsigned int idxTrie;
    register dynTrie *trie;
{
    register dynTrie *tptr = trie->son;
    register unsigned int i;
    dynTrie *tptr2;
    unsigned int bTrieList = pTrieList;
    unsigned int bHZList = pHZList;
    int numChoice = 0;

    if (tptr->key == '\0') {
	char *hzptr = tptr->hzptr;

	while (*hzptr) {
	    if (*hzptr != HZ_PHRASE_TAG) {
		/* a hanzi */
		hzList[pHZList  ].byte1 = *hzptr++;
		hzList[pHZList++].byte2 = *hzptr++;
	    } else {
		/* a phrase */
		register int i, hzlen = *(hzptr+1);

		hzList[pHZList  ].byte1 = *hzptr++;
		hzList[pHZList++].byte2 = *hzptr++;
		for (i = 0; i < hzlen; i++) {
		    hzList[pHZList  ].byte1 = *hzptr++;
		    hzList[pHZList++].byte2 = *hzptr++;
		}
	    }
	    numChoice ++ ;
	}
	tptr = tptr->next;
    }

    tptr2 = tptr;	/* save for second pass */
    while (tptr) {
	trieList[ pTrieList++ ].tn_key = tptr->key;
	tptr = tptr->next;
    }
    trieList[ idxTrie ].tn_nextKeys = bTrieList;
    trieList[ idxTrie ].tn_numNextKeys = pTrieList - bTrieList;

    trieList[ idxTrie ].tn_hzidx = bHZList;
    for (tptr = tptr2, i = bTrieList; tptr; i++, tptr = tptr->next) {
	numChoice += Linearize (i, tptr); 
    }
    trieList[ idxTrie ].tn_numHZchoice = numChoice;

    return (numChoice);
}

/* 
 * InsertTrie -- insert the keys-HZs pair into dynTrie structure
 */
InsertTrie (kptr, hzptr)
    register char *kptr;
    char *hzptr;
{
    register dynTrie *tptr = root;
    dynTrie *NewTrieNode ();

    while (*kptr) {
	unsigned char key = *kptr++;

	if (tptr->son == NULL) {
	    tptr->son = NewTrieNode (key, (dynTrie *)NULL, (dynTrie *)NULL);
	    tptr = tptr->son;
	} else if (tptr->son->key > key) {
	    /* new key should be the 1st son, the old one becomes the next */
	    tptr->son = NewTrieNode (key, (dynTrie *)NULL, tptr->son);
	    tptr = tptr->son;
	} else if (tptr->son->key == key) {
	    tptr = tptr->son;
	} else {
	    /* ASSERT: (tptr->son->key < key) */
	    tptr = tptr->son;
	    while ((tptr->next != NULL) && (tptr->next->key < key)) {
	        tptr = tptr->next;	/* try next */
	    }
	    /* ASSERT: (tptr->next == NULL) || (tptr->next->key >= key) */
	    if ((tptr->next == NULL) || (tptr->next->key > key)) {
		/* add it here (to keep it sorted) */
		tptr->next = NewTrieNode (key, (dynTrie *)NULL, tptr->next);
		tptr = tptr->next;
	    } else {	/* tptr->next->key == key */
		tptr = tptr->next;
	    }
	}
    }

    /* come to the end of the key string kptr */

    if (tptr->son == NULL) {
	tptr->son = NewTrieNode ('\0', (dynTrie *)NULL, (dynTrie *)NULL);
	tptr->son->hzptr = malloc (strlen (hzptr) + 1);
	if (tptr->son->hzptr == NULL)
	    Error ("Run out of memory");
	strcpy (tptr->son->hzptr, hzptr);
    } else if (tptr->son->key != '\0') {
	/* new key should be the 1st son, the old one becomes the next */
	tptr->son = NewTrieNode ('\0', (dynTrie *)NULL, tptr->son);
	tptr->son->hzptr = malloc (strlen (hzptr) + 1);
	if (tptr->son->hzptr == NULL)
	    Error ("Run out of memory");
	strcpy (tptr->son->hzptr, hzptr);
    } else {
	tptr->son->hzptr = realloc (tptr->son->hzptr,
		strlen (tptr->son->hzptr) + strlen (hzptr) + 1);
	if (tptr->son->hzptr == NULL)
	    Error ("Run out of memory");
	strcat (tptr->son->hzptr, hzptr);
    }
    totalHZbytes += strlen (hzptr);
}

dynTrie *NewTrieNode (key, son, next)
    unsigned char key;
    struct _dynTrie *son;
    struct _dynTrie *next;
{
    register dynTrie *t = (dynTrie *) malloc (sizeof(dynTrie));

    if (t == NULL)
	Error ("Run out of memory");
    t->key = key;
    t->son = son;
    t->next = next;
    if (key)
	totalTrieNode++ ;
    return (t);
}

Output (ofile)
    FILE *ofile;
{
    (void) fwrite (MAGIC_CIT, 2, 1, ofile);	/* write the magic number */
    if ((fwrite (&hzInputTable, sizeof(HZinputTable), 1, ofile) == 0) ||
	(fwrite (trieList, sizeof(trieNode), hzInputTable.sizeTrieList, ofile)
	    != hzInputTable.sizeTrieList) ||
	(fwrite (hzList, sizeof(XChar2b), hzInputTable.sizeHZList, ofile)
	    != hzInputTable.sizeHZList))
    {
	perror ("Writing output file");
	exit (1);
    }

    if ((fwrite ((char *)(&clen), sizeof(unsigned int), 1, ofile) == 0) ||
	(fwrite (comment, 1, clen, ofile) != clen))
    {
	perror ("Writing output file");
	exit (1);
    }
}

ParseKey (buf)
    char buf[];
{
    register char *p1 = buf, *p2 = buf;

    while (*p1) {
	if (*p1 == '\\') {
	    p1++;
	    if ((p1[0] == '0') || (p1[0] == '1')) {
		if ((p1[1] >= '0') && (p1[1] <= '7') &&
		    (p1[2] >= '0') && (p1[2] <= '7')) {
			*p2 = (*p1++ - '0') << 6;
			*p2 += (*p1++ - '0') << 3;
			*(p2++) += (*p1++ - '0');
			continue;
		}
	    } else if (isdigit (*p1))
		Error ("Error in parsing input keys");
	}
	*p2++ = *p1++;
    }
    *p2 = '\0';
}

#define	DEF_PROMPT	"\272\272\327\326\312\344\310\353\241\313\241\241"
			/* han zi shu ru (Chinese Input) :: SPACE */

InitTable ()
{
    register int i;

    hzInputTable.version = CIT_VERSION;
    hzInputTable.encode = GB_ENCODE;	/* dafault: GB coding */
    hzInputTable.builtin = 0;
    hzInputTable.autoSelect = 0;
    for (i = 0; i < 128; i++) {
	hzInputTable.keytype[i] = HZ_KEY_INVALID;
	hzInputTable.keyprompt[i].prompt[0] = i;
	hzInputTable.keyprompt[i].prompt[1] = '\0';
	hzInputTable.keyprompt[i].ptlen = 1;
    }
    strcpy ((char *)hzInputTable.prompt, DEF_PROMPT);
    hzInputTable.lenPrompt = strlen(DEF_PROMPT);
}

/* predefined value */
Predefine ()
{
#define	SetIfInvalid(k,f)					\
	if (hzInputTable.keytype[k] == HZ_KEY_INVALID)		\
	    hzInputTable.keytype[k] = (f)

    if (! bs) {
	SetIfInvalid ( '\010', HZ_KEY_BACKSPACE );		/* \b */
	SetIfInvalid ( '\177', HZ_KEY_BACKSPACE );		/* DEL */
    }
    if (! kl) {
	SetIfInvalid ( '\015', HZ_KEY_KILL );			/* \r */
	SetIfInvalid ( '\025', HZ_KEY_KILL );			/* ^U */
    }
    if (! mr) {
	SetIfInvalid ( '.', HZ_KEY_RIGHT );
	SetIfInvalid ( '>', HZ_KEY_RIGHT );
    }
    if (! ml) {
	SetIfInvalid ( ',', HZ_KEY_LEFT );
	SetIfInvalid ( '<', HZ_KEY_LEFT );
    }
    if (! rp) {
	SetIfInvalid ( '\020', HZ_KEY_REPEAT );			/* ^P */
	SetIfInvalid ( '\022', HZ_KEY_REPEAT );			/* ^R */
    }
    if (! wd) {
	SetIfInvalid ( '*', HZ_KEY_WILDCARD );
    }
    if (! wr) {
	SetIfInvalid ( '?', HZ_KEY_WILDCHAR );
    }
    if (! sl) {
	if ((hzInputTable.keytype['0'] == HZ_KEY_INVALID) &&
	    (hzInputTable.keytype['1'] == HZ_KEY_INVALID) &&
	    (hzInputTable.keytype['2'] == HZ_KEY_INVALID) &&
	    (hzInputTable.keytype['3'] == HZ_KEY_INVALID) &&
	    (hzInputTable.keytype['4'] == HZ_KEY_INVALID) &&
	    (hzInputTable.keytype['5'] == HZ_KEY_INVALID) &&
	    (hzInputTable.keytype['6'] == HZ_KEY_INVALID) &&
	    (hzInputTable.keytype['7'] == HZ_KEY_INVALID) &&
	    (hzInputTable.keytype['8'] == HZ_KEY_INVALID) &&
	    (hzInputTable.keytype['9'] == HZ_KEY_INVALID))
	{
	    hzInputTable.keytype['0'] = 0 | HZ_KEY_SELECTION_MASK;
	    if (hzInputTable.keytype[' '] == HZ_KEY_INVALID)
		hzInputTable.keytype[' '] = 0 | HZ_KEY_SELECTION_MASK;
	    hzInputTable.keytype['1'] = 1 | HZ_KEY_SELECTION_MASK;
	    hzInputTable.keytype['2'] = 2 | HZ_KEY_SELECTION_MASK;
	    hzInputTable.keytype['3'] = 3 | HZ_KEY_SELECTION_MASK;
	    hzInputTable.keytype['4'] = 4 | HZ_KEY_SELECTION_MASK;
	    hzInputTable.keytype['5'] = 5 | HZ_KEY_SELECTION_MASK;
	    hzInputTable.keytype['6'] = 6 | HZ_KEY_SELECTION_MASK;
	    hzInputTable.keytype['7'] = 7 | HZ_KEY_SELECTION_MASK;
	    hzInputTable.keytype['8'] = 8 | HZ_KEY_SELECTION_MASK;
	    hzInputTable.keytype['9'] = 9 | HZ_KEY_SELECTION_MASK;
	    strcpy (hzInputTable.choicelb, "0123456789");
	    hzInputTable.maxchoice = 10;
	}
    } else
	hzInputTable.maxchoice = selection;
}

getprompt (buffer, strbuf)
     char *buffer, *strbuf;
{
#define	is7space(c)	(isspace(c) && (!((c) & 0x80)))

    while (isspace (*buffer))  buffer++;
    while (*buffer && (! isspace(*buffer)))  buffer++;	/* skip 1st field */
    while (is7space (*buffer))  buffer++;
    if (! *buffer) {
	*strbuf = '\0';
	return (0);
    }
    while (*buffer && (! is7space(*buffer)))
	*strbuf++ = *buffer++;
    *strbuf = '\0';
    return(1);
}
