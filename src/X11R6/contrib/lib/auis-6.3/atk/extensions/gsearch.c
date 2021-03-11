/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
 \* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/gsearch.c,v 1.5 1994/04/20 16:01:45 rr2b Exp $";
#endif


#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>

#include <class.h>

#include <bind.ih>
#include <text.ih>
#include <textv.ih>
#include <mark.ih>
#include <search.ih>
#include <message.ih>
#include <im.ih>
#include <owatch.ih>

#include <gsearch.eh>

#define MAX(a,b) (((a)>(b))?(a):(b))

#define DYNSTR_GROWSIZE (64)
#define STATESTACK_GROWSIZE (32)

#define CTRL_G (7)
#define CTRL_H (8)
#define CTRL_L (12)
#define CTRL_Q (17)
#define CTRL_R (18)
#define CTRL_S (19)
#define CTRL_W (23)
#define CTRL_Y (25)
#define ESC (27)
#define DEL (127)

struct dynstr {
    int used, allocated;
    char *text;
};

struct statestacknode {
    int patternlen, wrappedp, failurep;
    long position, length, searchfrom;
    int forwardp;
};

struct statestack {
    int used, allocated;
    struct statestacknode *nodes;
};

#ifndef _IBMR2
extern char *malloc(), *realloc();
#endif

static jmp_buf jmpenv;
static struct dynstr LastPattern;
static struct statestacknode *StackTop;

static int statestack_init(s)
struct statestack *s;
{
    s->used = 0;
    s->allocated = STATESTACK_GROWSIZE;
    if (!(s->nodes =
	  (struct statestacknode *) malloc(STATESTACK_GROWSIZE *
					    (sizeof (struct statestacknode)))))
	return (1);
    return (0);
}

static void statestack_destroy(s)
struct statestack *s;
{
    free(s->nodes);
}

static void statestack_pop(s)
struct statestack *s;
{
    --(s->used);
    StackTop = s->nodes + s->used - 1;
}

static void statestack_push(s, pl, wp, fp, pos, len, sf, fwdp)
struct statestack *s;
int pl, wp, fp;
long pos, len, sf;
int fwdp;
{
    if (s->used == s->allocated) {
	int newsize = s->allocated + STATESTACK_GROWSIZE;

	if (!(s->nodes =
	     (struct statestacknode *)
	      realloc(s->nodes,
		      newsize * (sizeof (struct statestacknode)))))
	    longjmp(jmpenv, 1);
	s->allocated = newsize;
    }
    s->nodes[s->used].patternlen = pl;
    s->nodes[s->used].wrappedp = wp;
    s->nodes[s->used].failurep = fp;
    s->nodes[s->used].position = pos;
    s->nodes[s->used].length = len;
    s->nodes[s->used].searchfrom = sf;
    s->nodes[(s->used)++].forwardp = fwdp;
    StackTop = s->nodes + s->used - 1;
}

static int dynstr_init(d)
struct dynstr *d;
{
    d->used = 1;
    d->allocated = DYNSTR_GROWSIZE;
    if (!(d->text = malloc(DYNSTR_GROWSIZE)))
	return (1);
    d->text[0] = '\0';
    return (0);
}

static void dynstr_shortento(d, size)
struct dynstr *d;
int size;
{
    d->used = size;
    d->text[d->used - 1] = '\0';
}

static void dynstr_ensuresize(d, size)
struct dynstr *d;
int size;
{
    int newsize;

    if (d->allocated >= size)
	return;
    if (!(d->text = realloc(d->text,
			    newsize = MAX(DYNSTR_GROWSIZE,
					  size + (DYNSTR_GROWSIZE >> 1)))))
	longjmp(jmpenv, 1);
    d->allocated = newsize;
}

static void dynstr_put(d, str)
struct dynstr *d;
char *str;
{
    int need = 1 + strlen(str);

    dynstr_ensuresize(d, need);
    strcpy(d->text, str);
    d->used = need;
}

static void dynstr_append(d, str)
struct dynstr *d;
char *str;
{
    int need = d->used + strlen(str);

    dynstr_ensuresize(d, need);
    strcat(d->text, str);
    d->used = need;
}

static void dynstr_destroy(d)
struct dynstr *d;
{
    free(d->text);
}

static int dynstr_empty(d)
struct dynstr *d;
{
    return ((d->used == 0) || (d->text[0] == '\0'));
}

static void dynstr_addchar(d, c)
struct dynstr *d;
int c;
{
    int need = d->used + 1;

    dynstr_ensuresize(d, need);
    d->text[d->used - 1] = c;
    d->text[(d->used)++] = '\0';
}

static void dynstr_copy(dest, src)
struct dynstr *dest, *src;
{
    dynstr_ensuresize(dest, src->used);
    strcpy(dest->text, src->text);
    dest->used = src->used;
}

static void dosearch(tv, forwardp)
struct textview *tv;
int forwardp;
{
    FILE *tmp_file;
    struct text *txt = (struct text *) textview_GetDataObject(tv);
    struct owatch_data *watchtv = owatch_Create(tv);
    long origpos = textview_GetDotPosition(tv);
    long origlen = textview_GetDotLength(tv);
    long foundloc, newsearchfrom;
    struct dynstr pattern, prompt;
    int wasmeta = 0;
    int c, dodokey = 0, oldforwardp;
    char *compiled, *compileerr, *tmpbuf;
    struct statestack stack;

    if (dynstr_init(&pattern)) {
	message_DisplayString(tv, 0, "I-Search is out of memory; aborted.");
	im_ForceUpdate();
	return;
    }
    if (dynstr_init(&prompt)) {
	dynstr_destroy(&pattern);
	message_DisplayString(tv, 0, "I-Search is out of memory; aborted.");
	im_ForceUpdate();
	return;
    }
    if (statestack_init(&stack)) {
	dynstr_destroy(&pattern);
	dynstr_destroy(&prompt);
	message_DisplayString(tv, 0, "I-Search is out of memory; aborted.");
	im_ForceUpdate();
	return;
    }
    
    if (setjmp(jmpenv)) {
	if (owatch_Check(watchtv)) {
		textview_SetDotPosition(tv, origpos);
		textview_SetDotLength(tv, origlen);
		textview_FrameDot(tv, origpos);
	}
	message_DisplayString(im_GetLastUsed(), 0, 
			"I-Search is out of memory; aborted.");
	goto cleanup_and_return;
    }

    statestack_push(&stack, 1, 0, 0, origpos, origlen,
		    textview_GetDotPosition(tv), forwardp);

  initialstate:
    
    dynstr_put(&prompt, "I-Search");
    if (!forwardp)
	dynstr_append(&prompt, " backward");
    if (!dynstr_empty(&LastPattern)) {
	dynstr_append(&prompt, " [");
	dynstr_append(&prompt, LastPattern.text);
	dynstr_append(&prompt, "]");
    }
    dynstr_append(&prompt, ": ");
    message_DisplayString(tv, 0, prompt.text);
    im_ForceUpdate();
    while (1) {
	c = im_GetCharacter(textview_GetIM(tv));
	if ( ! owatch_Check(watchtv)) goto exitstate;
	switch (c) {
	  case CTRL_L:
	    im_RedrawWindow(textview_GetIM(tv));
	    break;
	  case CTRL_G:
	    message_DisplayString(tv, 0, "Cancelled.");
	    goto cleanup_and_return;
	  case CTRL_H:
	  case DEL:
	    break;
	  case CTRL_Q:
	    while ((c = im_GetCharacter(textview_GetIM(tv))) == EOF)
		;
	    if ( ! owatch_Check(watchtv)) goto exitstate;
	    dynstr_addchar(&pattern, c);
	    goto compilestate;
	  case CTRL_R:
	  case CTRL_S:
	    forwardp = (c == CTRL_S);
	    if (!dynstr_empty(&LastPattern)) {
		dynstr_copy(&pattern, &LastPattern);
		goto compilestate;
	    }
	    goto initialstate;
	  case CTRL_W:
	    if (textview_GetDotLength(tv) > 0)
		goto appendselectionstate;
	    break;
	  case CTRL_Y:
	    goto appendkillheadstate;
	  case ESC:
	    wasmeta = im_WasMeta(textview_GetIM(tv));
	    /* Fall through */
	  case EOF:
	    goto exitstate;
	  default:
	    if (isascii(c) && (isprint(c) || isspace(c))) {
		dynstr_addchar(&pattern, c);
		goto compilestate;
	    }
	    else {
		dodokey = 1;
		goto exitstate;
	    }
	}
    }

  emptypatternstate:
    
    dynstr_put(&prompt, "I-Search");
    if (!forwardp)
	dynstr_append(&prompt, " backward");
    dynstr_append(&prompt, ": ");
    message_DisplayString(tv, 0, prompt.text);
    im_ForceUpdate();
    while (1) {
	c = im_GetCharacter(textview_GetIM(tv));
	if ( ! owatch_Check(watchtv)) goto exitstate;
	switch (c) {
	  case CTRL_L:
	    im_RedrawWindow(textview_GetIM(tv));
	    break;
	  case CTRL_G:
	    message_DisplayString(tv, 0, "Cancelled.");
	    goto cleanup_and_return;
	  case CTRL_H:
	  case DEL:
	    break;
	  case CTRL_Q:
	    while ((c = im_GetCharacter(textview_GetIM(tv))) == EOF)
		;
	    if ( ! owatch_Check(watchtv)) goto exitstate;
	    dynstr_addchar(&pattern, c);
	    goto compilestate;
	  case CTRL_R:
	  case CTRL_S:
	    forwardp = (c == CTRL_S);
	    goto initialstate;
	  case CTRL_W:
	    if (textview_GetDotLength(tv) > 0)
		goto appendselectionstate;
	    break;
	  case CTRL_Y:
	    goto appendkillheadstate;
	  case ESC:
	    wasmeta = im_WasMeta(textview_GetIM(tv));
	    /* Fall through */
	  case EOF:
	    goto exitstate;
	  default:
	    if (isascii(c) && (isprint(c) || isspace(c))) {
		dynstr_addchar(&pattern, c);
		goto compilestate;
	    }
	    else {
		dodokey = 1;
		goto exitstate;
	    }
	}
    }

  compilestate:

    compiled = NULL;
    if ((compileerr = search_CompilePattern(pattern.text,
					    &compiled)) == NULL)
	goto searchstate;
    statestack_push(&stack, pattern.used, StackTop->wrappedp,
		    StackTop->failurep, StackTop->position, StackTop->length,
		    StackTop->searchfrom, forwardp);
    goto partialstate;

  searchstate:

    if (forwardp)
	foundloc = search_MatchPattern(txt, StackTop->searchfrom, compiled);
    else
	foundloc = search_MatchPatternReverse(txt, StackTop->searchfrom,
					      compiled);
    if (foundloc >= 0) {
	statestack_push(&stack, pattern.used, StackTop->wrappedp,
			0, foundloc, search_GetMatchLength(),
			StackTop->searchfrom, forwardp);
	textview_SetDotPosition(tv, foundloc);
	textview_SetDotLength(tv, search_GetMatchLength());
	textview_FrameDot(tv, foundloc);
	goto successstate;
    }
    statestack_push(&stack, pattern.used, StackTop->wrappedp,
		    1, StackTop->position, StackTop->length,
		    StackTop->searchfrom, forwardp);
    if (im_IsPlaying()) {
	im_CancelMacro();	/* This section should emulate exitstate */
	if (!dynstr_empty(&pattern))
	    dynstr_copy(&LastPattern, &pattern);
	mark_SetPos(tv->atMarker, origpos);
	mark_SetLength(tv->atMarker, origlen);
	message_DisplayString(tv, 0,
			      "Search failed, macro aborted, mark set.");
	goto cleanup_and_return;
    }
    goto failurestate;

  newsearchstate:

    if (forwardp)
	foundloc = search_MatchPattern(txt, newsearchfrom, compiled);
    else
	foundloc = search_MatchPatternReverse(txt, newsearchfrom,
					      compiled);
    if (foundloc >= 0) {
	statestack_push(&stack, pattern.used, StackTop->wrappedp,
			0, foundloc, search_GetMatchLength(),
			newsearchfrom, forwardp);
	textview_SetDotPosition(tv, foundloc);
	textview_SetDotLength(tv, search_GetMatchLength());
	textview_FrameDot(tv, foundloc);
	goto successstate;
    }
    statestack_push(&stack, pattern.used, StackTop->wrappedp,
		    1, StackTop->position, StackTop->length,
		    StackTop->searchfrom, forwardp);
    if (im_IsPlaying()) {
	im_CancelMacro();	/* This section should emulate exitstate */
	if (!dynstr_empty(&pattern))
	    dynstr_copy(&LastPattern, &pattern);
		mark_SetPos(tv->atMarker, origpos);
	mark_SetLength(tv->atMarker, origlen);
	message_DisplayString(tv, 0,
			      "Search failed, macro aborted, mark set.");
	goto cleanup_and_return;
    }
    goto failurestate;

  wrapsearchstate:

    if (forwardp)
	foundloc = search_MatchPattern(txt, (long) 0, compiled);
    else
	foundloc = search_MatchPatternReverse(txt, text_GetLength(txt),
					      compiled);
    if (foundloc >= 0) {
	statestack_push(&stack, pattern.used, 1,
			0, foundloc, search_GetMatchLength(),
			(long) 0, forwardp);
	textview_SetDotPosition(tv, foundloc);
	textview_SetDotLength(tv, search_GetMatchLength());
	textview_FrameDot(tv, foundloc);
	goto successstate;
    }
    statestack_push(&stack, pattern.used, 1,
		    1, StackTop->position, StackTop->length,
		    StackTop->searchfrom, forwardp);
    if (im_IsPlaying()) {
	im_CancelMacro();	/* This section should emulate exitstate */
	if (!dynstr_empty(&pattern))
	    dynstr_copy(&LastPattern, &pattern);
	mark_SetPos(tv->atMarker, origpos);
	mark_SetLength(tv->atMarker, origlen);
	message_DisplayString(tv, 0,
			      "Search failed, macro aborted, mark set.");
	goto cleanup_and_return;
    }
    goto failurestate;

  partialstate:

    dynstr_put(&prompt, "");
    if (StackTop->failurep)
	dynstr_append(&prompt, "Failing ");
    if (StackTop->wrappedp) {
	if (StackTop->failurep)
	    dynstr_addchar(&prompt, 'w');
	else
	    dynstr_addchar(&prompt, 'W');
	dynstr_append(&prompt, "rapped ");
    }
    dynstr_append(&prompt, "I-Search");
    if (!forwardp)
	dynstr_append(&prompt, " backward");
    dynstr_append(&prompt, ": ");
    dynstr_append(&prompt, pattern.text);
    dynstr_append(&prompt, "  [incomplete input");
    if (compileerr) {
	dynstr_append(&prompt, " - ");
	dynstr_append(&prompt, compileerr);
    }
    dynstr_append(&prompt, "]");
    message_DisplayString(tv, 0, prompt.text);
    im_ForceUpdate();
    while (1) {
	c = im_GetCharacter(textview_GetIM(tv));
	if ( ! owatch_Check(watchtv)) goto exitstate;
	switch (c) {
	  case CTRL_L:
	    im_RedrawWindow(textview_GetIM(tv));
	    break;
	  case CTRL_G:
	    message_DisplayString(tv, 0, "Cancelled.");
	    goto cleanup_and_return;
	  case CTRL_H:
	  case DEL:
	    goto popstate;
	  case CTRL_Q:
	    while ((c = im_GetCharacter(textview_GetIM(tv))) == EOF)
		;
	    if ( ! owatch_Check(watchtv)) goto exitstate;
	    dynstr_addchar(&pattern, c);
	    goto compilestate;
	  case CTRL_R:
	  case CTRL_S:
	    forwardp = (c == CTRL_S);
	    break;
	  case CTRL_W:
	    goto appendselectionstate;
	  case CTRL_Y:
	    goto appendkillheadstate;
	  case ESC:
	    wasmeta = im_WasMeta(textview_GetIM(tv));
	    /* Fall through */
	  case EOF:
	    goto exitstate;
	  default:
	    if (isascii(c) && (isprint(c) || isspace(c))) {
		dynstr_addchar(&pattern, c);
		goto compilestate;
	    }
	    else {
		dodokey = 1;
		goto exitstate;
	    }
	}
    }

  exitstate:

    if (!dynstr_empty(&pattern))
	dynstr_copy(&LastPattern, &pattern);
    if (owatch_Check(watchtv)) {
	struct im *im;
	mark_SetPos(tv->atMarker, origpos);
	mark_SetLength(tv->atMarker, origlen);
	message_DisplayString(tv, 0, "Mark set.");
	im = textview_GetIM(tv);
	im_ForceUpdate();
	if (dodokey)
	    im_DoKey(im, c);
	else {
	    if (wasmeta) {
		im_DoKey(im, ESC);
		im_DoKey(im, im_GetCharacter(im));
	    }
	}
    }
    else {
	message_DisplayString(NULL, 0, "I-Search cancelled");
	 if ( ! dodokey && wasmeta) {
		/* discard second half of meta key  ???  */
		im_GetCharacter(im_GetLastUsed());
 	}
    }
    goto cleanup_and_return;
    
  popstate:

    statestack_pop(&stack);
    goto poppedstate;

  popbeforeerrorstate:

    while (StackTop->failurep) {
	statestack_pop(&stack);
    }
    goto poppedstate;

  poppedstate:

    textview_SetDotPosition(tv, StackTop->position);
    textview_SetDotLength(tv, StackTop->length);
    textview_FrameDot(tv, StackTop->position);
    dynstr_shortento(&pattern, StackTop->patternlen);
    forwardp = StackTop->forwardp;
    if (dynstr_empty(&pattern))
	goto emptypatternstate;
    compiled = NULL;
    if (compileerr = search_CompilePattern(pattern.text, &compiled))
	goto partialstate;
    if (StackTop->failurep)
	goto failurestate;
    goto successstate;

  appendselectionstate:

    if (!(tmpbuf = malloc(1 + textview_GetDotLength(tv)))) {
	longjmp(jmpenv, 1);
    }
    text_CopySubString(txt, textview_GetDotPosition(tv),
		       textview_GetDotLength(tv), tmpbuf, FALSE);
    dynstr_append(&pattern, tmpbuf);
    free(tmpbuf);
    goto compilestate;

  appendkillheadstate:

#define CLINELENGTH (40)
    tmp_file = im_FromCutBuffer(textview_GetIM(tv));
    {
      char *res, inbuf[CLINELENGTH];
      boolean usual;

      res = fgets(inbuf, CLINELENGTH, tmp_file);
      usual = FALSE;
      if (res) {
	  if (strncmp(inbuf, "\\begindata{", 11))
	      usual = TRUE;
	  else {
	      long num;
	      long idnum;
	      char obuffer[296];
	      struct text *tx;
	      num = sscanf(inbuf, "\\begindata{%[^,],%ld}", obuffer, &idnum);
	      if (num != 2)
		  usual = TRUE;
	      else {
		  if (class_IsTypeByName(obuffer, "text")) {
		      tx = (struct text *)class_NewObject(obuffer);
		      num = text_Read(tx, tmp_file, idnum);
		      if (!num) {
			  idnum = text_GetLength(tx);
			  for (num=0; num < idnum; num++) {
			      c = text_GetChar(tx, num);
			      dynstr_addchar(&pattern, c);
			  }
		      }
		      else {
			  /* error reading inset -- do nothing */
		      }
		      text_Destroy(tx);
		  }
		  else {
		      /* not a text inset -- do nothing */
		  }
	      }
	  }

	  if (usual) {
	      for (res = inbuf; *res; res++)
		  dynstr_addchar(&pattern, *res);
	      while ((c = fgetc(tmp_file)) != EOF)
		  dynstr_addchar(&pattern, c);
	  }
      }
    }
    im_CloseFromCutBuffer(textview_GetIM(tv), tmp_file);
    goto compilestate;

  successstate:

    dynstr_put(&prompt, "");
    if (StackTop->wrappedp)
	dynstr_append(&prompt, "Wrapped ");
    dynstr_append(&prompt, "I-Search");
    if (!forwardp)
	dynstr_append(&prompt, " backward");
    dynstr_append(&prompt, ": ");
    dynstr_append(&prompt, pattern.text);
    message_DisplayString(tv, 0, prompt.text);
    im_ForceUpdate();
    while (1) {
	c = im_GetCharacter(textview_GetIM(tv));
	if ( ! owatch_Check(watchtv)) goto exitstate;
	switch (c) {
	  case CTRL_L:
	    im_RedrawWindow(textview_GetIM(tv));
	    break;
	  case CTRL_G:
	    textview_SetDotPosition(tv, origpos);
	    textview_SetDotLength(tv, origlen);
	    textview_FrameDot(tv, origpos);
	    message_DisplayString(tv, 0, "Cancelled.");
	    goto cleanup_and_return;
	  case CTRL_H:
	  case DEL:
	    goto popstate;
	  case CTRL_Q:
	    while ((c = im_GetCharacter(textview_GetIM(tv))) == EOF)
		;
	    if ( ! owatch_Check(watchtv)) goto exitstate;
	    dynstr_addchar(&pattern, c);
	    goto compilestate;
	  case CTRL_R:
	  case CTRL_S:
	    oldforwardp = forwardp;
	    forwardp = (c == CTRL_S);
	    if (oldforwardp == forwardp) {
		if (forwardp)
		    newsearchfrom = StackTop->position + 1;
		else
		    newsearchfrom = StackTop->position - 1;
		goto newsearchstate;
	    }
	    goto successstate;
	  case CTRL_W:
	    if (textview_GetDotLength(tv) > 0)
		goto appendselectionstate;
	    break;
	  case CTRL_Y:
	    goto appendkillheadstate;
	  case ESC:
	    wasmeta = im_WasMeta(textview_GetIM(tv));
	    /* Fall through */
	  case EOF:
	    goto exitstate;
	  default:
	    if (isascii(c) && (isprint(c) || isspace(c))) {
		dynstr_addchar(&pattern, c);
		goto compilestate;
	    }
	    else {
		dodokey = 1;
		goto exitstate;
	    }
	}
    }

  failurestate:

    dynstr_put(&prompt, "Failing ");
    if (StackTop->wrappedp)
	dynstr_append(&prompt, "wrapped ");
    dynstr_append(&prompt, "I-search");
    if (!forwardp)
	dynstr_append(&prompt, " backward");
    dynstr_append(&prompt, ": ");
    dynstr_append(&prompt, pattern.text);
    message_DisplayString(tv, 0, prompt.text);
    im_ForceUpdate();
    while (1) {
	c = im_GetCharacter(textview_GetIM(tv));
	if ( ! owatch_Check(watchtv)) goto exitstate;
	switch (c) {
	  case CTRL_L:
	    im_RedrawWindow(textview_GetIM(tv));
	    break;
	  case CTRL_G:
	    goto popbeforeerrorstate;
	  case CTRL_H:
	  case DEL:
	    goto popstate;
	  case CTRL_Q:
	    while ((c = im_GetCharacter(textview_GetIM(tv))) == EOF)
		;
	    if ( ! owatch_Check(watchtv)) goto exitstate;
	    dynstr_addchar(&pattern, c);
	    goto compilestate;
	  case CTRL_R:
	  case CTRL_S:
	    oldforwardp = forwardp;
	    forwardp = (c == CTRL_S);
	    if (oldforwardp == forwardp)
		goto wrapsearchstate;
	    goto searchstate;
	  case CTRL_W:
	    if (textview_GetDotLength(tv) > 0)
		goto appendselectionstate;
	    break;
	  case CTRL_Y:
	    goto appendkillheadstate;
	  case ESC:
	    wasmeta = im_WasMeta(textview_GetIM(tv));
	    /* Fall through */
	  case EOF:
	    goto exitstate;
	  default:
	    if (isascii(c) && (isprint(c) || isspace(c))) {
		dynstr_addchar(&pattern, c);
		goto compilestate;
	    }
	    else {
		dodokey = 1;
		goto exitstate;
	    }
	}
    }

cleanup_and_return:
	    im_ForceUpdate();
	    dynstr_destroy(&prompt);
	    dynstr_destroy(&pattern);
	    statestack_destroy(&stack);
	    owatch_Delete(watchtv);
	    return;
}

static void     fsearch(tv, key)
struct textview *tv;
long            key;
{
    dosearch(tv, 1);
}

static void     rsearch(tv, key)
struct textview *tv;
long            key;
{
    dosearch(tv, 0);
}

boolean         gsearch__InitializeClass()
{
    static struct bind_Description fns[] = {
        {"gsearch-forward", NULL, 0, NULL, 0, 0, fsearch,
        "Search forward incrementally.", "gsearch"},
        {"gsearch-backward", NULL, 0, NULL, 0, 0, rsearch,
	     "Search backward incrementally.", "gsearch"},
        {NULL},
    };
    struct classinfo *textviewClassinfo;

    if (dynstr_init(&LastPattern))
	return FALSE;
    textviewClassinfo = class_Load("textview");
    if (textviewClassinfo != NULL) {
        bind_BindList(fns, NULL, NULL, textviewClassinfo);
        return TRUE;
    }
    else
        return FALSE;
}
