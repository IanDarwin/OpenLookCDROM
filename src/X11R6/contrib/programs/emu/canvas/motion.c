#if !defined(lint) && !defined(__clipper__)
static char *rcsid = "motion.c,v 1.3 1994/06/02 20:07:43 me Exp";
#endif

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/*
 * Functions for parsing and executing motion state machines.
 *
 * Author: Jordan K. Hubbard
 * Date: July 29th, 1991.
 * Description: Routines for handling movement within the canvas in
 *		a user-configurable fashion.
 *
 * Revision History:
 *
 * motion.c,v
 * Revision 1.3  1994/06/02  20:07:43  me
 * use bitstream.h instead of bitstring.h
 *
 * Revision 1.2  1994/05/24  19:55:47  me
 * New Copyright
 * fixed bug where the text cursor dissappeared upon a move_abs
 *
 * Revision 1.1.1.1  1994/05/22  11:22:41  me
 * Initial import into CVS
 *
 * Revision 1.12  92/05/16  06:25:38  jkh
 * Synchronization checkin.
 * 
 * Revision 1.11  92/02/26  11:32:57  me
 * Steve Crooks clix port and general code cleanup
 */

#include "canvas.h"
#include "bitstream.h"

/* How many bits to reserve for characters */
#define C_SIZE		256

#define WHITE	"\n\t\r \014"
#define PUNCT	"!@#$%^&*()_-+={}[]:;'|\\~`'<>,./?\042"
#define ALPHA "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890"

struct _motion {
     bit_decl(pat, C_SIZE);
     unsigned int mask;
     int yes, no;
};
typedef struct _motion Motion, *MotionPtr;

struct _motionList {
     struct _motionList *next;
     String name;
     MotionPtr m;
};
typedef struct _motionList MotionList, *MotionListPtr;

Local MotionPtr getMotion(TermCanvasWidget, String);
Local MotionPtr findMotion(TermCanvasWidget, String);
Local String getEntry(String, String, bit_ref(bits), unsigned int *,
		      int *, int *);

/*
 * cfunc takes a parameter: ScreenCursorPtr scpt
 * that tells it from where to move.  It returns the new position there too.
 */
Export ScreenCursor
canvasDoMotion(TermCanvasWidget w, String name, XpTermMotionFunc cfunc,
	       ScreenCursor sc)
{
     MotionPtr m;
     ScreenCursor actsc;
     ScreenCursor retsc;
     int attr,ch, act_pos = 0, new_pos = 0;

     actsc = retsc = sc;
     
     m = getMotion(w, name);
     if (!m)
	  warn("Couldn't find motion type '%s'", name);
     else {
	  ch = (*cfunc)(w, &sc, &attr);
	  while (new_pos != MOTION_STOP && ch != EOF) {
	       if (ch >= 0) {
		    if (bit_test(m[act_pos].pat, ch) && (!m[act_pos].mask ||
			(attr & m[act_pos].mask)))
			 new_pos = m[act_pos].yes;
		    else
			 new_pos = m[act_pos].no;
		    if (new_pos >= 0)
			 act_pos = new_pos;
	       }
	       if (new_pos == MOTION_CONT) {
		    retsc = actsc;
		    actsc = sc;
		    ch = (*cfunc)(w, &sc, &attr);
	       }
	  }
     }
     return retsc;
}

Export void
canvasFreeMotions(TermCanvasWidget w)
{
     MotionListPtr q;

     q = (MotionListPtr)w->term.motionHead;
     while (q) {
	  MotionListPtr next = q->next;

	  XtFree((caddr_t)q->name);
	  XtFree((caddr_t)q->m);
	  XtFree((char *)q);
	  q = next;
     }
}

Local String
skip_space(String s)
{
     while (isspace(*s))
	  ++s;
     return s;
}

Local MotionPtr
findMotion(TermCanvasWidget w, String mtype)
{
     MotionListPtr p = (MotionListPtr) w->term.motionHead;

     while (p != NULL && strcmp(p->name, mtype) != 0) {
	  p = p->next;
     }
     if (p)
         return p->m;
     else
	 return NULL;
}

Local MotionPtr
getMotion(TermCanvasWidget w, String mtype)
{
     MotionPtr ret;
     char name[128];
     char class[128];
     String mdesc;

     ret = findMotion(w, mtype);
     if (ret == NULL) {
	  MotionListPtr p = (MotionListPtr)XtMalloc(sizeof(MotionList));

	  /* Set up the resource name */
	  sprintf(name, "motion-%s", mtype);
	  sprintf(class, "Motion-%s", mtype);
     
	  mdesc = get_sub_resource_string((Widget)w, NULL, name, class);
	  if (!mdesc) {
	       warn("getMotion: Couldn't get motion description '%s'",
		    mtype);
	       XtFree((caddr_t)p);
	  }
	  else {
	       mdesc = skip_space(mdesc);
	       if (!*mdesc) {
		    warn("Invalid motion description for %s.", mtype);
		    XtFree((char *)p);
		    return NULL;
	       }
	       else {
		    int mcount = 0;

		    p->name = XtNewString(mtype);
		    while (mdesc && *mdesc) {
			 int yes, no;
			 unsigned int mask;
			 bit_decl(tmp, C_SIZE);

			 mdesc = getEntry(mdesc, mtype, tmp, &mask, &yes, &no);
			 if (mdesc) {
			      if (!mcount)
				   p->m = (MotionPtr)XtMalloc(sizeof(Motion));
			      else
				   p->m = (MotionPtr)XtRealloc((caddr_t)p->m,
							       sizeof(Motion)
							       * mcount);
			      bcopy(tmp, p->m[mcount].pat,
				    _bit_size(C_SIZE) * 4);
			      p->m[mcount].mask = mask;
			      p->m[mcount].yes = yes;
			      p->m[mcount].no = no;
			      ++mcount;
			 }
		    }
		    ret = p->m;
	       }
	       if (!w->term.motionHead)
		    p->next = NULL;
	       else
		    p->next = (MotionListPtr)w->term.motionHead;
	       w->term.motionHead = (Generic)p;
	  }
     }
     return ret;
}

Local String
match_token(String s)
{
     static struct {
	  String from, to;
     } tokens[] = {
	  { "ALPHA",	ALPHA		},
	  { "PUNCT",	PUNCT		},
	  { "WHITE",	WHITE		},
	  { NULL, NULL			},
     };
     int i = 0;

     while (tokens[i].from) {
	  if (!strcmp(s, tokens[i].from))
	       return tokens[i].to;
	  ++i;
     }
     return NULL;
}
    
Local String
getEntry(String m, String name, bit_ref(bits), unsigned int *maskp,
	 int *yp, int *np)
{
     String str;
     char token[C_SIZE];
     int len, i = 0;

     /* Start clean */
     str = NULL;
     *maskp = 0;
     bit_clearall(bits, C_SIZE);

     m = skip_space(m);
     if (!*m)
	  return NULL;
get_token:
     if (isdigit(*m)) {
	  *maskp &= atoi(m);
	  while (isdigit(*m))
		++m;
     }
     else if (*m != '"') {
	  while (*m && !isspace(*m)) {
	       token[i++] = *(m++);
	       if (i == (C_SIZE - 1)) {
		    warn("Motion entry %s too long!", name);
		    return NULL;
	       }
	  }
	  token[i] = '\0';
	  if (!strcmp(token, "all") || !strcmp(token, "ALL")) {
	       bit_setall(bits, C_SIZE);
	  }
	  else {
	       str = match_token(token);
	       if (!str) {
		    warn("Motion entry %s has bogus token '%s'", name, token);
		    return NULL;
	       }
	  }
     }
     else {
	  ++m;
	  while (*m && *m != '"') {
	       token[i++] = *(m++);
	       if (i == (C_SIZE - 1)) {
		    warn("Motion entry %s too long!", name);
		    return NULL;
	       }
	  }
	  token[i] = '\0';
	  str = backslash_convert(token);
     }
     len = strlen(str);
     for (i = 0; i < len; i++)
	  bit_set(bits, str[i]);
     m = skip_space(m);

     if (*m == '|' || *m == '&') {
	  m = skip_space(m);
	  goto get_token;
     }

     if (!(isdigit(*m) || *m == '-' || *m == '+')) {
	  warn("Missing 'yes' index for '%s' motion entry.", name);
	  return NULL;
     }
     else {
	  *yp = atoi(m);
	  while (isdigit(*m) || *m == '-' || *m == '+')
	       ++m;
	  m = skip_space(m);
	  if (!(isdigit(*m) || *m == '-' || *m == '+')) {
	       warn("Missing 'no' index for '%s' motion entry.", name);
	       return NULL;
	  }
	  else {
	       *np = atoi(m);
	       while (isdigit(*m) || *m == '-' || *m == '+')
		    ++m;
	  }
     }
     return skip_space(m);
}
