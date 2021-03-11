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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/preview/RCS/dvi2disp.c,v 2.10 1992/12/15 21:39:11 rr2b R6tape $";
#endif


 


/* ***************************************************************** */

/* 	dvi2disp.c	Troff DVI preview program */

/* 
  	Uses the ITC window manager. 
	Part of prview.c

 */

/* ***************************************************************** */


#include <class.h>
#include <math.h>
#include <ctype.h>
#include <andrewos.h>
#define AUXMODULE 1
#include <preview.eh>
#include <view.ih>
#include <graphic.ih>
#include <fontdesc.ih>

preview_coordinate	DX = 5;	/* step size in x */
preview_coordinate	DY = 5;	/* step size in y */

preview_coordinate   maxdots = 32000; /* maximum number of dots in an object */

static int   geti (self )
struct preview *self;
{
   register    FILE * f = self->DviFileIn;
   register int   n = 0;
   register    Boolean neg = FALSE;
   register int   c;

   if (self->peekc == 0 || isspace(c = self->peekc))
      while (isspace(c = getc(f)));
   if (c == '-')
      {
	 neg = TRUE;
	 c = getc(f);
      }
   while (isdigit(c))
      {
	 n = n * 10 + c - '0';
	 c = getc(f);
      }
   self->peekc = c;
   return(neg ? -n : n);
}

static char *pregets (self) struct preview *self;
{
   register    FILE * f = self->DviFileIn;
   register char *p;
   static   Preview_Line buf;
   register    c;
   if (self->peekc == 0 || isspace(c = self->peekc))
      while (isspace(c = getc(f)));
   p = buf;
   while (c != EOF && !isspace(c))
      {
	 *p++ = c;
	 c = getc(f);
      }
   *p++ = '\0';
   self->peekc = c;
   return(buf);
}


/* ************************************************************ */
/* 								 */
/* 								 */
/* ************************************************************ */
/* 
	compute the physical position; 
	      return FALSE if not on screen
 */

static Boolean SetPosition(self)
register struct preview *self;
{
   register    cvx = self->LogicalX * self->DisplayResolution / self->InputResolution;
   register    cvy = self->LogicalY * self->DisplayResolution / self->InputResolution;
   if (cvx+self->xoff < 0 || self->WindowWidth+self->cursize <= cvx+self->xoff 
	|| cvy+self->yoff < 0 || self->WindowHeight+self->cursize <= cvy+self->yoff)
	return(FALSE);
   if (self->PhysicalX != cvx || self->PhysicalY != cvy)
      {
      	preview_MoveTo(self,cvx + self->xoff, cvy + self->yoff);
   	self->PhysicalX = cvx;
   	self->PhysicalY = cvy;
      }
   return(TRUE);
}


/* ************************************************************ */
/* 								 */
/* 								 */
/* ************************************************************ */

static UseFont(self,f,s)
struct preview *self;
int f,s;
{
   register    h, l, m;
   register struct preview_fontname  *p;
   int n;

   s = self->DisplayResolution*s/preview_DISPLAY_RESOLUTION;
   n  = f*60+s;

   h = self->NWMFonts - 1;
   l = 0;
   while (l < h)
      {
	 m = (l + h) >> 1;
	 p = &(self->WMFonts[m]);
	 if (n < p->number)
	    h = m - 1;
	 else
	    if (n > p->number)
	       l = m + 1;
	 else
	    {
	       l = m;
	       break;
	    }
      }
   p = &(self->WMFonts[l]);
   if (self->NWMFonts == 0 || p->number != n)
      {
	 Preview_Line fname;
	 static char *format[] =
	 {
	    "AndySymbol%d",
	    "AndySymbolA%d",
	    "Andy%d",
	    "Andy%db",
	    "Andy%di",
	    "Andy%dbi",
	    "AndyType%df",
	    "AndyType%dbf",
	    "AndyType%dif",
	    "AndyType%dbif",
	    "AndySans%d",
	    "AndySans%db",
	    "AndySans%di",
	    "AndySans%dbi",
	    "AndyType%df",
	    "AndyType%dbf",
	    "AndyType%dif",
	    "AndyType%dbif",
	 };
	 bcopy(p, p + 1,(self->NWMFonts - l) * sizeof(struct preview_fontname));
	 p->number = n;
	 if (f >= 17)
	     p->font = NULL;
	 else {
		char namebuf[100];
		long style;
		long size;
	 	sprintf(fname, format[f], s);
		fontdesc_ExplodeFontName(fname,namebuf,(long) sizeof(namebuf),&style,&size);
		p->font = fontdesc_Create(namebuf,style,size);
	      }
      }
   if (p->font != NULL) preview_SetFont(self,p->font);
}


/* ************************************************************ */
/* 								 */
/* 								 */
/* ************************************************************ */

static DrawThing(self)
struct preview *self;
{
   char *com = pregets (self);
   switch (com[0])
      {
	 case 'l':  /* draw line from current to +dx,+dy */
	       {
		  preview_coordinate   dx = geti (self);
		  preview_coordinate   dy = geti (self);
	/*  Don't use the SetPosition call here,since it won't move off screen */
		  preview_coordinate   cvx = self->LogicalX * self->DisplayResolution / self->InputResolution;
		  preview_coordinate    cvy = self->LogicalY * self->DisplayResolution / self->InputResolution;
                  if (self->PhysicalX != cvx || self->PhysicalY != cvy)
      			preview_MoveTo(self,cvx + self->xoff, cvy + self->yoff);
		  self->PhysicalX = cvx + dx * self->DisplayResolution / self->InputResolution;
		  self->PhysicalY = cvy + dy * self->DisplayResolution / self->InputResolution;
		  preview_DrawLineTo(self,self->PhysicalX + self->xoff, self->PhysicalY + self->yoff);
		  self->LogicalX += dx;
		  self->LogicalY += dy;
		  break;
	       }
	 case 'a': 
	       {
		  preview_coordinate   dx1,
		        dy1,
		        dx2,
		        dy2;
		  dx1 = geti(self);
		  dy1 = geti(self);
		  dx2 = geti(self);
		  dy2 = geti(self);
		  drawarc(self,dx1, dy1, dx2, dy2);
		  break;
	       }
	 case 'c': 
	       drawcirc(self,geti(self));
	       break;
	 case 'e': 
	       {
		  preview_coordinate   a,
		        b;
		  a = geti(self);
		  b = geti(self);
		  drawellip(self,a, b);
		  break;
	       }
	 case '~': 
	       {
		  char  buf[1000];
		  fgets(buf, sizeof buf, self->DviFileIn);
		  drawwig(self,buf);
		  break;
	       }
	 default: 
	       printf("Unimplemented draw command: %s\n", com);
	       break;
      }

   /* skip to end of line */
   while (self->peekc != '\n' && self->peekc != EOF)
      self->peekc = getc(self->DviFileIn);
}

/* ***************************************************************** */

#define Sansserif 8
#define Typewriter 4
#define Italic 2
#define Bold 1
#define SerifNormal 0
#define SerifItalic Italic
#define SerifBold Bold
#define SerifBoldItalic (Bold + Italic)
#define BoldItalic (Bold + Italic)

#define Symbol 0
#define SymbolA 1
#define Normal 2

struct trs
{
   char  name[2];
   unsigned char  ch;
   char  font;
}           trs[] =
{
               "!<", 0000, Symbol,
               "!=", 0071, SymbolA,
               "!>", 0000, Symbol,
               "**", 0052, Symbol,
               "*A", 0101, Symbol,
               "*B", 0102, Symbol,
               "*C", 0130, Symbol,
               "*D", 0104, Symbol,
               "*E", 0105, Symbol,
               "*F", 0106, Symbol,
               "*G", 0107, Symbol,
               "*H", 0121, Symbol,
               "*I", 0111, Symbol,
               "*K", 0113, Symbol,
               "*L", 0114, Symbol,
               "*M", 0115, Symbol,
               "*N", 0116, Symbol,
               "*O", 0117, Symbol,
               "*P", 0120, Symbol,
               "*Q", 0131, Symbol,
               "*R", 0122, Symbol,
               "*S", 0123, Symbol,
               "*T", 0124, Symbol,
               "*U", 0125, Symbol,
               "*W", 0127, Symbol,
               "*X", 0103, Symbol,
               "*Y", 0110, Symbol,
               "*Z", 0132, Symbol,
               "*a", 0141, Symbol,
               "*b", 0142, Symbol,
               "*c", 0170, Symbol,
               "*d", 0144, Symbol,
               "*e", 0145, Symbol,
               "*f", 0146, Symbol,
               "*g", 0147, Symbol,
               "*h", 0161, Symbol,
               "*i", 0151, Symbol,
               "*k", 0153, Symbol,
               "*l", 0154, Symbol,
               "*m", 0155, Symbol,
               "*n", 0156, Symbol,
               "*o", 0157, Symbol,
               "*p", 0160, Symbol,
               "*q", 0171, Symbol,
               "*r", 0162, Symbol,
               "*s", 0163, Symbol,
               "*t", 0164, Symbol,
               "*u", 0165, Symbol,
               "*w", 0167, Symbol,
               "*x", 0143, Symbol,
               "*y", 0150, Symbol,
               "*z", 0172, Symbol,
               "+-", 0061, SymbolA,
               "->", 0056, SymbolA,
               "<-", 0054, SymbolA,
               "<=", 0043, SymbolA,
               "==", 0072, SymbolA,
               ">=", 0063, SymbolA,
/* 
               "Fi", 0021, Normal,
               "Fl", 0022, Normal,
 */
               "\\'", '\'', Normal,
               "\\-", '-', Normal,
               "\\^",0000, Normal,
               "\\`", '`', Normal,
               "\\|", 0000, Normal,
/* 
	       "aa", 0000, Normal,
 */
               "ap", 0176, Symbol,
               "br", 0075, SymbolA,
               "bu", 0067, SymbolA,
               "bv", 0157, SymbolA,
               "ca", 0107, SymbolA,
               "co", 0123, SymbolA,
/* 
               "ct", 0000, Normal,
 */
               "cu", 0110, SymbolA,
	       "da", 0057, SymbolA,
/* 
               "dd", 0106, Symbol,
 */
               "de", 0060, SymbolA,
/* 
               "dg", 0041, Symbol,
 */
               "di", 0070, SymbolA,
/* 
               "em", 0000, Normal,
	       "en", 0000, Symbol,
 */
               "eq", 0075, Symbol,
               "es", 0106, SymbolA,
/* 
               "ff", 0006, Normal,
               "fi", 0024, Normal,
               "fl", 0025, Normal,
 */
               "fm", 0042, SymbolA,
/* 
	       "ga", 0000, Symbol,
 */
               "gr", 0121, SymbolA,
               "hy", 0055, Normal,
               "ib", 0115, SymbolA,
               "if", 0045, SymbolA,
               "ip", 0112, SymbolA,
               "is", 0162, SymbolA,
	       "lb", 0150, SymbolA,
	       "lc", 0151, SymbolA,
	       "lf", 0153, SymbolA,
	       "lk", 0155, SymbolA,
	       "lt", 0146, SymbolA,
               "mi", 0055, Symbol,
               "mo", 0116, SymbolA,
               "mu", 0064, SymbolA,
               "or", 0075, SymbolA,
               "pd", 0066, SymbolA,
               "pl", 0053, Symbol,
               "pt", 0065, SymbolA,
	       "rb", 0170, SymbolA,
	       "rc", 0171, SymbolA,
	       "rf", 0173, SymbolA,
	       "rk", 0175, SymbolA,
               "rn", 0140, Symbol,
	       "rt", 0166, SymbolA,
               "ru", 0137, Symbol,
               "sb", 0114, SymbolA,
/* 
	       "sc", 0000, Symbol,
 */
	       "sl", 0044, SymbolA,
               "sr", 0126, SymbolA,
	       "tm", 0124, SymbolA,

	       "ts", 0126, Symbol,
               "ua", 0055, SymbolA,
               "ul", 0137, Symbol,
               "~=", 0100, Symbol,
};

static ShowSpecial(self)
struct preview *self;
{
    register char  *s = pregets (self);
    register    h,
                l,
                m;
    register struct trs *p;
/* 
    if (s[0] == 'u' && s[1] == 'l' && s[2] == 0) {
	if (SetPosition(self))
	preview_EraseRectSize(self,self->PhysicalX + self->xoff, self->PhysicalY + 1 + self->yoff,
		(self->cursize * 17 * self->DisplayResolution + 1200) / 2400, 1);
    }
    else
	if (s[0] == 'r' && s[1] == 'u' && s[2] == 0) {
	    if (SetPosition(self))
	    preview_EraseRectSize(self,self->PhysicalX + self->xoff, self->PhysicalY + 1 + self->yoff,
		    (self->cursize * self->DisplayResolution + 120) / 240, 1);
	}
    else
	if (s[0] == 'b' && s[1] == 'r' && s[2] == 0) {
	    if (SetPosition(self))
	    preview_EraseRectSize(self,self->PhysicalX + self->xoff, self->PhysicalY + 2 + self->yoff, 1,
		    ((-10 * self->cursize) / 3 *self->DisplayResolution - 120) / 240);
	}
    else
 */
	if (s[0] == 'f' && s[1] == 'i' && s[2] == 0) {
	    if (SetPosition(self))
	    preview_DrawString(self,"fi",0);
	}
    else
	if (s[0] == 'f' && s[1] == 'f' && s[2] == 0) {
	    if (SetPosition(self))
	    preview_DrawString(self,"ff",0);
	}
    else
	if (s[0] == 'f' && s[1] == 'l' && s[2] == 0) {
	    if (SetPosition(self))
	    preview_DrawString(self,"fl",0);
	}
    else
	if (s[0] == 'F' && s[1] == 'i' && s[2] == 0) {
	    if (SetPosition(self))
	    preview_DrawString(self,"ffi",0);
	}
    else
	if (s[0] == 'F' && s[1] == 'l' && s[2] == 0) {
	    if (SetPosition(self))
	    preview_DrawString(self,"ffl",0);
	}
    else {
	l = 0;
	h = sizeof trs / sizeof trs[0];
	while (l < h - 1) {
	    m = (l + h) >> 1;
	    p = &trs[m];
	    if (s[0] < p->name[0]
		    || (s[0] == p->name[0] && s[1] < p->name[1]))
		h = m;
	    else
		l = m;
	}
	p = &trs[l];
	if (s[0] == p->name[0] && s[1] == p->name[1] && p->ch) {
	    struct fontdesc *ofont = preview_GetFont(self);
	    if (p->font != Normal)
		UseFont(self,p->font, self->cursize);
	    if (SetPosition(self))
		preview_DrawText(self,&(p->ch),1,0);
	    if (ofont != preview_GetFont(self))
		preview_SetFont(self,ofont);
	}
    }
    self->PhysicalX = -1;
}



/* ************************************************************ */
/* 								 */
/* 								 */
/* ************************************************************ */

static void DeviceControl(self)
struct preview *self;
{
   char  com = pregets (self)[0];
   if (self->debug) fprintf(stderr,"device control %c\n",com);
   switch (com)
      {
	 case 'i': 
	 case 'p': 
	 case 's': 
	 case 't': 
	       break;
	 case 'T': 
	       (void) pregets(self);
	       break;
	 case 'r': 
	       self->InputResolution = geti(self);
	       (void) geti(self);
	       (void) geti(self);
	       break;
	 case 'f': 
	       com = geti(self);
	       {
		  char *name = pregets (self);
		  int value;

		  value = SerifNormal;
		  switch (name[0]) {
		      case 'R':
		          break;
		      case 'I':
		          value = SerifItalic;
			  break;
		      case 'B':
		          value = (name[1] == 'I') ? SerifBoldItalic : SerifBold;
			  break;
		      case 'H':
		      case 'C':
		          value = (name[0] == 'C') ? Typewriter : Sansserif;
			  switch (name[1])  {
			      case 'R':
				  break;
			      case 'O':
			          value += Italic;
				  break;
			      case 'B':
			          value += Bold;
				  break;
			      case 'D':
			          value += BoldItalic;
				  break;
			  }
		  }
		  self->DviFonts[com] = value + 2;
	       }
	       break;
	 case 'H': 
	       self->cursize = geti(self);
	       UseFont(  self,self->curfont,  self->cursize);
	       break;
	 case 'S': 
	        self->slant = geti(self);
	       break;
	 default: 
	       printf("Unknown device command: %c\n", com);
	       break;
      }
}


/* ************************************************************ */
/* 								 */
/* 								 */
/* ************************************************************ */

static void DumpCharacter(self,c)
 struct preview *self;
char c;
{
       self->CharactersOnThisPage = TRUE;
       if (SetPosition(self))
       {
       if (c == '_')
	  preview_FillRectSize(self,self->PhysicalX +  self->xoff, self->PhysicalY + 1 +  self->yoff,
		(17 *  self->cursize *  self->DisplayResolution + 1200) / 2400, 1,preview_BlackPattern(self));
	else 
	    preview_DrawText(self,&c,1L,0);
/* 	  putc(c, winout); */
       }
       self->PhysicalX = -1;
}

void preview__DviToDisplay(self) 
struct preview *self;
{
   register    FILE * f =  self->DviFileIn;
   register int   c,lastc;

    self->CharactersOnThisPage = FALSE;
    self->peekc = getc(f);
   while ((c =  self->peekc) != EOF)
      {
	  self->peekc = 0;
	 if (isdigit(c))
	    {
	        self->LogicalX += (c - '0') * 10 + getc(f) - '0';
	       c = getc(f);
	       DumpCharacter(self,c);
	       if (self->debug)
	           fprintf(stderr, "%d%c",  self->LogicalX, c);
	    }
	 else  {
	    if (self->debug)
	       fprintf(stderr,"%c ", c);
	    switch (c)
	       {
		  case ' ': 
		  case '\t': 
		  case '\n': 
			break;

		  case 's': 
			 self->cursize = geti(self);
			UseFont( self, self->curfont,  self->cursize);
			break;

		  case 'f': 
			 self->curfont =  self->DviFonts[geti(self)];
			UseFont( self, self->curfont,  self->cursize);
			break;

		  case 'c': 
	       		c = getc(f);
			DumpCharacter(self,c);
			break;

		  case 'C': 
			ShowSpecial(self);
			break;

		  case 'H': 
			 self->LogicalX = geti(self);
			break;

		  case 'V': 
			 self->LogicalY = geti(self);
			break;

		  case 'h': 
			 self->LogicalX += geti(self);
			break;

		  case 'v': 
			 self->LogicalY += geti(self);
			break;

		  case 'n': 
			(void) geti(self);
			(void) geti(self);
			break;

		  case 'w': 
			break;

		  case 'p': 
			return;

		  case 'x': 
			DeviceControl(self);
			break;

		  case 'D': 
			DrawThing(self);
			break;
		  case '%':
		      /* skip over postscript */
		      while((c = getc(f)) != EOF){
			  if(c == '.' && lastc == '\n' && (((c = getc(f)) == '\n') || c == EOF))
			      break;
			  lastc = c;
		      }
		      break;
	       }
	 }
	 if ( self->peekc == 0)
	     self->peekc = getc(f);
	 if ( self->SizeChanged/* || --(PollCount) == 0 && PollWinin() */)
	    return;

      }
}



/* ******************************************************************* *\
* 								       *
* 	File: draw.c						       *
* 	Date: Tue Apr  3 16:43:15 1984				       *
* 	Author: ?						       *
* 							               *
* This file started out as draw.c from the Unix DVI troff distribution *
* It contains all the intense routines that understand arcs and splines*
* 								       *
* Converted to deal with the WM by James Gosling.		       *
* 								       *
* 								       *
\********************************************************************* */


#define	PI	3.141592654
#define	hmot(n)		self->LogicalX += n
#define	hgoto(n)	self->LogicalX = n
#define vgoto(n)	 	self->LogicalY = n
#define	vmot(n)		self->LogicalY += n

#define	sgn(n)	((n > 0) ? 1 : ((n < 0) ? -1 : 0))
#define	abs(n)	((n) >= 0 ? (n) : -(n))
#define	max(x,y)	((x) > (y) ? (x) : (y))
#define	min(x,y)	((x) < (y) ? (x) : (y))
#define	arcmove(X,Y)	{ hgoto(X); vmot(-y-(Y)); }


#define scale(v) (((v)* self->DisplayResolution+( self->InputResolution>>1))/ self->InputResolution)

static drawwig(self,s)	/* draw wiggly line */
struct preview *self;
	char *s;
{
    preview_coordinate     xc[50],
            yc[50],
            xp,
            yp,
            pxp,
            pyp;
    float   t1,
            t2,
            t3,
            w;
    preview_coordinate     i,
            j,
            numdots,
            N;
    char    temp[50],
           *p,
           *getstr ();

    p = s;
    for (N = 2; (p = getstr (p, temp)) != NULL && N < sizeof (xc) / sizeof (xc[0]); N++) {
	xc[N] = atoi (temp);
	p = getstr (p, temp);
	yc[N] = atoi (temp);
    }
    xc[0] = xc[1] = self->LogicalX;
    yc[0] = yc[1] =  self->LogicalY;
    for (i = 1; i < N; i++) {
	xc[i + 1] += xc[i];
	yc[i + 1] += yc[i];
    }
     self->LogicalX = xc[N] = xc[N - 1];
     self->LogicalY = yc[N] = yc[N - 1];
    for (i = 0; i <= N; i++) {
	xc[i] = scale(xc[i]);
	yc[i] = scale(yc[i]);
    }
    preview_MoveTo (self,xc[1]+ self->xoff, yc[1]+ self->yoff);
    pxp = pyp = -9999;
    for (i = 0; i < N - 1; i++) {/* interval */
	numdots = (dist (xc[i], yc[i], xc[i + 1], yc[i + 1]) + dist (xc[i + 1], yc[i + 1], xc[i + 2], yc[i + 2])) / 2;
	numdots /= DX;
	numdots = min (numdots, maxdots);
	for (j = 0; j < numdots; j++) {/* points within */
	    w = (float) j / numdots;
	    t1 = 0.5 * w * w;
	    w = w - 0.5;
	    t2 = 0.75 - w * w;
	    w = w - 0.5;
	    t3 = 0.5 * w * w;
	    xp = t1 * xc[i + 2] + t2 * xc[i + 1] + t3 * xc[i] + 0.5;
	    yp = t1 * yc[i + 2] + t2 * yc[i + 1] + t3 * yc[i] + 0.5;
	    if (xp != pxp || yp != pyp) {
		preview_DrawLineTo (self,xp+self->xoff, yp+self->yoff);
		pxp = xp;
		pyp = yp;
	    }
	}
    }
}

static char *getstr(p, temp)	/* copy next non-blank string from p to temp, update p */
char *p, *temp;
{
    while (*p == ' ' || *p == '\t' || *p == '\n')
	p++;
    if (*p == '\0') {
	temp[0] = 0;
	return (NULL);
    }
    while (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\0')
	*temp++ = *p++;
    *temp = '\0';
    return (p);
}

static drawcirc (self,d) 
struct preview *self;{
    preview_coordinate     xc, yc;
    xc = self->LogicalX;
    yc = self->LogicalY;
    conicarc (self, self->LogicalX + d / 2, - self->LogicalY,  self->LogicalX, - self->LogicalY,  self->LogicalX, - self->LogicalY, d / 2, d / 2);
    hgoto (xc + d);		/* circle goes to right side */
    vgoto (yc);
}

static dist(x1, y1, x2, y2)	/* integer distance from x1,y1 to x2,y2 */
{
    float   dx,
            dy;
    dx = x2 - x1;
    dy = y2 - y1;
    return sqrt (dx * dx + dy * dy) + 0.5;
}

static drawarc(self,dx1, dy1, dx2, dy2)
struct preview *self;
{
    preview_coordinate     x0,
            y0,
            x2,
            y2,
            r;
    x0 =  self->LogicalX + dx1;		/* center */
    y0 =  self->LogicalY + dy1;
    x2 = x0 + dx2;		/* "to" */
    y2 = y0 + dy2;
    r = sqrt ((float) dx1 * dx1 + (float) dy1 * dy1) + 0.5;
    conicarc (self,x0, -y0,  self->LogicalX, - self->LogicalY, x2, -y2, r, r);
     self->LogicalX = x2;
     self->LogicalY = y2;
}

static drawellip(self,a, b)
struct preview *self;
{
    preview_coordinate     xc,
            yc;
    xc =  self->LogicalX;
    yc =  self->LogicalY;
    conicarc (self, self->LogicalX + a / 2, - self->LogicalY,  self->LogicalX, - self->LogicalY,  self->LogicalX, - self->LogicalY, a / 2, b / 2);
    hgoto (xc + a);
    vgoto (yc);
}

#define sqr(x) (long int)(x)*(x)

static conicarc(self,xp, yp, x0, y0, x1, y1, a, b)
struct preview *self;
{
	/* based on Bresenham, CACM, Feb 77, pp 102-3 */
	/* by Chris Van Wyk */
	/* capitalized vars are an internal reference frame */
	long dotcount = 0;
	preview_coordinate	xs, ys, xt, yt, Xs, Ys, qs, Xt, Yt, qt,
		M1x, M1y, M2x, M2y, M3x, M3y,
		Q, move, Xc, Yc;
	preview_coordinate ox1, oy1;
	long	delta;
	float	xc, yc;
	float	radius, slope;
	float	xstep, ystep;

	xp = scale(xp);
	yp = scale(yp);
	x0 = scale(x0);
	y0 = scale(y0);
	x1 = scale(x1);
	y1 = scale(y1);
	a = scale(a);
	b = scale(b);
	ox1 = x1;
	oy1 = y1;
	if (a != b)	/* an arc of an ellipse; internally, will still think of circle */
		if (a > b) {
			xstep = (float)a / b;
			ystep = 1;
			radius = b;
		} else {
			xstep = 1;
			ystep = (float)b / a;
			radius = a;
		} 
	else {	/* a circular arc; radius is computed from center and first point */	
		xstep = 1; ystep = 1;
		radius = sqrt((float)(sqr(x0 - xp) + sqr(y0 - yp)));
	}


	xc = x0;
	yc = y0;
	/* now, use start and end point locations to figure out
	the angle at which start and end happen; use these
	angles with known radius to figure out where start
	and end should be
	 */
	slope = atan2((double)(y0 - yp), (double)(x0 - xp) );
	if (slope == 0.0 && x0 < xp)
		slope = 3.14159265;
	x0 = xp + radius * cos(slope) + 0.5;
	y0 = yp + radius * sin(slope) + 0.5;
	slope = atan2((double)(y1 - yp), (double)(x1 - xp));
	if (slope == 0.0 && x1 < xp)
		slope = 3.14159265;
	x1 = xp + radius * cos(slope) + 0.5;
	y1 = yp + radius * sin(slope) + 0.5;
	/* step 2: translate to zero-centered circle */
	xs = x0 - xp;
	ys = y0 - yp;
	xt = x1 - xp;
	yt = y1 - yp;
	/* step 3: normalize to first quadrant */
	if (xs < 0)
		if (ys < 0) {
			Xs = abs(ys);
			Ys = abs(xs);
			qs = 3;
			M1x = 0;
			M1y = -1;
			M2x = 1;
			M2y = -1;
			M3x = 1;
			M3y = 0;
		} else {
			Xs = abs(xs);
			Ys = abs(ys);
			qs = 2;
			M1x = -1;
			M1y = 0;
			M2x = -1;
			M2y = -1;
			M3x = 0;
			M3y = -1;
		} 
	else if (ys < 0) {
		Xs = abs(xs);
		Ys = abs(ys);
		qs = 0;
		M1x = 1;
		M1y = 0;
		M2x = 1;
		M2y = 1;
		M3x = 0;
		M3y = 1;
	} else {
		Xs = abs(ys);
		Ys = abs(xs);
		qs = 1;
		M1x = 0;
		M1y = 1;
		M2x = -1;
		M2y = 1;
		M3x = -1;
		M3y = 0;
	}


	Xc = Xs;
	Yc = Ys;
	if (xt < 0)
		if (yt < 0) {
			Xt = abs(yt);
			Yt = abs(xt);
			qt = 3;
		} else {
			Xt = abs(xt);
			Yt = abs(yt);
			qt = 2;
		} 
	else if (yt < 0) {
		Xt = abs(xt);
		Yt = abs(yt);
		qt = 0;
	} else {
		Xt = abs(yt);
		Yt = abs(xt);
		qt = 1;
	}


	/* step 4: calculate number of quadrant crossings */
	if (((4 + qt - qs)
	     % 4 == 0)
	     && (Xt <= Xs)
	     && (Yt >= Ys)
	    )
		Q = 3;
	else
		Q = (4 + qt - qs) % 4 - 1;
	/* step 5: calculate initial decision difference */
	delta = sqr(Xs + 1)
	 + sqr(Ys - 1)
	-sqr(xs)
	-sqr(ys);
	/* here begins the work of drawing
   we hope it ends here too */
	preview_MoveTo (self,((preview_coordinate)xc)+ self->xoff, ((preview_coordinate)-yc)+ self->yoff);
	while ((Q >= 0)
	     || ((Q > -2)
	     && ((Xt > Xc)
	     && (Yt < Yc)
	    )
	    )
	    ) {
		if (dotcount++ > DX) {
			preview_DrawLineTo (self,((preview_coordinate)xc)+ self->xoff, (-(preview_coordinate)yc)+ self->yoff);
			dotcount = 0;
		}
		if (Yc < 0.5) {
			/* reinitialize */
			Xs = Xc = 0;
			Ys = Yc = sqrt((float)(sqr(xs) + sqr(ys)));
			delta = sqr(Xs + 1) + sqr(Ys - 1) - sqr(xs) - sqr(ys);
			Q--;
			M1x = M3x;
			M1y = M3y;
			 {
				int	T;
				T = M2y;
				M2y = M2x;
				M2x = -T;
				T = M3y;
				M3y = M3x;
				M3x = -T;
			}
		} else {
			if (delta <= 0)
				if (2 * delta + 2 * Yc - 1 <= 0)
					move = 1;
				else
					move = 2;
			else if (2 * delta - 2 * Xc - 1 <= 0)
				move = 2;
			else
				move = 3;
			switch (move) {
			case 1:
				Xc++;
				delta += 2 * Xc + 1;
				xc += M1x * xstep;
				yc += M1y * ystep;
				break;
			case 2:
				Xc++;
				Yc--;
				delta += 2 * Xc - 2 * Yc + 2;
				xc += M2x * xstep;
				yc += M2y * ystep;
				break;
			case 3:
				Yc--;
				delta -= 2 * Yc + 1;
				xc += M3x * xstep;
				yc += M3y * ystep;
				break;
			}
		}
	}
	preview_DrawLineTo (self,((preview_coordinate)ox1)+ self->xoff,(-(preview_coordinate)oy1)+ self->yoff);
}
