/* psmung.c
 *
 * This program takes the output of ArborText's dvips and mungs it for NeWS
 * previewing. This consists of putting all of the font definitions into
 * the end of the prologue.
 * Fonts definitions are started by lines of the form
 *
 *   XP /F25 /cmr10 .... RP
 *
 * Character definitions are started by lines of the form
 *
 *   XP /F25 100 ....
 *   .... RP
 */

#include <stdio.h>
#include <ctype.h>

/* Reading and Writing files is done a line at a time by the functions
 *
 *   char * GetLine(FILE *stream, char *line)
 *          PutLine(FILE *stream, char *line)
 */


char line[BUFSIZ];

#define GetLine(stream,line) fgets(line,BUFSIZ,stream)
#define PutLine(stream,line) fputs(line,stream)


/* search states */
#define SCANNING 0
#define SKIPPING 1
#define COPYING 2


#define GetNumber(num,ptr) while (isdigit(*ptr)) num = num * 10 + *ptr++ - '0'
#define SkipSpace(ptr) while (isspace(*ptr)) ptr++


struct FontInfo {
  struct FontInfo *next;
  int number;
  char named;
  char bits[256];
} *Fontlist;

#define GetNamed(fptr) ((*fptr).named)
#define SetNamed(fptr) (*fptr).named = 1

/*#define GetLetter(fptr,letter) ((*fptr).bits[letter>>3] & (1<<(letter&3)))
#define SetLetter(fptr,letter) (*fptr).bits[letter>>3] |= (1<<(letter&3))*/
#define GetLetter(fptr,letter) ((*fptr).bits[letter])
#define SetLetter(fptr,letter) (*fptr).bits[letter] = 1


/* find font array if available, create and clear otherwise */
struct FontInfo *FindFont(font)
     int font;
{
  struct FontInfo *fptr = Fontlist;

  while (fptr) { /* linear search for font number */
    if ((*fptr).number == font)
      return fptr;
    fptr = (*fptr).next;
  }
  /* number not found: create new entry and link in */
  fptr = (struct FontInfo *)calloc((unsigned)1,
				   (unsigned)sizeof(struct FontInfo));
  (*fptr).next = Fontlist;
  (*fptr).number = font;
  Fontlist = fptr;
  return fptr;
}


/* test if 'RP' found and chop before it */
int FoundRP(str)
     char *str;
{
  while (*str) {
    if (*str++ == 'R')
      if (*str == 'P') {
	*str = '\0';
	*--str = '\n';
	return 1;
      }
  }
  return 0;
}


/* Here we go, here we go, here we go... */
main(argc,argv)
     int argc;
     char *argv[];
{
  int state;
  FILE *pages;
  char *tmpname="/tmp/ps.XXXXXX";

  Fontlist = NULL; /* initialise font list */

  /* (re)open stdin file */
  /* (re)open stdout file */
  
  pages = fopen(mktemp(tmpname),"w+"); /* open pages file */

  while (GetLine(stdin,line) && strncmp(line,"%%EndProlog",11))
    PutLine(stdout,line); /* pass header straight through */

  PutLine(pages,line); /* save first non-header line */

  PutLine(stdout,"XP\n");

  state = SCANNING;
  while (GetLine(stdin,line)) {
    switch (state) {
    case SCANNING: /* scan for XP lines */
      if (!strncmp(line,"XP /F",5)) { /* XP_/F found */
	char *cptr = &line[5];
	int fontnum = 0;
	struct FontInfo *fontptr;

	GetNumber(fontnum,cptr); /* parse font number */
	SkipSpace(cptr); /* skip white space */
	fontptr = FindFont(fontnum); /* find font record */

	if (*cptr == '/') { /* font definition found */
	  if (GetNamed(fontptr)) { /* check if font definition already done */
	    if (!FoundRP(line))
	      state = SKIPPING;
	  }
	  else {
	    SetNamed(fontptr);
	    if (!FoundRP(line))
	      state = COPYING;
	    PutLine(stdout,&line[3]); /* put line without XP & RP */
	  }
	}
	else if (isdigit(*cptr)) { /* character definition found */
	  int letter = 0;
	  GetNumber(letter,cptr); /* parse char number */
	  if (!GetNamed(fontptr))
	    fprintf(stderr,"Warning: font %d not defined (char %d)\n",
		    fontnum,letter);
	  if (GetLetter(fontptr,letter)) { /* check if letter already done */
	    if (!FoundRP(line))
	      state = SKIPPING;
	  }
	  else {
	    SetLetter(fontptr,letter);
	    if (!FoundRP(line))
	      state = COPYING;
	    PutLine(stdout,&line[3]); /* put line without XP & RP */
	  }
	}
	else fprintf(stderr,"Ignoring line:\n  %s",line);
      } else PutLine(pages,line);
      break;
    case SKIPPING: /* skipping to RP */
      if (FoundRP(line))
	state = SCANNING;
      break;
    case COPYING: /* copying to RP */
      if (FoundRP(line)) /* FoundRP chops line */
	state = SCANNING;
      PutLine(stdout,line);
      break;
    }
  }

  PutLine(stdout,"RP\n");

  rewind(pages); /* goto start of pages file */
  while (GetLine(pages,line))
    PutLine(stdout,line); /* copy pages to output */

  fclose(pages); /* close pages file */
  /* delete pages file */
}

