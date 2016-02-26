/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */
/*	$Header: /home/sau/mgr/nsrc/RCS/font_subs.c,v 1.4 92/09/08 10:51:07 sau Exp Locker: sau $
	$Source: /home/sau/mgr/nsrc/RCS/font_subs.c,v $
*/
static char	RCSid_[] = "$Source: /home/sau/mgr/nsrc/RCS/font_subs.c,v $$Revision: 1.4 $";

/* font routines */

#include "bitmap.h"
#include "font.h"
#include "default_font.h"
#include "window.h"
#include <stdio.h>
#ifdef DEBUG
#   include "defs.h"
#endif

#define HEAD(x)		font->head.x

/**************************************************************************
 *
 *	set up a font file
 */

struct font *
open_font(file)
char *file;			/* name of font file */
   {
   FILE *fp;
   int size;
   char *malloc();
   struct font *font, *open_sfont();

   if (file == (char *) 0 || *file == '\0') {
      return(open_sfont(default_font_head,&default_font));
      }

#ifdef DEBUG
      dprintf(f)(stderr,"Opening font file [%s]\n",file);
#endif

   if ((fp=fopen(file,"r"))  == NULL)
      return((struct font *)0);

   if ((font=(struct font *) malloc(sizeof(struct font))) == (struct font *)0) {
      fclose(fp);
      return((struct font *)0);
      }

   if (fread(&(font->head),HEADER_SIZE,1,fp) != 1) {
      free((char *) font);
      fclose(fp);
      return((struct font *)0);
      }

   if (HEAD(type) != FONT_A) {
      free((char *) font);
      fclose(fp);
      return((struct font *)0);
      }
                               
	/* fonts are always 32 bit aligned, 1 bit per pixel */

	size = (HEAD(wide)*HEAD(count)+31)&~31;

   font->data = bit_alloc(size,HEAD(high),NULL_DATA,1);
   font->table = (struct entry **) 0;

   /* read in font data */

   size = BIT_SIZE(font->data);
   fread(BIT_DATA(font->data), size, 1, fp);
	SET_DIRTY(font->data);

   /* create individual characters */

	font->glyph = (BITMAP **) malloc(sizeof(BITMAP *) * MAXGLYPHS);
   glyph_create(font,font->glyph,0);

   fclose(fp);
   return(font);
   }

/* add bold face and underlining to a font */

int
enhance_font(font)
struct font *font;			/* font to be enhanced */
	{
	BITMAP *data;				/* new bitmap data */
	BITMAP **glyph;			/* new font glyphs */
	int size;					/* current font size */
	register int i;

	if (HEAD(type)&0x80)		/* already enhanced */
		return(0);

	/* make data structures larger, copy existing data */

	glyph = (BITMAP **) malloc(sizeof(BITMAP *) * MAXGLYPHS * 4);
	for(i=0;i<MAXGLYPHS;i++)
		bit_destroy(font->glyph[i]);
	free(font->glyph);
	font->glyph = glyph;

	size = BIT_WIDE(font->data);
   data = bit_alloc(size*4,HEAD(high),NULL_DATA,1);
   bit_blit(data,0,0,size,HEAD(high),BIT_SRC,font->data,0,0);

	if (HEAD(type) == FONT_S)		/* watch out for static font data */
		HEAD(type) = FONT_A;
	else
		bit_destroy(font->data);
	font->data = data;

	/* make extra font glyph pointers */

	for(i=0;i<4;i++)
		glyph_create(font,font->glyph + i*MAXGLYPHS,i*size);

	/* make font "wider"; tack "enhancements" on to the right */

   bit_blit(data,size,0,size,HEAD(high),BIT_SRC,font->data,0,0); /* copy font */
	for(i=0;i<MAXGLYPHS;i++)			/* under line it */
		if (i != ' ')
			bit_blit(font->glyph[MAXGLYPHS+i],0,0,HEAD(wide),HEAD(high),
						BIT_SRC|BIT_DST, font->glyph['_'],0,0);

	/* now for bold and bold_underline */

   bit_blit(data,2*size,0,2*size,HEAD(high),BIT_SRC,font->data,0,0);
	for(i=0;i<MAXGLYPHS*2;i++)			/* embolden */
			bit_blit(font->glyph[2*MAXGLYPHS+i],1,0,HEAD(wide),HEAD(high),
						BIT_SRC|BIT_DST, font->glyph[i],0,0);

	/* mark font "expanded" */

	HEAD(type) |= 0x80;
	return(1);
	}

/**************************************************************************
 *
 *	deallocate a font
 */

int
free_font(dead_font)
struct font *dead_font;		/* font to be deallocated */
   {
   register int i;
	int count;		/* # of glyphs to trash */

   if (!dead_font)
      return(-1);

	count = dead_font->head.type & 0x80 ? 4*MAXGLYPHS : MAXGLYPHS;
	dead_font->head.type &= ~0x80;

   for(i=0;i<count;i++)
      if (dead_font->glyph[i])
         bit_destroy(dead_font->glyph[i]);
   if (dead_font->head.type != FONT_S)
      bit_destroy(dead_font->data);
	free(dead_font->glyph);
   zap_fhash(dead_font);		/* free up hash table space */
   i=font_purge(dead_font);	/* eliminate references to dead font */

#ifdef DEBUG
      dprintf(f)(stderr,"freeing font %d (%d references)\n",
      dead_font->ident,i);
#endif

   free((char *) dead_font);
   }

/**************************************************************************
 *
 *	set up a static font file
 */

struct font *
open_sfont(head,data)
struct font_header head;	/* font header */
BITMAP *data;		/* array of bits */
   {
   char *malloc();
   struct font *font;

   if ((font=(struct font *) malloc(sizeof(struct font))) == (struct font *)0)
      return((struct font *)0);

   font->head = head;
   font->data = data;
   font->head.type = FONT_S;
   font->table = (struct entry **) 0;

   /* create individual characters */

	font->glyph = (BITMAP **) malloc(sizeof(BITMAP *) * MAXGLYPHS);
   glyph_create(font,font->glyph,0);

   return(font);
   }


static
glyph_create(font,glyph,offset)
struct font *font;	/* the font to glyphiffy */
BITMAP **glyph;		/* where to put the glyph pitmaps */
int offset;				/* x pffset into font data to start glyphs */
   {
   register int i, x;
   int		first = HEAD(start);
   int		last = HEAD(start) + HEAD(count);
   int		wide = HEAD(wide);
   int		high = HEAD(high);
   int		nochar;
   
   /* Pick the character to be printed for characters not in the set.
      Normally, it is the character specified by C_NOCHAR, but it that isn't
      in the range of the set, we pick the first character (which is usually
      a space).
   */
   nochar = C_NOCHAR - HEAD(start);
   if( nochar >= last )
	nochar = 0;
   nochar = nochar*wide + offset;

   x = offset;
   for(i=0; i<MAXGLYPHS; i++)
      if (i < first || i >= last)
         glyph[i] = bit_create(font->data, nochar, 0, wide, high);
      else {
         glyph[i] = bit_create(font->data, x, 0, wide, high);
         x += wide;
         }
   }
