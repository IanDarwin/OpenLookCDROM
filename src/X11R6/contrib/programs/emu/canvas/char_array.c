#if !defined(lint) && !defined(__clipper__)
static char *rcsid = "char_array.c,v 1.3 1994/06/02 10:58:16 me Exp";
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
 * character array manipulation routines
 *
 * Author: Michael Elbel
 * Date: March 20th, 1990.
 * Description: here are all routines that manipulate the structure of
 * 		character arrays including creation, resize, deleting lines
 * 		and so on.
 *
 * Revision History:
 *
 * char_array.c,v
 * Revision 1.3  1994/06/02  10:58:16  me
 * Changed float interfaced functions to double
 *
 * Revision 1.2  1994/05/24  19:55:35  me
 * New Copyright
 * fixed bug where the text cursor dissappeared upon a move_abs
 *
 * Revision 1.1.1.1  1994/05/22  11:22:40  me
 * Initial import into CVS
 *
 * [...]
 *
 * Revision 1.15  92/02/26  11:32:39  me
 * Steve Crooks clix port and general code cleanup
 * 
 * Revision 1.1  90/04/12  14:29:47  me
 * Initial revision
 */

#include "canvas.h"

#ifdef TRACK_MALLOC
#ifdef DOUBLE_FONTS 
Local int line_flags_count = 0;
#endif /* DOUBLE_FONTS */ 
Local int tab_list_count = 0;
Local int char_array_count = 0;
Local int line_array_count = 0;
Local int saved_area_count = 0;
Local int saved_line_count = 0;
#endif

#ifdef DOUBLE_FONTS
Export LineFlags
create_line_flags(int lines)
{
     LineFlags flags;

     if ((flags = (LineFlags)XtMalloc(lines * sizeof(LineFlagsElem))) == NULL)
	  fatal("couldn't allocate line flags");
#ifdef TRACK_MALLOC
     debug("allocated %d bytes for line flags %d",
	   columns * sizeof(LineFlagsElem),
	   line_flags_count++);
#endif
     return(flags);
}

/* Clears line flags*/
Export void
clear_line_flags(LineFlags flags, int lines)
{
     bzero(flags, lines * sizeof(LineFlagsElem));
}

/*
 * Creates new LineFlags with the specified size and copies the
 * old flags over.
 * The old flags are free'd.
 */
Export LineFlags
resize_line_flags(LineFlags old_flags, int old_lines, int new_lines)
{
     LineFlags new;
     int i;

     new = create_line_flags(new_lines);
     if (new_lines < old_lines)
	  bcopy(old_flags, new, new_lines * sizeof(LineFlagsElem));
     else {
	  clear_line_flags(new, new_lines);
	  bcopy(old_flags, new, old_lines * sizeof(LineFlagsElem));
     }

     XtFree((void *)old_flags);
#ifdef TRACK_MALLOC
     debug("Freeing line flags %d", --line_flags_count);
#endif
     
     return(new);
}
#endif /* DOUBLE_FONTS */ 

Export TabList
create_tab_list(int columns)
{
     TabList list;

     if ((list = (TabList)XtMalloc(columns * sizeof(int))) == NULL)
	  fatal("couldn't allocate Tab List");
#ifdef TRACK_MALLOC
     debug("allocated %d bytes for tab list %d", columns * sizeof(int),
	   tab_list_count++);
#endif
     return(list);
}

/* Clears a Tab List */
Export void
clear_tab_list(TabList list, int cols)
{
     bzero(list, cols * sizeof(int));
}

/* Initializes a Tab List to fixed steps */
Export void
init_fixed_tabs(TabList list, int cols, int step)
{
     int i;

     for (i = 0; i < cols; i += step)
	  *(list + i) = True;
}

/*
 * Creates a new TabList with the specified size and copies as many
 * old tabs over as will fit in the new list.
 * The old list is free'd.
 */
Export TabList
resize_tab_list(TabList old_list, int old_col, int new_col, int step)
{
     TabList new;
     int i;

     new = create_tab_list(new_col);
     if (new_col < old_col) {
	  bcopy(old_list, new, new_col * sizeof(TabListElem));
     }
     else {
	  clear_tab_list(new, new_col);
	  bcopy(old_list, new, old_col * sizeof(TabListElem));
	  for (i = 0; i < new_col; i += step)
		  if (i >= old_col)
			  *(new + i) = True;
     }

     XtFree(old_list);
#ifdef TRACK_MALLOC
     debug("Freeing tab array %d", --tab_list_count);
#endif
     
     return(new);
}

/*
 * Creates a char_array of the specified size and clears it.
 *
 * If either Dimension is 0, set it to 1.
 */
Export CharArray
create_char_array(int lin, int col)
{
     int i,len;
     CharArray array;

     if (lin == 0) {
	  lin = 1;
     }
     if (col == 0) {
	  col = 1;
     }
     
     /* first create the Line Array */
     
     if ((array = (LinePtr)XtMalloc(lin * sizeof(Line))) == NULL)
	  fatal("couldn't allocate Line Pointers for char_array");
     len = col * sizeof(Char);
#ifdef TRACK_MALLOC
     debug("Allocated %d bytes for char array %d", lin * sizeof(Line),
	   char_array_count++);
#endif
     /* next create the lines */
     for (i = 0; i < lin; i++) {
	  if ((array[i] = (Line)XtMalloc(len)) == NULL)
	       fatal("couldn't allocate Line %d for char_array",i);
	  bzero(array[i], len);
#ifdef TRACK_MALLOC
	  debug("Allocated %d bytes for line %d", len, line_array_count++);
#endif
     }
     
     return(array);
}

/*
 * Frees a char_array of the given size, can also be used to free
 * the save_array, since it checks if lines are allocated.
 */
Export void
free_char_array(CharArray array, int lines)
{
     int i;
     LinePtr ptr;
     
     /* first free all lines */
     for (ptr = array, i = 0; i < lines; i++, ptr++)
	  if (*ptr != NULL)
	       XtFree((caddr_t)*ptr);

     /* now free the array itself */
     XtFree((caddr_t)array);
}

/*
 * Creates a new CharArray with the specified size and copies the old
 * data into it. The old array gets erased.
 * Extra space will be added to the right and the bottom, it will
 * be initialized with zeroes.
 * Lines that have to be deleted get cut off below the cursor and then, if this
 * is not enough, on the top.
 * Columns are cut off at the right side.
 * Characters on the right will be lost, while top lines will be saved
 * in the save_area. This assumes, that the save_area has already been
 * resized.
 */
Export CharArray
resize_char_array(TermCanvasWidget w, CharArray old_array, int old_lin,
		  int old_col, int new_lin, int new_col)
{
     CharArray new;
     int i, len, dels, pos_dels;
     LinePtr from, to;
     
     new = create_char_array(new_lin, new_col);
     len = (new_col < old_col ? new_col : old_col) * sizeof(Char);
     
     if (new_lin < old_lin) {
	  dels = old_lin - new_lin;
	  pos_dels = old_lin - w->term.array_cur.lin - 1;
	  
	  /* First delete lines under the cursor, don't save them */
	  for (from = old_array + (old_lin - 1);
	       (dels > 0) && (pos_dels > 0);
	       dels--, pos_dels--) {
	       XtFree((caddr_t)*from--);
#ifdef TRACK_MALLOC
	       debug("Freeing line %d", --line_array_count);
#endif
	  }

	  /* If there are any left over, delete them at the top */
	  if (dels > 0) {
#ifdef DOUBLE_FONTS
	       save_array_lines(w, old_array, w->term.line_flags,
				(int)(old_lin - new_lin));
#else
	       save_array_lines(w, old_array, (int)(old_lin - new_lin));
#endif
	       from = old_array + dels;
	  }
	  else {
	       from = old_array;
	  }
     }
     else
	  from = old_array;
     to = new;

     /* copy the lines and free the old ones */
     for (i = old_lin < new_lin ? old_lin : new_lin; i; i--) {
	  bcopy(*from, *to++, len);
	  XtFree((caddr_t)*from++);
#ifdef TRACK_MALLOC
	  debug("Freeing line %d", --line_array_count);
#endif
     }

     /* free the old array */
     XtFree((caddr_t)old_array);
#ifdef TRACK_MALLOC
     debug("Freeing char array %d", --char_array_count);
#endif
     return((CharArray)new);
}

/*
 * Scrolls the area starting at start_line, which is "lines" long and
 * "scrolled_lines" up or down. Empty lines are inserted at the top or
 * bottom of the area.
 * Depending on saveflag the lines that scroll off are either saved via
 * 'save_array_lines' or reused for the new line.
 * 
 * If DOUBLE_FONTS are enabled, they get handled properly
 */

/* saveflag = whether to save the deleted lines in the savearea */
Export void
scroll_array(TermCanvasWidget w, int start_line, int lines, int scroll_lines,
	     Boolean saveflag)
{
     int i, len;
     int abs_lines;
     LinePtr clear_start;
     LinePtr tmp, from, to;
#ifdef DOUBLE_FONTS
     LineFlags clear_start_lflags, ltmp, lfrom, lto;
#endif 
     
     abs_lines = abs(scroll_lines);
     /* number of bytes in a line */
     len = w->term.columns * sizeof(Char);
     
#ifdef DOUBLE_FONTS 
     if (scroll_lines < 0) {
	  /* scroll down */
	  clear_start = w->term.char_array
	       + (start_line + lines + scroll_lines);
	  clear_start_lflags = w->term.line_flags +
	       (start_line + lines + scroll_lines);
     } else {
	  /* scroll up */
	  clear_start = w->term.char_array + start_line;
	  clear_start_lflags = w->term.line_flags + start_line;
     }
     if (saveflag)
	  save_array_lines(w, clear_start, clear_start_lflags, abs_lines);
     else 
	  for (i = abs_lines, tmp = clear_start; i; i--) {
	       bzero (*tmp++, len);
	  }
     
#else 
     if (scroll_lines < 0)
	  /* scroll down */
	  clear_start = w->term.char_array
	       + (start_line + lines + scroll_lines);
     else
	  /* scroll up */
	  clear_start = w->term.char_array + start_line;
     if (saveflag)
	  save_array_lines(w, clear_start, abs_lines);
     else
	  for (i = abs_lines, tmp = clear_start; i; i--, tmp++)
	       bzero(*tmp,len);
#endif /* DOUBLE_FONTS */
     
     /* save the cleared area */
#ifdef HAVE_ALLOCA
     tmp = (Line *)alloca(abs_lines * sizeof(Line));
#else
     tmp = (Line *)XtMalloc(abs_lines * sizeof(Line));
#ifdef TRACK_MALLOC
     debug("Allocated %d bytes for saved area %d", abs_lines * sizeof(Line),
	   ++saved_area_count);
#endif /* TRACK_MALLOC */
#endif /* HAVE_ALLOCA */
     
     for (i = abs_lines, from = clear_start, to = tmp; i; i--)
	  *(to++) = *(from++);

     
     /* move the lines */
#ifdef DOUBLE_FONTS 
     if (scroll_lines < 0) {
	  /* scroll down */
	  for (i = lines - abs_lines,
	       from = clear_start - 1, to = from + abs_lines,
	       lfrom = clear_start_lflags - 1, lto = lfrom + abs_lines;
	       i; i--) {
	       *(to--) = *(from--);
	       *(lto--) = *(lfrom--);
	  }
	  
	  /* put back the cleared area  and clear the lflags */
	  for (i = abs_lines, from = tmp,
	       to = w->term.char_array + start_line,
	       lto = w->term.line_flags + start_line; i; i--) {
	       *(to++) = *(from++);
	       *(lto++) = 0;
	  }
     } else {
	  /* scroll up */
	  for (i = lines - abs_lines,
	       from = clear_start + abs_lines, to = clear_start,
	       lfrom = clear_start_lflags + abs_lines,
	       lto = clear_start_lflags; i; i--) {
	       *(to++) = *(from++);
	       *(lto++) = *(lfrom++);
	  }
	  
	  /*
	   * put back the cleared area and clear the lflags,
	   * to and lto are already set right
	   */
	  for (i = abs_lines, from = tmp ; i; i--) {
	       *(to++) = *(from++);
	       *(lto++) = 0;
	  }
     }
#else 
     if (scroll_lines < 0) {
	  /* scroll down */
	  for (i = lines - abs_lines,
	       from = clear_start - 1, to = from + abs_lines; i; i--)
	       *(to--) = *(from--);
	  
	  /* put back the cleared area */
	  for (i = abs_lines, from = tmp,
	       to = w->term.char_array + start_line; i; i--)
	       *(to++) = *(from++);
     } else {
	  /* scroll up */
	  for (i = lines - abs_lines,
	       from = clear_start + abs_lines, to = clear_start; i; i--)
	       *(to++) = *(from++);
	  
	  /* put back the cleared area, to is already set right */
	  for (i = abs_lines, from = tmp ; i; i--)
	       *(to++) = *(from++);
     }
#endif /* DOUBLE_FONTS */
#ifndef HAVE_ALLOCA
     XtFree((char *)tmp);
#ifdef TRACK_MALLOC
     debug("Freed saved area %d", --saved_area_count);
#endif /* TRACK_MALLOC */
#endif /* HAVE_ALLOCA */
}

/* Resizes the save_area's width */
Export void
resize_save_area(TermCanvasWidget w, int old_width, int new_width)
{
     int i, len, cplen;
     Line tmp, *ptr;
     
     /* If the width doesn't change, just get back */
     if (old_width == new_width)
	  return;

     len = new_width * sizeof(Char);
     cplen = (new_width < old_width ? new_width : old_width) * sizeof(Char);
     ptr = w->term.save_array;
     
     for (i = w->term.save_lines; i; i--) {
	  /* Is there a Line at all? */
	  if (*ptr != NULL) {
	       /* create a new line */
	       tmp = (Line)XtMalloc(len);
	       
	       /* clear it */
	       bzero(tmp, len);
	       
	       /* copy the stuff over */
	       bcopy(*ptr, tmp, cplen);

	       /* free the old one */
	       XtFree((caddr_t)*ptr);

	       *ptr = tmp;
	  }
	  ptr++;
     }
}

/* Appends lines to the save_area and creates new ones in the old place */
Export void
#ifdef DOUBLE_FONTS
save_array_lines(TermCanvasWidget w, LinePtr start, LineFlags lstart,
		 int lines)
#else 
save_array_lines(TermCanvasWidget w, LinePtr start, int lines)
#endif
{
     int i;
     LinePtr from, to, tmp;
#ifdef DOUBLE_FONTS
     LineFlags lfrom, lto;
#endif 
     int len = w->term.columns * sizeof(Char);

     /* Save the top of the save_array and create new lines if necessary */
#ifdef HAVE_ALLOCA
     tmp = (Line *)alloca(lines * sizeof(Line));
#else
     tmp = (Line *)XtMalloc(lines * sizeof(Line));
#ifdef TRACK_MALLOC
     debug("Allocated %d bytes for saved area %d", lines * sizeof(Line),
	   ++saved_area_count);
#endif /* TRACK_MALLOC */
#endif /* HAVE_ALLOCA */
     for (from = w->term.save_array, to = tmp, i = lines; i; i--) {
	  if (*from != NULL)
	       *to = *from;
	  else {
	       *to = (Line)XtMalloc(len);
#ifdef TRACK_MALLOC
	       debug("Allocated %d bytes for saved line %d", len,
		     ++saved_line_count);
#endif
	  }
	  bzero(*(to++), len);
	  from++;
     }
     
     /* Scroll the save_array up, from is already at the right position */
     to = w->term.save_array;
#ifdef DOUBLE_FONTS
     lfrom = w->term.s_line_flags + lines;
     lto = w->term.s_line_flags;

     for (i = w->term.save_lines - lines; i; i--) {
	  *(to++) = *(from++);
	  *(lto++) = *(lfrom++);
     }

     /* Now copy the lines to save into the save_array, to is already ok */
     from = start;
     lfrom = lstart;
     for (i = lines; i; i--) {
	  *(to++) = *(from++);
	  *(lto++) = *(lfrom++);
     }
#else 
     for (i = w->term.save_lines - lines; i; i--)
	  *(to++) = *(from++);

     /* Now copy the lines to save into the save_array, to is already ok */
     from = start;
     for (i = lines; i; i--)
	  *(to++) = *(from++);
#endif /* DOUBLE_FONTS */

     /* Now copy the lines to save into the save_array, to is already ok */
     from = start;
     for (i = lines; i; i--)
	  *(to++) = *(from++);

     /* put the saved or new lines in the old place */
     for (i = lines, to = start, from = tmp; i; i--)
	  *(to++) = *(from++);
     
     /* Adjust the save_size */
     if (w->term.save_size < w->term.save_lines) {
	  w->term.save_size += lines;
	  if (w->term.save_size > w->term.save_lines)
	       w->term.save_size = w->term.save_lines;
	  /*
	   * Tell the Parent to adjust the scrollbar
	   */
	  if (w->term.adjust_scroll_bar != NULL) {
	       double spos, ssize;
	       spos = (double)(w->term.save_size - w->term.scroll_pos) /
		    (double)(w->term.lines + w->term.save_size);
	       ssize = (double)w->term.lines /
		    (double)(w->term.lines + w->term.save_size);
/* 	       printf("pos = %f, size = %f\n", spos, ssize); */
	       w->term.adjust_scroll_bar(XtParent(w), spos, ssize);
	  }
     }
}

/*
 * Deletes characters in the given line starting with the given column.
 * Characters to the right automatically get shifted left.
 */
Export void
delete_array_chars(TermCanvasWidget w, int line, int start_col, int no_chars,
		   int line_len)
{
     CharPtr from, to;
     int i;
     
     /* clip to the right border of the line */
     if ((start_col + no_chars) > line_len)
	  no_chars = line_len - start_col;
     
     /* set up copying pointers */
     to = w->term.char_array[line] + start_col;
     from = to + no_chars;
     
     /* copy the chars */
     for (i = line_len - start_col - no_chars; i; i--)
	  *to++ = *from++;

     /* clear the rest of the line */
     for (i = no_chars ; i; i--) {
	  to->value = 0;
	  to->attributes = 0;
	  to++;
     }
}

/*
 * Erases characters in the given line starting with the given column.
 * Characters to the right stay at their place
 */
Export void
erase_array_chars(TermCanvasWidget w, int line, int start_col,
		  int no_chars, int line_len)
{
     CharPtr ptr;
     int i;
     
     /* clip to the right border of the line */
     if ((start_col + no_chars) > line_len)
	  no_chars = line_len - start_col;
     
     /* set up pointer */
     ptr = w->term.char_array[line] + start_col;
     
     /* clear the chars */
     for (i = no_chars; i; i--) {
	  ptr->value = 0;
	  ptr->attributes = 0;
	  ptr++;
     }
}

/*
 * Clears a number of lines in the array (e.g. for a clear screen operation)
 */
Export void
erase_array_lines(TermCanvasWidget w, int start, int no)
{
#ifdef DOUBLE_FONTS
     LinePtr linptr = w->term.char_array + start;
     LineFlags lfptr = w->term.line_flags + start;
     int len = w->term.columns * sizeof(Char);
     
     while (no--) {
	  bzero(*linptr++, len);
	  *lfptr++ = 0;
     }
#else 
     LinePtr linptr = w->term.char_array + start;
     int len = w->term.columns * sizeof(Char);
     
     while (no--)
	  bzero(*linptr++, len);
#endif /* DOUBLE_FONTS */
}

/*
 * Puts the given string into the array with the given attribute.
 */
Export void
put_string_in_array(TermCanvasWidget w, char *buffer, char cnt, int lin,
		    int col, unsigned char attrib, unsigned char color)
{
     CharPtr to = w->term.char_array[lin] + col;

     for (; cnt; cnt--) {
	  to->value = *buffer++;
	  to->attributes = attrib;
	  to->color = color;
	  to++;
     }
}

/*
 * Shift the given line starting at 'col' 'cnt' positions to the right.
 * The created space is NOT cleared.
 */
/*ARGSUSED*/
Export void
shift_array_line(TermCanvasWidget w, Line line, int col, int cnt, int cols)
{
     if ((col + cnt) >= cols)
	  return;

     bcopy(line + col, line + col + cnt, (cols - col - cnt) * sizeof(Char));
}
