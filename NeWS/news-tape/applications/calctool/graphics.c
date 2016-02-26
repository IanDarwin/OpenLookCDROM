
/*  graphics.c
 *
 *  These are the independent graphics routines used by calctool.
 *
 *  Copyright (c) Rich Burridge - May 1988.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Version 2.2.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

#include "calctool.h"
#include "color.h"
#include "extern.h"


but_text(row,column,portion,state)
int row,column,portion ;
enum but_state state ;
{
  enum font_type butfont ;
  int i,n ;

  n = row*BCOLS*2 + column*2 + portion ;
  get_label(n) ;
  for (spaces = 0, i = 0; i < strlen(pstr); i++)
    if (pstr[i] == ' ') spaces++ ;
  if (buttons[n].color != GREY)
    {
      x = chxoff[spaces] ;
      y = (n & 1) ? 40: 18 ;
      if (spaces == 3)  y += 4 ;
      butfont = (spaces == 3) ? BFONT : NFONT ;
      if (state == NORMAL)
        color = (!iscolor & portion) ? WHITE : BLACK ;
      if (state == INVERTED)
        color = (portion) ? BLACK : WHITE ;
      text(column*(BWIDTH+BGAP)+BBORDER+x,
           row*(BHEIGHT+BGAP)+BBORDER+y,KEYCANVAS,butfont,color,pstr) ;
    }
}


draw_button(row,column,portion,state)
int row,column,portion ;
enum but_state state ;
{
  int n ;

  n = row*BCOLS*2 + column*2 + portion ;
  if (!portion)
    {
      color = (iscolor) ? buttons[n].color : WHITE ;
      drawbox(column*(BWIDTH+BGAP)+BBORDER,
              row*(BHEIGHT+BGAP)+BBORDER,BWIDTH,BHEIGHT) ;
      fillbox(column*(BWIDTH+BGAP)+BBORDER+1,
              row*(BHEIGHT+BGAP)+BBORDER+1,KEYCANVAS,42,50,1,color) ;
    }
  else
    { 
      drawbox(column*(BWIDTH+BGAP)+BBORDER+5,
              row*(BHEIGHT+BGAP)+BBORDER+26,34,21) ;
      color = (iscolor) ? buttons[n].color : BLACK ;
      fillbox(column*(BWIDTH+BGAP)+BBORDER+6,
              row*(BHEIGHT+BGAP)+BBORDER+27,KEYCANVAS,32,19,1,color) ;
    }
  but_text(row,column,portion,state) ;
}


handle_down_event(type)
int type ;
{
  int n ;
 
  getxy(&x,&y) ;
  if (!down)
    {
      column = (x - BBORDER) / (BWIDTH + BGAP) ;
      row = (y - BGAP) / (BHEIGHT + BGAP) ;
      portion = (y - BBORDER - (row * (BHEIGHT + BGAP))) / (BHEIGHT / 2) ;
      n = row*BCOLS*2 + column*2 + portion ;
      if (pending_op != '?' && n <= (NOBUTTONS*2))
        inv_but(row,column,portion,INVERTED) ;
      down = type ;
    }
}


inv_but(row,column,portion,state)
int row,column,portion ;
enum but_state state ;
{
  int n ;
 
  n = row*BCOLS*2 + column*2 + portion ;
  if (pending_op != '?')
    {
      if (state == NORMAL)
        if (iscolor) color = buttons[n].color ;
        else color = (portion) ? BLACK : WHITE ;
      if (state == INVERTED)
        color = (portion) ? WHITE : BLACK ;
      fillbox(column*(BWIDTH+BGAP)+BBORDER+6,
              row*(BHEIGHT+BGAP)+BBORDER+5+(portion*22),
              KEYCANVAS,32,19,portion,color) ;
      but_text(row,column,portion,state) ;
    }
}


make_canvas(toggle)
int toggle ;
{
  if (toggle) tstate = !tstate ;
  color = (iscolor) ? GREY : WHITE ;
  clear_canvas(KEYCANVAS,color) ;
  for (row = 0; row < BROWS; row++)
    for (column = 0; column < BCOLS; column++)
      for (portion = 0; portion < 2; portion++)
        draw_button(row,column,portion,NORMAL) ;

  if (rstate) draw_regs() ;
}


make_registers()           /* Calculate memory register frame values. */

{
  char line[MAXLINE] ;     /* Current memory register line. */
  int n ;

  clear_canvas(REGCANVAS,WHITE) ;
  text(15,20,REGCANVAS,NFONT,BLACK,"Memory Registers") ;
  for (n = 0; n < MAXREGS; n++)
    {
      SPRINTF(line,"%1d   ",n) ;
      STRCAT(line,make_number(mem_vals[n])) ;
      text(15,40+15*n,REGCANVAS,NFONT,BLACK,line) ;
    }
}
