
/*  news.c
 *
 *  These are the NeWS dependent graphics routines used by calctool.
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
#include <sys/types.h>

/* Various event used by the NeWS interface. */
#define  CFRAME_REPAINT_EVENT  100    /* Main frame needs repainting. */
#define  RFRAME_REPAINT_EVENT  101    /* Register frame needs repainting. */
#define  LEFT_DOWN_EVENT       102    /* Left mouse button was depressed. */
#define  LEFT_UP_EVENT         103    /* Left mouse button was debounced. */
#define  MIDDLE_DOWN_EVENT     104    /* Middle mouse button was depressed. */
#define  MIDDLE_UP_EVENT       105    /* Middle mouse button was debounced. */
#define  KEYBOARD_EVENT        106    /* Keyboard character was pressed. */

char old_str[4][MAXLINE] ;    /* Previous contents of calculator text items. */

extern FILE *PostScript ;
extern FILE *PostScriptInput ;


clear_canvas(canvas,color)
enum can_type canvas ;
int color ;
{
  char c ;

       if (canvas == KEYCANVAS) c = 'K' ;
  else if (canvas == REGCANVAS) c = 'R' ;
  FPRINTF(PostScript,"%d %cC PSClearCanvas\n",color,c) ;
}


close_frame()
{
  FPRINTF(PostScript,"PSCloseFrame\n") ;
}


destroy_frame()
{
  exit(0) ;
}


drawbox(x,y,width,height)
int x,y,width,height ;
{
  FPRINTF(PostScript,"%d %d %d %d PSDrawBox\n",x,y,width,height) ;
}


draw_regs()
{
  FPRINTF(PostScript,"PSDrawRegs\n") ;
}


fillbox(x,y,canvas,width,height,boundry,color)
enum can_type canvas ;
int x,y,width,height,boundry,color ;
{
  char c ;

       if (canvas == KEYCANVAS) c = 'K' ;
  else if (canvas == REGCANVAS) c = 'R' ;
  FPRINTF(PostScript,"%d %d %cC %cCHeight %d %d %d %d PSFillBox\n",
          x,y,c,c,width,height,boundry,color) ;
}


getxy(x,y)
int *x,*y ;
{
  pscanf(PostScriptInput,"%d%d",x,y) ;
}


handle_key_event()
{
  int i,n ;

  pscanf(PostScriptInput,"%d%d%d",&n,&x,&y) ;
  for (i = 0; i < TITEMS; i++)
    if (n == buttons[i].value) return(i) ;
  return(-1) ;
}


handle_up_event(type)
int type ;
{
  int n ;

  pscanf(PostScriptInput,"%d%d",&x,&y) ;
  if ((type == LEFT_UP_EVENT   && down == LEFT_DOWN_EVENT) ||
      (type == MIDDLE_UP_EVENT && down == MIDDLE_DOWN_EVENT))
    {
      n = row*BCOLS*2 + column*2 + portion ;
      if (pending_op != '?' && n <= (NOBUTTONS*2))
        inv_but(row,column,portion,NORMAL) ;
      down = 0 ;
      return(n) ;
    }
  return(-1) ;
}


init_fonts()
{
  FPRINTF(PostScript,"PSInitFonts\n") ;
}


init_ws_type()
{
  if (ps_open_PostScript() < 0) return -1 ;
  if (send_ps_file(NEWSFILE) == -1)
    {
      FCLOSE(PostScript) ;
      return(-1) ;
    }
  FFLUSH(PostScript) ;
  if (ferror(PostScript))
    {
      FCLOSE(PostScript) ;
      return(-1) ;
    }
  FPRINTF(PostScript,"PSIsColor\n") ;
  pscanf(PostScriptInput,"%d",&iscolor) ;
  FPRINTF(PostScript,"PSInitialise\n") ;
  return(0) ;
}


load_colors()      /* Create and load calctool color map. */
{
  u_char red[CALC_COLORSIZE], green[CALC_COLORSIZE], blue[CALC_COLORSIZE] ;
  int i ;

  calc_colorsetup(red,green,blue) ;
  FPRINTF(PostScript,"%d PSMakeColorTable\n",CALC_COLORSIZE) ;
  for (i = 0; i < CALC_COLORSIZE; i++)
    FPRINTF(PostScript,"%d %d %d %d PSLoadColor\n",
                       red[i],green[i],blue[i],i) ;
}


/*ARGSUSED*/
make_frames(argc,argv)
int argc ;
char *argv[] ;
{
  FPRINTF(PostScript,"%d %d %d %d PSMakeFrames\n",
                     180,10,TWIDTH,DISPLAY+THEIGHT+15) ;
}


make_icon()
{
  FPRINTF(PostScript,"PSMakeIcons\n") ;
}


make_items()
{
  int i ;

  for (i = (int) BASEITEM; i < (int) TTYPEITEM; i++)
    STRCPY(old_str[i],"") ;
  set_item(BASEITEM,"") ;        /* base. */
  set_item(DISPLAYITEM,"") ;     /* display. */
  set_item(OPITEM,"") ;          /* op. */
  set_item(TTYPEITEM,"") ;       /* ttype. */
}


make_subframes()
{
  FPRINTF(PostScript,"%d PSMakeSubframes\n",DISPLAY) ;
}


send_ps_file(fname)
char *fname ;
{
  FILE *stream ;
  int c ;
 
  if ((stream = fopen(fname,"r")) == NULL) return -1 ;
  while ((c = getc(stream)) != EOF) putc(c,PostScript) ;
  FCLOSE(stream) ;
  return 0 ;
}


set_cursor(type)
int type ;
{
  FPRINTF(PostScript,"%d PSSetCursor\n",type) ;
}


set_item(itemno,str)
enum item_type itemno ;
char *str ;
{
  enum font_type fontno ;
  int x,y ;

  if (itemno == DISPLAYITEM)
    {
      fontno = NFONT ;
      y = DISPLAY-20 ;
    }
  else
    {
      fontno = SFONT ;
      y = DISPLAY-10 ;
    }

       if (itemno == BASEITEM) x = BBORDER ;
  else if (itemno == DISPLAYITEM)
    x = 5+(MAX_DIGITS - strlen(old_str[(int) DISPLAYITEM]))*9 ;
  else if (itemno == OPITEM) x = BBORDER+2*(BWIDTH+BGAP) ;
  else if (itemno == TTYPEITEM) x = BBORDER+(BWIDTH+BGAP) ;
  text(x,y,PANELCANVAS,fontno,WHITE,old_str[(int) itemno]) ;

  if (itemno == DISPLAYITEM)
    x = 5+(MAX_DIGITS - strlen(str))*9 ;
  text(x,y,PANELCANVAS,fontno,BLACK,str) ;
  STRCPY(old_str[(int) itemno],str) ;
}


start_tool()
{
  int n,type ;

  while (1)
    {
      FFLUSH(PostScript) ;
      if (pscanf(PostScriptInput,"%d",&type) == EOF) destroy_frame() ;
      switch (type)
        {
          case CFRAME_REPAINT_EVENT : make_canvas(0) ;
                                      set_item(BASEITEM,base_str[(int) base]) ;
                                      set_item(TTYPEITEM,ttype_str[(int) ttype]) ;
                                      break ;
          case RFRAME_REPAINT_EVENT : make_registers() ;
                                      break ;
          case LEFT_DOWN_EVENT      :
          case MIDDLE_DOWN_EVENT    : handle_down_event(type) ;
                                      break ;
          case LEFT_UP_EVENT        :
          case MIDDLE_UP_EVENT      : n = handle_up_event(type) ;
                                      if (n >= 0 && n <= (NOBUTTONS*2))
                                        process_item(n) ;
                                      break ;
          case KEYBOARD_EVENT       : n = handle_key_event() ;
                                      if (n >= 0 && n <= TITEMS)
                                        process_item(n) ;
                                      break ;
          default                   : ;
        }
    }
}


text(x,y,canvas,fontno,color,str)
enum can_type canvas ;
enum font_type fontno ;
int x,y,color ;
char *str ;
{
  int i ;
  char font,fonttype[6],line[MAXLINE] ;

       if (fontno == SFONT) STRCPY(fonttype,"SFont") ;
  else if (fontno == NFONT) STRCPY(fonttype,"NFont") ;
  else if (fontno == BFONT) STRCPY(fonttype,"BFont") ;
       if (canvas == KEYCANVAS) font = 'K' ;
  else if (canvas == REGCANVAS) font = 'R' ;
  else if (canvas == PANELCANVAS) font = 'P' ;
  line[0] = '\0' ;
  for (i = 0; i < strlen(str); i++)
    switch (str[i])
      {
        case '\\' : STRCAT(line,"\\\\") ;
                    break ;
        case '('  : STRCAT(line,"\\(") ;
                    break ;
        case ')'  : STRCAT(line,"\\)") ;
                    break ;
        default   : STRNCAT(line,&str[i],1) ;
      }
  FPRINTF(PostScript,"%cC %cCHeight %s ",font,font,fonttype) ;
  FPRINTF(PostScript,"%d %d %d (%s) PSMakeText\n",x,y,color,line) ;
}


toggle_reg_canvas()

{
  rstate = !rstate ;
  FPRINTF(PostScript,"%d PSToggleRegCanvas\n",rstate) ;
}
