
/*  calctool.c
 *
 *  A calculator program for use with the NeWS and SunView graphics packages.
 *
 *  Copyright (c) Rich Burridge - May 1988.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Basic algorithms by Ed Falk, Sun Microsystems, Mountain View.
 *
 *  Version 2.2.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

#include "patchlevel.h"
#include "color.h"
#include "calctool.h"

double powers[11][4] = {
         {    1.0,          1.0,           1.0,             1.0 },
         {    2.0,          8.0,          10.0,            16.0 },
         {    4.0,         64.0,         100.0,           256.0 },
         {    8.0,        512.0,        1000.0,          4096.0 },
         {   16.0,       4096.0,       10000.0,         65536.0 },
         {   32.0,      32768.0,      100000.0,       1048576.0 },
         {   64.0,     262144.0,     1000000.0,      16777216.0 },
         {  128.0,    2097152.0,    10000000.0,     268435456.0 },
         {  256.0,   16777216.0,   100000000.0,    4294967296.0 },
         {  512.0,  134217728.0,  1000000000.0,   68719476736.0 },
         { 1024.0, 1073741824.0, 10000000000.0, 1099511627776.0 }
} ;

char base_str[4][4] = { "BIN", "OCT", "DEC", "HEX" } ;
char ttype_str[3][5] = { "DEG", "GRAD", "RAD" } ;

char digits[] = "0123456789abcdef" ;
int basevals[4] = {2, 8, 10, 16} ;

/* Length of display in characters for each base. */
int disp_length[4] = {32, 15, 12, 12} ;

/* X offset in pixels for various length button strings. */
int chxoff[4] = { 5, 9, 14, 16 } ;

/* Valid keys when an error condition has occured. */
/*                            MEM  KEYS HELP   clr   QUIT OFF  */
char validkeys[MAXVALID]  = { 'M', 'K', '?', '\177', 'q', 'o' } ;

double disp_val ;             /* Value of the current display. */
double last_input ;           /* Previous number input by user. */
double mem_vals[MAXREGS] ;    /* Memory register values. */
double result ;               /* Current calculator total value. */
double tresults[3] ;          /* Current trigonometric results. */

enum base_type base ;         /* Current base: BIN, OCT, DEC or HEX. */
enum trig_type ttype ;        /* Trigonometric type (deg, grad or rad). */

FILE *hfd ;         /* File descriptor for help information. */

int accuracy ;      /* Number of digits precision (Max 9). */
int color ;         /* Color being used for current raster operation. */
int column ;        /* Column number of current key/mouse press. */
int down ;          /* Indicates is a mouse button is down. */
int error ;         /* Indicates some kind of display error. */
int iscolor ;       /* Set if this is a color screen. */
int ishelp ;        /* Set if there is a help file. */
int issel ;         /* Set if valid [Get] selection. */
int new_input ;     /* New number input since last op. */
int pending ;       /* Indicates command depending on multiple presses. */
int pending_op ;    /* Arithmetic operation for pending command. */
int pointed ;       /* Whether a decimal point has been given. */
int portion ;       /* Portion of button on current key/mouse press. */
int row ;           /* Row number of current key/mouse press. */
int rstate ;        /* Indicates if memory register frame is displayed. */
int spaces ;        /* Number of spaces in current button string. */
int toclear ;       /* Indicates if display should be cleared. */
int tstate ;        /* Indicates current button set being displayed. */
int x ;             /* X offset for text for button. */
int y ;             /* Y offset for text for button. */

/* Routines obeyed by mouse button or character presses. */
int close_frame(),    destroy_frame(),  do_atrig(),     do_base() ;
int do_calculation(), do_clear(),       do_constant(),  do_delete() ;
int do_immediate(),   do_keys(),        do_nothing(),   do_number() ;
int do_pending(),     do_point(),       do_portion(),   do_registers() ;
int do_pending(),     do_trig(),        do_trigtype(),  toggle_reg_canvas() ;

char cur_op ;            /* Current arithmetic operation. */
char current ;           /* Current button or character pressed. */
char old_cal_value ;     /* Previous calculation operator. */
char pstr[5] ;           /* Current button text string. */
char *selection ;        /* Current [Get] selection. */

struct button buttons[TITEMS] = {              /* Calculator button values. */
  { ">   ", '>',    OP_SET,   LGREY,    do_pending },       /* Row 1. */
  { "<   ", '<',    OP_SET,   LGREY,    do_pending },
  { "BIN ", 'B',    OP_CLEAR, YELLOW,   do_base },
  { "MEM ", 'M',    OP_CLEAR, BLUE,     toggle_reg_canvas },
  { "OCT ", 'O',    OP_CLEAR, YELLOW,   do_base },
  { "D   ", 'd',    OP_NOP,   PINK,     do_number },
  { "DEC ", 'D',    OP_CLEAR, YELLOW,   do_base },
  { "E   ", 'e',    OP_NOP,   PINK,     do_number }, 
  { "HEX ", 'H',    OP_CLEAR, YELLOW,   do_base },
  { "F   ", 'f',    OP_NOP,   PINK,     do_number },  
  { "KEYS", 'K',    OP_CLEAR, BLUE,     do_keys },
  { "?   ", '?',    OP_SET,   BLUE,     do_pending },  

  { "&32 ", 'i',    OP_CLEAR, LGREY,    do_immediate },      /* Row 2. */
  { "STO ", 's',    OP_SET,   MAUVE,    do_pending },
  { "&16 ", 'h',    OP_CLEAR, LGREY,    do_immediate },
  { "RCL ", 'r',    OP_SET,   MAUVE,    do_pending }, 
  { "PI  ", 'P',    OP_CLEAR, ORANGE,   do_constant },
  { "A   ", 'a',    OP_NOP,   PINK,     do_number }, 
  { "e   ", 'E',    OP_CLEAR, ORANGE,   do_constant },
  { "B   ", 'b',    OP_NOP,   PINK,     do_number },
  { "%   ", '%',    OP_SET,   LPURPLE,  do_calculation },
  { "C   ", 'c',    OP_NOP,   PINK,     do_number },
  { "clr ", '\177', OP_CLEAR, BLUE,     do_clear },
  { "bsp ", '\010', OP_NOP,   BLUE,     do_delete },

  { "OR  ", '|',    OP_SET,   GREEN,    do_calculation },   /* Row 3. */
  { "AND ", '&',    OP_SET,   GREEN,    do_calculation },
  { "ASIN", '}',    OP_CLEAR, LGREEN,   do_atrig },
  { "SIN ", '{',    OP_CLEAR, LGREEN,   do_trig },
  { "e^x ", '#',    OP_CLEAR, ORANGE,   do_immediate },
  { "7   ", '7',    OP_NOP,   LBLUE,    do_number },
  { "10^x", '$',    OP_CLEAR, ORANGE,   do_immediate },
  { "8   ", '8',    OP_NOP,   LBLUE,    do_number },
  { "y^x ", 'Y',    OP_SET,   ORANGE,   do_calculation },
  { "9   ", '9',    OP_NOP,   LBLUE,    do_number }, 
  { "INT ", 'I',    OP_CLEAR, LGREY,    do_portion },
  { "X   ", 'x',    OP_SET,   LPURPLE,  do_calculation },

  { "XNOR", 'n',    OP_SET,   GREEN,    do_calculation },   /* Row 4. */
  { "XOR ", '^',    OP_SET,   GREEN,    do_calculation },
  { "ACOS", ')',    OP_CLEAR, LGREEN,   do_atrig },
  { "COS ", '(',    OP_CLEAR, LGREEN,   do_trig },
  { "ln  ", 'N',    OP_CLEAR, ORANGE,   do_immediate },
  { "4   ", '4',    OP_NOP,   LBLUE,    do_number },
  { "log ", 'G',    OP_CLEAR, ORANGE,   do_immediate },
  { "5   ", '5',    OP_NOP,   LBLUE,    do_number },
  { "SQRT", 'S',    OP_CLEAR, ORANGE,   do_immediate },
  { "6   ", '6',    OP_NOP,   LBLUE,    do_number },
  { "FRAC", 'F',    OP_CLEAR, LGREY,    do_portion },
  { "/   ", '/',    OP_SET,   LPURPLE,  do_calculation },

  { "NOT ", '~',    OP_CLEAR, GREEN,    do_immediate },     /* Row 5. */
  { "ACC ", 'A',    OP_SET,   BLUE,     do_pending },
  { "ATAN", 'T',    OP_CLEAR, LGREEN,   do_atrig },
  { "TAN ", 't',    OP_CLEAR, LGREEN,   do_trig },
  { "1/x ", 'R',    OP_CLEAR, ORANGE,   do_immediate },
  { "1   ", '1',    OP_NOP,   LBLUE,    do_number },
  { "x!  ", '!',    OP_CLEAR, ORANGE,   do_immediate },
  { "2   ", '2',    OP_NOP,   LBLUE,    do_number },
  { "x^2 ", '@',    OP_CLEAR, ORANGE,   do_immediate },
  { "3   ", '3',    OP_NOP,   LBLUE,    do_number },
  { "CHS ", 'C',    OP_CLEAR, LGREY,    do_immediate },
  { "-   ", '-',    OP_SET,   LPURPLE,  do_calculation },

  { "QUIT", 'q',    OP_CLEAR, BLUE,     destroy_frame },    /* Row 6. */
  { "OFF ", 'o',    OP_CLEAR, BLUE,     close_frame },
  { "    ", ' ',    OP_CLEAR, GREY,     do_nothing },
  { "    ", ' ',    OP_CLEAR, GREY,     do_nothing },
  { "DEG ", '\004', OP_CLEAR, RED,      do_trigtype },
  { "0   ", '0',    OP_NOP,   LBLUE,    do_number },
  { "RAD ", '\022', OP_CLEAR, RED,      do_trigtype },
  { ".   ", '.',    OP_NOP,   LPURPLE,  do_point },
  { "GRAD", '\007', OP_CLEAR, RED,      do_trigtype },
  { "=   ", '=',    OP_CLEAR, LPURPLE,  do_calculation },
  { "ABS ", 'U',    OP_CLEAR, LGREY,    do_portion },
  { "+   ", '+',    OP_SET,   LPURPLE,  do_calculation },

  { "    ", 'X',    OP_NOP,   WHITE,    do_calculation },   /* Extra definitions. */
  { "    ", '*',    OP_NOP,   WHITE,    do_calculation },
  { "    ", '\015', OP_NOP,   WHITE,    do_calculation },
  { "    ", 'Q',    OP_NOP,   WHITE,    destroy_frame },
} ;

char display[MAXLINE] ;     /* Current calculator display. */
char helpname[MAXLINE] ;    /* Filename for help file. */
char progname[MAXLINE] ;    /* Name of this program. */


main(argc,argv)
int argc ;
char *argv[] ;
{
  STRCPY(progname,argv[0]) ; /* Save this programs name. */
  get_options(argc,argv) ;   /* Get command line arguments. */
  open_helpfile(helpname) ;  /* Open helpfile if present. */
  if (init_ws_type())        /* Determine window system type. */
    {
      FPRINTF(stderr,"Error initialising window system.\n") ;
      exit(1) ;
    }
  make_icon() ;              /* Set up the calctool window icon. */
  init_fonts() ;             /* Open required fonts. */
  make_frames(argc,argv) ;   /* Create calctool window frames. */
  make_subframes() ;         /* Create panels and canvases. */
  load_colors() ;            /* Load the initial calctool colormap. */
  make_items() ;             /* Create panel items and cursors. */

  pending = 0 ;              /* No initial pending command. */
  rstate = 0 ;               /* No memory register frame display initially. */
  tstate = 0 ;               /* Button values displayed first. */
  base = DEC ;               /* Initial base. */
  ttype = DEG ;              /* Initial trigonometric type. */
  down = 0 ;                 /* No mouse presses initially. */
  accuracy = 2 ;             /* Initial accuracy. */
  do_clear() ;               /* Initialise and clear display. */
  start_tool() ;             /* Display the calculator. */
  exit(0) ;
}


char_val(chr)
char chr ;
{
       if (chr >= '0' && chr <= '9') return(chr - '0') ;
  else if (chr >= 'a' && chr <= 'f') return(chr - 'a' + 10) ;
  else return(-1) ;
}


clear_display()
{
  int i ;

  pointed = 0 ;
  toclear = 1 ;
  STRCPY(display,"0.") ;
  for (i = 0; i < accuracy; i++) STRNCAT(display,"0",1) ;
  set_item(DISPLAYITEM,display) ;
  disp_val = 0.0 ;
}


double
convert_display()    /* Convert input string into a double. */
{
  int i,inum ;
  double val ;
  char *optr ;

  val = 0.0 ;
  optr = display ;
  while ((inum = char_val(*optr)) >= 0)
    {
      val = val * basevals[(int) base] + inum ;
      *optr++ ;
    }
      
  if (*optr == '.')
    for (i = 1; (inum = char_val(*++optr)) >= 0; i++)
      val += inum / powers[i][(int) base] ;
  return(val) ;
}
 
 
get_label(n)
int n ;
{
  if (tstate)
    switch (buttons[n].value)
        {
          case CTRL('d') : STRCPY(pstr,"^d  ") ;
                           break ;
          case CTRL('g') : STRCPY(pstr,"^g  ") ;
                           break ;
          case '\010'    : STRCPY(pstr,"bsp ") ;
                           break ;
          case CTRL('r') : STRCPY(pstr,"^r  ") ;
                           break ;
          case '\177'    : STRCPY(pstr,"del ") ;
                           break ;
          default        : SPRINTF(pstr,"%c   ",buttons[n].value) ;
        }
  else STRCPY(pstr,buttons[n].str) ;
}


get_options(argc,argv)        /* Extract command line options. */
int argc ;
char *argv[] ;
{
  STRCPY(helpname,HELPNAME) ;    /* Default help filename. */
  argv++ ;
  argc-- ;
  while (argc > 0)
    {
      switch (argv[0][1])
        {
          case 'h' : argv++ ;
                     argc-- ;
                     STRCPY(helpname,*argv) ;      /* Get new help filename. */
                     break ;
          case 'v' : FPRINTF(stderr,"%s version 2.2.%1d\n",progname,PATCHLEVEL) ;
                     break ;
          case '?' : FPRINTF(stderr,"Usage: %s [-h helpfile] [-v] [-?]\n",progname) ;
                     exit(1) ;
        }
      argc-- ;
      argv++ ;
    }
}


grey_buttons(base)     /* Grey out numeric buttons depending upon base. */
enum base_type base ;
{
  char val ;
  int column,i,n,portion,row ;

  for (i = 0; i < 16; i++)
    {
      val = digits[i] ;
      for (n = 0; n < TITEMS; n++)
        if (val == buttons[n].value) break ;
      if (i < basevals[(int) base])
        {
          if (i < 10) buttons[n].color = LBLUE ;
          else buttons[n].color = PINK ;
        }
      else buttons[n].color = GREY ;
      row = n / (BCOLS*2) ;
      column = (n - (row*BCOLS*2)) / 2 ;
      portion = n & 1 ;
      draw_button(row,column,portion,NORMAL) ;
    }
}


initialise()
{
  error = 0 ;              /* Currently no display error. */
  cur_op = '?' ;           /* No arithmetic operator defined yet. */
  old_cal_value = '?' ;
  result = 0.0 ;           /* No previous result yet. */
  last_input = 0.0 ;
}


char *
make_number(number)        /* Convert display value to current base. */
double number ;            /* Value to convert. */
{
  char *optr ;
  double val ;
  int cmax ;       /* Maximum number of characters to display. */
  int ndig ;       /* Total number of digits to generate. */
  int ddig ;       /* Number of digits to left of . */
  int dval ;

  if (isinf(number) || isnan(number))
    {
      STRCPY(display,"Error") ;
      error = 1 ;
      set_item(OPITEM,"CLR") ;
      return(display) ;
    }

  cmax = disp_length[(int) base] ;
  optr = display ;
  val = fabs(number) ;
  if (number < 0.0) *optr++ = '-' ;
  val += .5 / powers[accuracy][(int) base] ;

  if (val < 1.0)
    {
      ddig = 0 ;
      *optr++ = '0' ;
      cmax-- ;
    }
  else
    {
      for (ddig = 0; val >= 1.0; ddig++)
        val /= powers[1][(int) base] ;
    }

  if ((ndig = ddig + accuracy) > cmax)
    {
      if (ddig > cmax)
        {
          STRCPY(display,"Overflow") ;
          error = 1 ;
          set_item(OPITEM,"CLR") ;
          return(display) ;
        }
      else
        {
          STRCPY(display,"Reducing precision") ;
          set_item(DISPLAYITEM,display) ;
          sleep(1) ;
          bzero(display,MAXLINE) ;
          accuracy = cmax - ddig ;
          if (accuracy < 0) accuracy = 0 ;
          ndig = ddig + accuracy ;
        }
    }

  while (ndig-- > 0)
    {
      if (ddig-- == 0) *optr++ = '.' ;
      val *= powers[1][(int) base] ;
      dval = val ;
      *optr++ = digits[dval] ;
      val -= (int) val ;
    }
 *optr++ = '\0' ;
  toclear = 1 ;
  pointed = 0 ;
  return(display) ;
}


matherr(x)      /* Calctools' math library error-handling routine. */
struct exception *x ;
{
  SPRINTF(display,"Error in %s",x->name) ;
  set_item(DISPLAYITEM,display) ;
  error = 1 ;
  set_item(OPITEM,"CLR") ;
  return(1) ;
}


open_helpfile(helpname)     /* Open helpfile if present. */
char *helpname ;
{
  char *getenv(), name[MAXLINE], *paths, *ptr ;
  int i ;

  i = 0 ;
  ishelp = 1 ;
  if ((hfd = fopen(helpname,"r")) == NULL)
    {
      paths = getenv("PATH") ;
      if ((ptr = paths) && helpname[0] != '/')
        for (;;)
          if (*ptr == ':' || *ptr == 0)
            {
              if (*ptr == 0) break ;
              name[i++] = '/' ;
              name[i] = 0 ;
              STRCAT(name,helpname) ;
              if ((hfd = fopen(name,"r")) != NULL) return ;
              *ptr++ ;
              i = 0 ;
            }
          else name[i++] = *ptr++ ;
      FPRINTF(stderr,"%s: Help file: %s not found\r\n",progname,helpname) ;
      ishelp = 0 ;
    }
}


process_item(n)
int n ;
{
  int i,isvalid ;

  if (n > TITEMS) return ;

  current = buttons[n].value ;
  if (current == 'X') current = 'x' ;         /* Reassign "extra" values. */
  if (current == '*') current = 'x' ;
  if (current == '\015') current = '=' ;
  if (current == 'Q') current = 'q' ;

  if (error)
    {
      isvalid = 0 ;                    /* Must press a valid key first. */
      for (i = 0; i < MAXVALID; i++)
        if (current == validkeys[i]) isvalid = 1 ;
      if (pending == '?') isvalid = 1 ;
      if (!isvalid) return ;
      error = 0 ;
    }

  if (pending)
    {
      for (n = 0; n < TITEMS; n++)
        if (pending == buttons[n].value) break ;
    }
  switch (buttons[n].opdisp)
    {
      case (int) OP_SET   : set_item(OPITEM,buttons[n].str) ;
                            break ;
      case (int) OP_CLEAR : if (error) set_item(OPITEM,"CLR") ;
                            else set_item(OPITEM,"") ;
    }
  (*buttons[n].func)() ;
}


show_display(val)
double val ;
{
  if (!error)
    {
      STRCPY(display,make_number(val)) ;
      set_item(DISPLAYITEM,display) ;
    }
}
