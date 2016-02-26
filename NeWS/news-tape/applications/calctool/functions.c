
/*  functions.c
 *
 *  This file contains the seperate functions used by calctool,
 *  whenever a calculator button is pressed.
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

#include "calctool.h"
#include "color.h"
#include "extern.h"

BOOLEAN ibool() ;
double setbool() ;


do_atrig()
{
  switch (current)
    {
      case ')' : disp_val = acos(disp_val) ;     /* cos-1. */
                 break ;
      case '}' : disp_val = asin(disp_val) ;     /* sin-1. */
                 break ;
      case 'T' : disp_val = atan(disp_val) ;     /* tan-1. */
    }

  tresults[(int) DEG]  = disp_val * 180.0 / M_PI ;
  tresults[(int) GRAD] = disp_val * 200.0 / M_PI ;
  tresults[(int) RAD]  = disp_val ;
  cur_op = current ;
  show_display(tresults[(int) ttype]) ;
  disp_val = tresults[(int) ttype] ;
}


do_base()    /* Change the current base setting. */
{
  switch (current)
    {
      case 'B' : base = BIN ;
                 break ;
      case 'O' : base = OCT ;
                 break ;
      case 'D' : base = DEC ;
                 break ;
      case 'H' : base = HEX ;
    }
  grey_buttons(base) ;
  set_item(BASEITEM,base_str[(int) base]) ;
  show_display(disp_val) ;
  make_registers() ;
}


do_calculation()      /* Perform arithmetic calculation and display result. */
{
  if (current == '=' && old_cal_value == '=')
    if (new_input) result = last_input ;
    else disp_val = last_input ;

  if (current != '=' && old_cal_value == '=') cur_op = '?' ;
  switch (cur_op)
    {
      case ')'    :                                     /* cos-1. */
      case '}'    :                                     /* sin-1. */
      case 'T'    :                                     /* tan-1. */
      case '('    :                                     /* cos. */
      case '{'    :                                     /* sin. */
      case 't'    :                                     /* tan. */
      case '?'    : result = disp_val ;                 /* Undefined. */
                    break ;
      case '+'    : result += disp_val ;                /* Addition. */
                    break ;
      case '-'    : result -= disp_val ;                /* Subtraction. */
                    break ;
      case 'x'    : result *= disp_val ;                /* Multiplication. */
                    break ;
      case '/'    : result /= disp_val ;                /* Division. */
                    break ;
      case '%'    : result *= disp_val * 0.01 ;         /* % */
                    break ;
      case 'Y'    : result = pow(result,disp_val) ;     /* y^x */
                    break ;
      case '&'    : result = setbool(ibool(result) & ibool(disp_val)) ;  /* AND */
                    break ;
      case '|'    : result = setbool(ibool(result) | ibool(disp_val)) ;  /* OR */
                    break ;
      case '^'    : result = setbool(ibool(result) ^ ibool(disp_val)) ;  /* XOR */
                    break ;
      case 'n'    : result = setbool(~(ibool(result) ^ ibool(disp_val))) ;  /* XNOR */
                    break ;
      case '='    : break ;                             /* Equals. */
    }
  show_display(result) ;
  if (!(current == '=' && old_cal_value == '=')) last_input = disp_val ;

  disp_val = result ;
  if (current != '=') cur_op = current ;
  old_cal_value = current ;
  new_input = 0 ;
}


do_clear()       /* Clear the calculator display and re-initialise. */
{
  clear_display() ;
  if (error) set_item(DISPLAYITEM,"") ;
  initialise() ;
}


do_constant()
{
  switch (current)
    {
      case 'E' : disp_val = M_E ;      /* e. */
                 break ;
      case 'P' : disp_val = M_PI ;     /* PI. */
    }
  show_display(disp_val) ;
}


do_delete()     /* Remove the last numeric character typed. */
{
  if (strlen(display)) display[strlen(display)-1] = '\0' ;
  set_item(DISPLAYITEM,display) ;
  disp_val = convert_display() ;    /* Convert input to a number. */
}


do_factorial(val)     /* Calculate the factorial of val. */
double val ;
{
  double a ;
  int i ;

  if (val == (int) val)
    {
      i = val ;
      a = 1.0 ;
      while ((i > 0) && (a != HUGE)) a *= (float) i-- ;
    }
  else
    {
      a = gamma(val+1) ;
      a = exp(a) ;
      if (signgam) a = -a ;
    }
  return (a) ;
}


do_immediate()
{
  switch (current)
    {
      case '#' : disp_val = exp(disp_val) ;                     /* e^x */
                 break ;
      case '$' : disp_val = exp(M_LN10*disp_val) ;              /* 10^x */
                 break ;
      case 'N' : disp_val = log(disp_val) ;                     /* ln */
                 break ;
      case 'G' : disp_val = log10(disp_val) ;                   /* log */
                 break ;
      case 'S' : disp_val = sqrt(disp_val) ;                    /* SQRT */
                 break ;
      case 'R' : disp_val = 1.0 / disp_val ;                    /* 1/x */
                 break ;
      case '@' : disp_val *= disp_val ;                         /* x^2 */
                 break ;
      case 'C' : disp_val = -disp_val ;                         /* CHS */
                 break ;
      case 'i' : disp_val = setbool(ibool(disp_val)) ;          /* &32 */
                 break ;
      case 'h' : disp_val = setbool(ibool(disp_val) & 0xffff) ; /* &16 */
                 break ;
      case '!' : disp_val = do_factorial(disp_val) ;            /* x! */
                 break ;
      case '~' : disp_val = setbool(~ibool(disp_val)) ;         /* NOT */
    }
  show_display(disp_val) ;
}


do_keys()
{
  make_canvas(1) ;
}


do_nothing()    /* Dummy routine for the two blank keys. */
{
}


do_number()
{
  int n ;
  static int maxvals[4] = {1, 7, 9, 15} ;

  n = current - '0' ;
  if (base == HEX && current >= 'a' && current <= 'f')
    n = current - 'a' + 10 ;
  if (n > maxvals[(int) base]) return ;

  if (toclear)
    {
      SPRINTF(display,"%c",current) ;
      toclear = 0 ;
    }
  else if (strlen(display) < disp_length[(int) base])
    STRNCAT(display,&current,1) ;
  set_item(DISPLAYITEM,display) ;
  disp_val = convert_display() ;    /* Convert input to a number. */
  new_input = 1 ;
}


do_pending()
{
  char help_str[MAXLINE],nextline[MAXLINE],*p ;
  int n,shift,y ;
  BOOLEAN temp ;

  switch (pending)
    {
      case 'A' : if (current >= '0' && current <= '9')         /* ACC. */
                   {
                     accuracy = char_val(current) ;
                     make_registers() ;
                   }
                 break ;
      case '?' : if (pending_op == '?')                        /* HELP. */
                   {
                     if (ishelp) ishelp++ ;
                     pending_op = '=' ;
                     make_canvas(0) ;
                     set_cursor(MAINCURSOR) ;
                   }
                 else
                   {
                     clear_canvas(KEYCANVAS,WHITE) ;
                     y = 20 ;
                     if (!ishelp) text(20,y,KEYCANVAS,NFONT,BLACK,"No help file found.") ;
                     else
                       { 
                         for (n = 0; n < TITEMS; n++)
                           if (current == buttons[n].value) break ;
                         color = (iscolor) ? buttons[n].color : WHITE ;
                         clear_canvas(KEYCANVAS,color) ;
                         SPRINTF(help_str,"_%s_\n",buttons[n].str) ;
                         rewind(hfd) ;
                         y = 15 ;
                         p = fgets(nextline,BUFSIZ,hfd) ;
                         if (EQUAL(p,"_calctool.help_\n"))
                           {
                             while (p = fgets(nextline,BUFSIZ,hfd))
                               if (*p == '_' && EQUAL(p,help_str)) break ;
                             if (!p) text(5,y,KEYCANVAS,NFONT,BLACK,"No help for this item.") ;
                             for (;;)
                               {
                                 FGETS(nextline,BUFSIZ,hfd) ;
                                 if (nextline[0] == '_') break ;
                                 nextline[strlen(nextline)-1] = '\0' ;
                                 text(5,y,KEYCANVAS,NFONT,BLACK,nextline) ;
                                 y += 15 ;
                               }
                           }
                         else text(5,y,KEYCANVAS,NFONT,BLACK,"Invalid help file given.") ;
                       }    
                     text(5,y+25,KEYCANVAS,NFONT,BLACK,"Click LEFT or press any valid key.") ;
                     pending_op = '?' ;
                     return ;
                   }
                 break ;
      case 's' :                                               /* STO. */
      case 'r' : if (current >= '0' && current <= '9')         /* RCL. */
                   {
                     switch (pending)
                       {
                         case 'r' : disp_val = mem_vals[char_val(current)] ;
                                    break ;
                         case 's' : switch (pending_op)
                                      {
                                        case '+' : mem_vals[char_val(current)] += disp_val ;
                                                   break ;
                                        case '-' : mem_vals[char_val(current)] -= disp_val ;
                                                   break ;
                                        case 'x' : mem_vals[char_val(current)] *= disp_val ;
                                                   break ;
                                        case '/' : mem_vals[char_val(current)] /= disp_val ;
                                                   break ;
                                        case '=' : mem_vals[char_val(current)] = disp_val ;
                                      }
                                    make_registers() ;
                       }
                     break ;
                   }
                 else if (current == '+' || current == '-' ||
                          current == 'x' || current == '/')
                   {
                     pending_op = current ;
                     return ;
                   }
                 break ;
      case '<' :
      case '>' : if (current >= '0' && current <= '9')
                   {
                     for (n = 0; n < TITEMS; n++)
                       if (current == buttons[n].value) break ;
                     shift = char_val(buttons[n].value) ;
                     temp = ibool(convert_display()) ;
                     switch (pending)
                       {
                         case '<' : temp = temp << shift ;
                                    break ;
                         case '>' : temp = temp >> shift ;
                       }
                     STRCPY(display,make_number(setbool(temp))) ;
                     disp_val = last_input = convert_display() ;
                   }
                 break ;
      default  : if (!pending)
                   {
                     pending = current ;
                     pending_op = '=' ;
                     if (pending == '?') set_cursor(HELPCURSOR) ;
                     if (pending == '?' && (ishelp <= 1)) do_pending() ;
                     return ;
                   }
    }    
  show_display(disp_val) ;
  if (error) set_item(OPITEM,"CLR") ;
  else set_item(OPITEM,"") ;
  pending = 0 ;
}


do_point()       /* Handle numeric point. */
{
  if (!pointed)
    {
      if (toclear)
        {
          STRCPY(display,".") ;
          toclear = 0 ;
        }
      else if (strlen(display) < disp_length[(int) base])
        STRNCAT(display,".",1) ;
      pointed = 1 ;
    }
  set_item(DISPLAYITEM,display) ;
  disp_val = convert_display() ;    /* Convert input to a number. */
}


do_portion()
{
  switch (current)
    {
      case 'U' : disp_val = fabs(disp_val) ;       /* ABS. */
                 break ;
      case 'F' : disp_val -= (int) disp_val ;      /* FRAC. */
                 break ;
      case 'I' : disp_val = (int) disp_val ;       /* INT. */
    }
  show_display(disp_val) ;
}


do_trig()
{
  double temp ;

       if (ttype == DEG)  temp = disp_val * M_PI / 180.0 ;
  else if (ttype == GRAD) temp = disp_val * M_PI / 200.0 ;
  else                    temp = disp_val ;

  switch (current)
    {
      case '(' : tresults[(int) RAD] = cos(temp) ;   /* cos. */
                 break ;
      case '{' : tresults[(int) RAD] = sin(temp) ;   /* sin. */
                 break ;
      case 't' : tresults[(int) RAD] = tan(temp) ;   /* tan. */
    }
  tresults[(int) DEG]  = tresults[(int) RAD] ;
  tresults[(int) GRAD] = tresults[(int) RAD] ;

  cur_op = current ;
  show_display(tresults[(int) ttype]) ;
  disp_val = tresults[(int) ttype] ;
}


do_trigtype()   /* Change the current trigonometric type. */
{
  switch (current)
    {
      case CTRL('d') : ttype = DEG ;
                       break ;
      case CTRL('g') : ttype = GRAD ;
                       break ;
      case CTRL('r') : ttype = RAD ;
    }
  if (cur_op == ')' || cur_op == '}' || cur_op == 'T' ||
      cur_op == '(' || cur_op == '{' || cur_op == 't')
    {
      disp_val = tresults[(int) ttype] ;
      show_display(tresults[(int) ttype]) ;
    }
  set_item(TTYPEITEM,ttype_str[(int) ttype]) ;
}


BOOLEAN
ibool(x)
double x ;
{
  BOOLEAN p ;
 
  if (x > 68719476736.00) return(0) ;
  else if (x < -68719476736.00) return(0) ;
       else
         {
           while(x < 0.0) x += 4294967296.00 ;
           while(x > 4294967296.00) x -= 4294967296.00 ;
           p = x ;
           return (p) ;
         }
}


double
setbool(p)
BOOLEAN p ;
{
  BOOLEAN q ;
  double val ;

  q = p & 0x80000000 ;
  p &= 0x7fffffff ;
  val = p ;
  if (q) val += 2147483648.0 ;
  return(val) ;
}
