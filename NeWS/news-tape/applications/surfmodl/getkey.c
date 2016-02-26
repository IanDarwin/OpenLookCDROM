#include "surfmodl.h"
int getkey()
{
 char c;           /* value from keypress */
    int Result;   /* result of keypress */

/* Pause for keypress */
  /*
  while ( (!keypressed() ) )
    c = ' ';
  scanf (c);
  if ( (c == '\e') && (keypressed) ) {  */  /* escape character (fcn key) */
  /*
    scanf ( c);
    Result = ord(c) - 58;
    */
  /* Treat a spacebar like a 0 */
  /*
  }  else if ( (ord(c) == 32) )
    Result = 0;
   else
    Result = ord(c) - 48;
  if ( (Result > 0) && (Result < 10) )
     return Result;
   else
     return 0;
     */
     return getchar();
} /* function Getkey */
keypressed()
{
	return getchar();
};
