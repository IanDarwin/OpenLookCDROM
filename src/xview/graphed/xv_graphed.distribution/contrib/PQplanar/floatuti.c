/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : floatuti.c                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "modula.h"
#include <stdio.h>
#include <math.h>

/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define F_EPS 0.0001




BOOLEAN i_zero(x)
        INTEGER x;
  BEGIN
    RETURN(x == 0 );
  END;


BOOLEAN i_not_zero(x)
        INTEGER x;
  BEGIN
    RETURN(x > 0 );
  END;


BOOLEAN f_zero(x)
        REAL x;
  BEGIN
    RETURN(fabs(x) <= F_EPS );
  END;


BOOLEAN f_not_zero(x)
        REAL x;
  BEGIN
    RETURN(fabs(x) > F_EPS );
  END;


BOOLEAN f_equal(x,y)
        REAL x,y;
  BEGIN
    RETURN( f_zero(x - y) );
  END


BOOLEAN f_not_equal(x,y)
        REAL x,y;
  BEGIN
    RETURN( NOT(f_zero(x - y)) );
  END


BOOLEAN f_greater_or_equal(x,y)
        REAL x,y;
  BEGIN
    RETURN( (x - y) >= -F_EPS);
  END


BOOLEAN f_greater(x,y)
        REAL x,y;
  BEGIN
    RETURN( (x - y) > F_EPS);
  END



