/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : mtx_stat.c                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 18.09.90                                       */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/
#include "path.h"

#include STDI
#include SGRAPHI

#include "modula.h"

#include "simplex.h"


arrm2n *A;

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void set_matrix_element(m,n,wert)
        INTEGER m, n ;
        REAL    wert;
  BEGIN
    static int firstused = TRUE;
    
    if (firstused) {
    	firstused = FALSE;
    	A = (arrm2n*) calloc ((unsigned)(m2*maxn), sizeof(REAL));
    }
    (*A)[m][n] = wert;
  END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

REAL get_matrix_element(m,n)
        INTEGER m, n ;
  BEGIN
    RETURN((*A)[m][n]);
  END

