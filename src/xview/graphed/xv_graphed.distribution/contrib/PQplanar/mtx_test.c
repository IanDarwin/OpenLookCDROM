/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : mtx_test.c                                     */
/*        AUTOR : Uwe Schnieders                                  */
/*        UPDATE: 23.07.90                                         */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "modula.h"
#include "listen.h"
#include "listen1.h"
#include "dfs.h"
#include "plzeich.h"
#include "sgraphhi.h"
#include "simplexh.h"
#include "simptrei.h"
#include <stdio.h>
#include <math.h>



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void print_matrix(Azeilen,Aspalten)
        INTEGER Azeilen,Aspalten;
  BEGIN
    INTEGER i,j;

#if TEST
      FOR_TO(i,1, get_max_U_element(Azeilen) )
        FOR_TO(j,1, get_max_W_element(Aspalten) )
          printf("%1.0f ",get_matrix_element(i,j));
        END;
        printf("b , %3.0f",get_B_element(i));
        printf("\n");
      END;
      printf("C , \n ");
      FOR_TO(j,1, Aspalten )
        printf("%1.0f ",get_Cost_element(j));
      END;
      printf("\n");
      printf("\n");
#endif
      
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

void print_X(Azeilen)
        INTEGER Azeilen;
  BEGIN
#if TEST
    INTEGER i;

      printf("X , \n ");
      FOR_TO(i,1,Azeilen)            
        printf("%f ",get_X_element(i));
      END;
      printf("\n");
#endif
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

void print_W(Aspalten)
        INTEGER Aspalten;
  BEGIN
#if TEST
    INTEGER i;

      printf("W , \n ");
      FOR_TO(i,1,Aspalten)            
        printf("%d ",get_W_element(i));
      END;
      printf("\n");
#endif
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

void print_M(Aspalten)
        INTEGER Aspalten;
  BEGIN
#if TEST
    INTEGER i;

      printf("M , \n ");
      FOR_TO(i,1,Aspalten)            
        printf("%d ",get_M_element(i));
      END;
      printf("\n");
#endif
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

void print_U(Azeilen)
        INTEGER Azeilen;
  BEGIN
#if TEST
    INTEGER i;

      printf("U , \n ");
      FOR_TO(i,1,Azeilen)            
        printf("%d ",get_U_element(i));
      END;
      printf("\n");
#endif
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

void print_N(Azeilen)
        INTEGER Azeilen;
  BEGIN
#if TEST
    INTEGER i;

      printf("N , \n ");
      FOR_TO(i,1,Azeilen)            
        printf("%d ",get_N_element(i));
      END;
      printf("\n");
#endif
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

void print_F()
  BEGIN
#if TEST
      printf("Optimalwert : \n ");
      printf(" F , %f \n",get_optimal_wert());
#endif
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

void print_ergebniss(Azeilen,Aspalten)
        INTEGER Azeilen,Aspalten;
  BEGIN
#if TEST
      print_matrix(Azeilen,Aspalten);

      print_X(Aspalten);

      print_W(Aspalten);
      print_M(Aspalten);

      print_U(Azeilen);
      print_N(Azeilen);

      print_F();
#endif
  END






