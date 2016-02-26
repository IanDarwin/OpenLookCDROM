/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : simplex.c                                      */
/*        AUTOR : Uwe Schnieders                                  */
/*        UPDATE: 23.07.90                                         */
/*****************************************************************/

/* MODULE revisedsimplex; */

/*
Author        : Peter Kleinschmidt, Passau
Machine : IBM-PC or compatibles
Compiler: Turbo-Pascal 3.0
Purpose : Minimizes a linear objective function c for nonnegative variables x
          such that Ax=b, where A is an mxn matrix and b is a nonnegative
          vector .
          To solve larger problems the constants maxm and maxn have to be
          increased and m2 should be set to maxm+2.
          The tolerance eps can be changed if numerical stability is an issue.
Method        : Revised simplex method.
Source        : "Discrete optimization algorithms with Pascal programs" by
          Syslo, Deo and Kowalik, Prentice-Hall, Englewood-Cliffs, 1983.
Input        : Textfile which contains the following data:
          First row: number of constraints (m), number of variables (n)
          Second row+: Elements of matrix A (rowwise)
          Next row: righthandsides (b)        (nonnegative)
          Next row: costcoefficients (c)
Output        : Value of objective function at the optimum,
          Values of the basic variables at the optimum,
          or a message which displays infeasibility or unboundedness
Procedures provided:
parameters: Reads the name of the data file
input: Reads the data file
output: Generates the output
psimplex: Solves the LP (Taken from the above book) */

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "modula.h"
#include "simplex.h"
#include "simptrei.h"


 /*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

 void psimplex(m, n, b, x,c,w,f,nofeas, nosol) 
     INTEGER m, n ; 
     arrm2   b, x ; 
     arrn    c ; 
     arrm    w ; 
     REAL    *f ; 
     BOOLEAN *nofeas, *nosol;

  BEGIN 
 /*   VAR  */
    register INTEGER  i, j, k, l, p, q ; 
    REAL d, r, s ; 
    arrm2m2  u ; 
    arrm2  y ; 
    BOOLEAN  ex, phase, stop ; 

    (*nofeas) = FALSE; 
    (*nosol)  = FALSE; 
    p = m+2; 
    q = m+2; 
    phase = TRUE; 
    k = m+1; 
    FOR_TO(j,1,n) 
      set_matrix_element(k,j,c[j]); 
      s = 0.0; 
      FOR_TO(i,1,m)
        s = s - get_matrix_element(i,j);
      END;
      set_matrix_element(p,j,s);
    END; 
    s = 0.0; 
    FOR_TO(i,1,m) 
      w[i] = n + i; 
      r = b[i]; 
      x[i] = r; 
      s = s-r; 
    END; 
    x[k] = 0.0; 
    x[p] = s; 
    FOR_TO(i,1,p) 
      FOR_TO(j,1,p) 
        u[i][ j] = 0.0
      END; 
      u[i][ i] = 1.0
    END; 
    stop = FALSE; 
    REPEAT 
      IF (x[p] >= (-eps)) AND phase THEN 
        phase = FALSE; 
        q = m+1
      END; 
      d = 0.0; 
      FOR_TO(j,1,n) 
        s = 0.0; 
        FOR_TO(i,1,p) 
          s = s+u[q][ i]*(get_matrix_element(i, j))
        END; 
        IF d > s THEN 
          d = s; 
          k = j;
        END
      END; 
      IF d > (-eps) THEN 
        stop = TRUE; 
        IF phase THEN 
          *nofeas = TRUE
        ELSE 
          *f = ( -(x[q]) );
        END
      ELSE 
        FOR_TO(i,1,q) 
          s = 0.0; 
          FOR_TO(j,1,p) 
            s = s+u[i][ j]*get_matrix_element(j, k)
          END; 
          y[i] = s
        END; 
        ex = TRUE; 
        FOR_TO(i,1,m) 
          IF y[i] >= eps THEN 
            s = x[i]/y[i]; 
            IF ex OR (s < d) THEN 
              d = s; 
              l = i
            END; 
            ex = FALSE
          END
        END; 
        IF ex THEN 
          (*nosol) = TRUE; 
          stop = TRUE
        ELSE 
          w[l] = k; 
          s = 1.0/y[l]; 
          FOR_TO(j,1,m) 
            u[l][ j] = u[l][ j]*s
          END; 
          FOR_TO(i,1,q)
            IF i != l THEN
              s = y[i]; 
              x[i] = x[i]-d*s; 
              FOR_TO(j,1,m) 
                u[i][ j] = u[i][ j]-u[l][ j]*s;
              ENDFOR;
            ENDIF;
          ENDFOR;
          x[l] = d
        END
      END
    UNTIL(stop); 
    IF *nofeas OR *nosol THEN 
      printf("Keine Loesung gefunden \n");
    END;
  END 


