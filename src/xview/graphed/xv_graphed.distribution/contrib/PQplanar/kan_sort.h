/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : kan_sort.h                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 25.07.90                                       */
/*****************************************************************/
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*****************************************************************/


/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern BOOLEAN compute_kantensortierung();

extern void    kanten_ausgabe();

extern void    kanten_ausgabe2();

extern REAL    r_winkel();

extern REAL    winkel_zwische_zwei_kanten();

extern REAL    abs_winkel_der_kanten();

extern void    conect_edgelist();

extern void    compute_vektor_with_new_xy();
