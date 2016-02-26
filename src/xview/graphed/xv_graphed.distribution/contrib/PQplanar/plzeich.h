/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : plzeich.h                                      */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 18.09.90                                       */
/*****************************************************************/


/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define SIN(x)  sin(x*M_PI/180.0)
#define COS(x)  cos(x*M_PI/180.0)

#define ADD(x,y) iif( f_greater_or_equal(x+y,360.0) , (x+y)-360.0 , (x+y) )


/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern void    besetze_datenstrucktur();

extern void    vergebe_variable();

extern void    draw_with_optimierungsrichtung_mode();
