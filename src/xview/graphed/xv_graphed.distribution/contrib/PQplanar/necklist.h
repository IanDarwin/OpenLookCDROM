/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : necklist.h                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/
/*        Aufbau einer Neck-Listen-Datenstrucktur als einfach    */
/*        verkettete Liste von Zeigern auf Necke .               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*****************************************************************/


/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/

typedef ADRlist NECK_LIST;


/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define NECK_LIST_NECK(x) ((NECK)((x)->elem))



/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern void      Necklist_delete_element();

extern void      Necklist_push();

extern void      print_necklist();

extern NECK_LIST compute_Neck_List();

extern void      makiere_Aussenknoten();

