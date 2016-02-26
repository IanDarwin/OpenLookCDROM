/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : neck.h                                         */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/
/*        Aufbau einer Neck-Datenstrucktur als doppelt verkettete*/
/*        Liste von Zeigern.                                     */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*****************************************************************/

/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/

typedef  ADDRlist  NECK;


/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define NECK_INIT(x)        DINIT_LIST(x)

#define NECK_EDGELIST(x)    ((EDGELIST)((x)->elem))

#define NECK_ECKE(x)        ((EDGELIST)((x)->elem))

#define NECK_NODE(x)        EDGELIST_NODE(NECK_ECKE(x))

#define for_all_neck_elements(list,element)    dfor_all_elements(list,(char*)element)

#define end_for_all_neck_elements(list)        dend_for_all_elements(list)

/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern void     setze_Zaehler_null();

extern void     erhoehe_Zaehler();

extern void     print_Neck();

extern void     verbinde_Neck_Edgelist();

extern void     suche_Neck_im_graphen();

extern INTEGER  grad();

extern void     Neck_push();

extern void     Neck_init();

extern EDGELIST Neck_nth_element();

extern void     Neck_set_nth_element();

extern void     Neck_delete_nth_element();



