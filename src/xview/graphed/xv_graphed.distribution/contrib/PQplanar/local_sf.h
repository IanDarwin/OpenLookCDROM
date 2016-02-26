/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : local_sf.h                                      */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 16.08.90                                       */
/*****************************************************************/

/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/

typedef	enum	{ LOCAL_OPTIMIZE_OBJECT_WINKEL,
                  LOCAL_OPTIMIZE_OBJECT_KANTEN } Local_Optimize_Object;


typedef	enum	{ LOCAL_MODE_MAX_MIN,
                  LOCAL_MODE_MIN_MAX,
                  LOCAL_MODE_MIN_VARIANZ } Local_Optimize_Mode;


typedef enum    { LOCAL_MODE_ALL,
                  LOCAL_MODE_BEST_DIRECTION,
                  LOCAL_MODE_BEST_NODE       } Richtungs_Mode;

typedef enum    { LOCAL_MODE_SMALEST,
                  LOCAL_MODE_BIGGEST         } Knoten_Mode;



/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern void                      show_local_subframe();

extern Local_Optimize_Object     get_optimize_object_mode();

extern Local_Optimize_Mode       get_local_optimize_mode();

extern Richtungs_Mode            get_richtungs_mode();

extern Knoten_Mode               get_knoten_mode();
