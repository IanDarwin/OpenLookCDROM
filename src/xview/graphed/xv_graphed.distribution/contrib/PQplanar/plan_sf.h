/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : plan_sf.h                                      */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 16.08.90                                       */
/*****************************************************************/

/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/

typedef	enum	{ MODE_EASY,
                  MODE_TRAINGULATION } Optimize_Mode;


typedef enum    { MODE_AUS_PLANAREN_GRAPH,
                  MODE_AUS_PLANARTEST } Kantensortierung_Mode;


typedef enum    { MODE_MAXIMALES_NECK,
                  MODE_MINIMALES_NECK,
                  MODE_RECHTECK,
                  MODE_MAXIMALE_AUSDEHNUNG } Ausseneck_Mode;


typedef enum    { MODE_GLOBALE_OPITMIERUNG,
                  MODE_INNEN_NACH_AUSSEN,
                  MODE_AUSSEN_NACH_INNEN  } Optimierungsrichtung_Mode;


typedef enum    { MODE_GRAPH_IS_STNUMBERT ,
                  MODE_STNUMBER_FROM_LABEL,
                  MODE_COMPUTE_ST_NUMBER  } ST_number_Mode;


typedef enum    { MODE_TIEFENSUCHE_TRIANGULIERUNG,
                  MODE_STRAHL_TRIANGULIERUNG ,
                  MODE_RAND_TRIANGULIERUNG ,
                  MODE_PUNKT_EIN_TRIANGULIERUNG,
                  MODE_SORTIERTE_TRIANGULIERUNG }  Triangulierungs_Mode;


typedef enum    { MODE_EINFACHER_SIMPLEX,
                  MODE_MPS_SIMPLEX }  Simplex_Mode;



/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern Optimize_Mode             get_optimize_mode();

extern Kantensortierung_Mode     get_kantensortierung_mode();

extern Ausseneck_Mode            get_ausseneck_mode();
 
extern Optimierungsrichtung_Mode get_optimierungsrichtung_mode();

extern ST_number_Mode            get_st_number_mode();

extern Triangulierungs_Mode      get_triangulierungs_mode();

extern Simplex_Mode              get_simplex_mode();

extern void	                 show_plan_subframe ();
