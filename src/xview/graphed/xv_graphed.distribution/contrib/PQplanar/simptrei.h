/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : simptrei.h                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/

/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern void simplex_treiber();

extern void set_matrix_element();

extern void set_start_matrix_element();

extern REAL get_matrix_element();

extern REAL get_start_matrix_element();

extern void set_B_element();

extern void set_start_B_element();
 
extern REAL get_B_element() ;

extern REAL get_start_B_element(); 

extern void set_Cost_element();

extern void set_start_Cost_element();

extern REAL get_Cost_element(); 
 
extern void set_cost0_element();

extern BOOLEAN get_cost0_element(); 
  
extern void set_deleted_element();

extern BOOLEAN get_deleted_element(); 
 
extern void set_X_element(); 

extern REAL get_X_element(); 

extern void set_W_element(); 
 
extern INTEGER get_W_element() ;

extern INTEGER get_max_W_element() ;

extern void set_U_element(); 

extern INTEGER get_U_element(); 

extern INTEGER get_max_U_element(); 

extern void set_M_element(); 

extern INTEGER get_M_element(); 

extern void set_N_element(); 

extern INTEGER get_N_element() ;

extern REAL get_optimal_wert();

extern INTEGER get_maxm();

extern INTEGER get_maxn();



