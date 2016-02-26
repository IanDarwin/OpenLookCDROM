/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : pqtreehi.h                                     */
/*        AUTOR : Uwe Schnieders 				 */
/*        UPDATE: 23.07.90					 */
/*****************************************************************/


/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern void init_PQTree();

extern BOOLEAN Reduce() ;

extern void Put_Sons_from_PNode_to_list();

extern void Put_full_Sons_from_QNode_to_list();

extern void replace_full_sons_off_Qnode();

extern void replace_sons_off_PNode();

extern BOOLEAN Qnode_entartet();

extern void convert_Qnode_to_Pnode();




