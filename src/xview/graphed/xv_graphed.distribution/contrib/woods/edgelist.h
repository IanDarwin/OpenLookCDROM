/* (C) Universitaet Passau 1986-1991 */
/*******************************************************************/
/*         FILE : edgelist.h                                       */
/*        AUTOR : Uwe Schnieders                                   */
/*        UPDATE: 23.07.90                                         */
/*******************************************************************/
/*        Der 'struct edge_list_rec' wird als Datenelement an      */
/*        die doppelt verkettete Liste eines jeden Knoten gehaengt.*/
/*        Es wird praktisch auf die meist gerichtete Strucktur des */
/*        Sgraph (GraphEd) eine ungerichtete Datenstrucktur        */
/*        draufgesetzt.                                            */
/*                                                                 */
/*        node : ist der Knoten dem Diese Liste gehoert            */
/*        edge : ist die parallele gerichtete Kante des Sgraph     */
/*        cores: ist die korespondierende Strucktur des anderen    */
/*                Knoten am anderen Ende der Kante                 */
/*        full :         zeigt ob der Winkel bereits besetzt ist   */
/*                                                                 */
/*        var  :         Nummer der Spalte im Simplextableau       */
/*                                                                 */
/*                                                                 */
/*******************************************************************/

/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/

typedef ADDRlist EDGELIST;

typedef struct edge_list_rec { INTEGER var;
                               REAL        xw;
                               Sedge    edge;
                               Snode    node;
                               EDGELIST cores;
                               ADDRlist neck;
                               BOOLEAN full;
                               BOOLEAN fix;
                               BOOLEAN aussen;
                               BOOLEAN reached;
                               REAL fix_xw;
                             } *edge_list_Ptr;



/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define EDGELIST_INIT(x)     DINIT_LIST(x)

#define EDGELIST_SUC(x)      ADDRLIST_SUC(x)

#define EDGELIST_PRE(x)      ADDRLIST_PRE(x)

#define EDGELIST_ELEMENT(x)   ((edge_list_Ptr)((x)->elem))

#define EDGELIST_VAR(x)       \
     (EDGELIST_ELEMENT(x)->var)

#define EDGELIST_XW(x)       \
     (EDGELIST_ELEMENT(x)->xw)

#define EDGELIST_EDGE(x)      \
     (EDGELIST_ELEMENT(x)->edge)

#define EDGELIST_NODE(x)      \
     (EDGELIST_ELEMENT(x)->node)

#define EDGELIST_CORES(x)      \
     (EDGELIST_ELEMENT(x)->cores)

#define EDGELIST_NECK(x)      \
     (EDGELIST_ELEMENT(x)->neck)

#define EDGELIST_FULL(x)      \
     (EDGELIST_ELEMENT(x)->full)

#define EDGELIST_AUSSEN(x)      \
     (EDGELIST_ELEMENT(x)->aussen)

#define EDGELIST_REACHED(x)      \
     (EDGELIST_ELEMENT(x)->reached)

#define EDGELIST_FIX(x)      \
     (EDGELIST_ELEMENT(x)->fix)

#define EDGELIST_FIX_XW(x)      \
     (EDGELIST_ELEMENT(x)->fix_xw)


#define PUSH_EDGELIST(list,node,edge)  \
        (list) = push_edgelist(list,node,edge)

#define PUSH_NODE_EDGELIST(node,edge)  \
        NODE_EDGELIST(node) = push_edgelist(NODE_EDGELIST(node),node,edge)

#define QUEUE_NODE_EDGELIST(node,edge)  \
        NODE_EDGELIST(node) = queue_edgelist(NODE_EDGELIST(node),node,edge)

#define for_edgelist(node,edgelist_element)  \
        dfor_all_elements(NODE_EDGELIST(node),(char*)edgelist_element)

#define SORT_EDGELIST(list,less) \
        dsort_list(&list,less)

#define end_for_edgelist(node)  \
        dend_for_all_elements(NODE_EDGELIST(node))

/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/

extern EDGELIST push_edgelist();

extern EDGELIST queue_edgelist();

extern void edgelist_verbinde();

extern EDGELIST edgelist_insert_after();

extern EDGELIST edgelist_insert_before();


