/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : pqtest.c                                           */
/*        AUTOR : Uwe Schnieders                                   */
/*        UPDATE: 23.07.90                                         */
/*****************************************************************/
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*****************************************************************/



/*        includes        *****************************************************/
#include "path.h"

#include STDI
#include SGRAPHI

#include "modula.h"
#include "pqtree.h"



/*        defines                *****************************************************/

#define        UNDEF                 -1



/*        externals        *****************************************************/

extern        int        fprintf();
extern        char        *sprintf();
extern        FILE        *fopen();
extern        int        fclose();

extern        void        print_graph();

extern        Snode        Root_T_S;


/*        typedefs        *****************************************************/



/*        modul-globals        *****************************************************/




/*        test-routines        *****************************************************/

#if TEST

/*****************************************************************************

        print_pq_tree_attributes

*****************************************************************************/
static        void        print_pq_tree_attributes (file, g)
        FILE                *file;
        Sgraph                g;
{
        fprintf (file, "\t{$ pq_tree " );
        fprintf (file, "   Root_T_S %d",
                ( Root_T_S ? Root_T_S->nr : UNDEF )        );
        fprintf (file, "   $}\n" );
}

/*****************************************************************************

        print_pq_node_attributes

*****************************************************************************/
static        void        print_pq_node_attributes (file, n)
        FILE                *file;
        Snode                n;
{
        fprintf (file, "\t{$ pq_node " );

        switch (PQ_NODE_TYP(n))
        {
                case PNode:        fprintf (file, "   Typ PNode");
                                break;
                case QNode:        fprintf (file, "   Typ QNode");
                                break;
                case FNode:        fprintf (file, "   Typ FNode");
                                break;
                case RNode:        fprintf (file, "   Typ RNode");
                                break;
                case Leaf :        fprintf (file, "   Typ Leaf ");
                                break;
        }

        fprintf (file, "   FA %3d",
                PQ_NODE_FA(n) ? PQ_NODE_FA(n)->nr : UNDEF );
        fprintf (file, "   FS %3d",
                PQ_NODE_FS(n) ? PQ_NODE_FS(n)->nr : UNDEF );
        fprintf (file, "   SS %3d",
                PQ_NODE_SS(n) ? PQ_NODE_SS(n)->nr : UNDEF );
        fprintf (file, "   LN %3d",
                PQ_NODE_LN(n) ? PQ_NODE_LN(n)->nr : UNDEF );
        fprintf (file, "   RN %3d",
                PQ_NODE_RN(n) ? PQ_NODE_RN(n)->nr : UNDEF );

        fprintf (file, "\n\t           " );
        IF PQ_NODE_TYP(n) == RNode THEN
          fprintf (file, "   direct_node ->%d",
                  ( ((Snode)PQ_NODE_EDGE(n))->nr )               );
         ELSE
          fprintf (file, "   edge %d->%d",
                ( PQ_NODE_EDGE(n) ? PQ_NODE_EDGE(n)->snode->nr : UNDEF ),
                ( PQ_NODE_EDGE(n) ? PQ_NODE_EDGE(n)->tnode->nr : UNDEF ) );
          fprintf (file, "   edge->Snode %d",
                ( PQ_NODE_EDGE(n) ? ((Snode)(PQ_NODE_EDGE(n)->attrs.data))->nr
                          : UNDEF ) );
        END;

        switch (PQ_NODE_LAB(n))
        {
                case empty  :       fprintf (file, "   Lab empty  ");
                                break;
                case full   :        fprintf (file, "   Lab full   ");
                                break;
                case partial:        fprintf (file, "   Lab partial");
                                break;
        }

        switch (PQ_NODE_MARK(n))
        {
                case unmarked :        fprintf (file, "   Mark unmarked ");
                                break;
                case queued   :        fprintf (file, "   Mark queued   ");
                                break;
                case blocked  :        fprintf (file, "   Mark blocked  ");
                                break;
                case unblocked:        fprintf (file, "   Mark unblocked");
                                break;
                case visited  :        fprintf (file, "   Mark visited  ");
                                break;
        }

        fprintf (file, "\n\t           " );
        fprintf (file, "   PCC %d", PQ_NODE_PCC(n) );
        fprintf (file, "   PLC %d", PQ_NODE_PLC(n) );

        fprintf (file, "   $}\n" );
}

/*****************************************************************************

        print_pq_edge_attributes

*****************************************************************************/
static        void        print_pq_edge_attributes (file, e)
        FILE                *file;
        Sedge                e;
{
/*         int_dummy = fprintf (file, "\t{$ pq_edge " );
        int_dummy = fprintf (file, "   edge.attrs.data -> nr %d",
                ( (Snode)(e->attrs.data) ) -> nr );
        int_dummy = fprintf (file, "   $}\n" ); */
}

/*****************************************************************************

        dump_pq_tree

*****************************************************************************/
BOOLEAN        dump_pq_tree (g)
        Sgraph        g;
{
        static        int        number = 0;
        static        char        fname[] = "dump_pqtree.000 000 000";
        auto        FILE        *out_file;

        remove_all_edges(g);
        build_new_edges(g);
        sprintf ( fname, "d_pq.%03d", number++ );
        if ( !( out_file = fopen (fname , "w") ) )
        {
                printf("error opening %s\n",fname);
                RETURN(FALSE);
        }
/*        fprintf(out_file, "\t%s\n\n",fname);  */
        print_graph (out_file, g, print_pq_tree_attributes,
                                  print_pq_node_attributes,
                                  print_pq_edge_attributes        );
        if ( fclose (out_file) )
        {
                printf("error closing %s\n",fname);
                RETURN(FALSE);
        }
        RETURN(TRUE);
}

void        remove_all_edges(g)
        Sgraph        g;
        BEGIN
            Snode node;

           for_all_nodes (g,node);
             WHILE (node->slist != NIL) DO
               remove_edge(node->slist);
             END;
             WHILE (node->tlist != NIL) DO
               remove_edge(node->tlist);
             END;
            end_for_all_nodes (g,node);
        END

void        build_new_edges(g)
        Sgraph        g;
        BEGIN
            Snode node;
           Snode  Akt;
           DirectionType Direc;
           Attributes      attrs;

           attrs.data = NIL;
           for_all_nodes (g,node);
             Akt = PQ_NODE_FS(node);
             IF Akt != NIL THEN 
               Direc = DetermineDirec( Akt ); 
               WHILE Akt != NIL DO
                 make_edge (node, Akt, attrs);
                 Akt = next_Son(Direc,Akt);
               END;
             END;
            end_for_all_nodes (g,node);
        END


          
#endif

