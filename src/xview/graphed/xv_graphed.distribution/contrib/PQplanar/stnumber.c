/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*        FILE : stnumber.c                                      */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/
/*        This file contains the routine                         */
/*        st_number to compute a ST-Numbering                    */
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
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "modula.h"
#include "listen.h"
#include "plan_sf.h"
#include "stnumber.h"
#include "sgraphhi.h"

/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/


extern        void        print_graph();

extern        ADRlist        createlist();
extern        void           releaselist();
extern        int            emptylist();
extern        char           *poplist();
extern        ADRlist        pushlist();
extern        ADRlist        queuelist();



/*        globals                ***********************************************/


/*        modul-globals        ***********************************************/




/*        test-routines        ***********************************************/

#if TEST

/*****************************************************************************

        print_list

*****************************************************************************/
static        void        print_list (file,l)
        FILE                *file;
        LIST                l;
{
        Snode        e;

        for_all_elements(l,(char *)e);
                fprintf(file,"%d  ",PRE_ORDER_NUM(e));
        end_for_all_elements;
        fprintf(file,"\n");
}

/*****************************************************************************

        print_st_graph_attributes

*****************************************************************************/
static        void        print_st_graph_attributes (file, g)
        FILE                *file;
        Sgraph                g;
{
        fprintf (file, "\t{* st_graph" );
        fprintf (file, "   mark %s",
                                ST_GRAPH_MARK(g) == ST_NEW ? "new" : "old" );
        fprintf (file, "   *}\n" );
}

/*****************************************************************************

        print_st_node_attributes

*****************************************************************************/
static        void        print_st_node_attributes (file, n)
        FILE                *file;
        Snode                n;
{
        fprintf (file, "\t{* st_node " );
        fprintf (file, "   mark %s",
                ST_NODE_MARK(n) == ST_NEW ? "new" : "old" );
        fprintf (file, "   pre_order_num %3d", PRE_ORDER_NUM(n) );
        fprintf (file, "   lv %3d", LV(n) );
        fprintf (file, "\n\t           " );
        fprintf (file, "   cycle_w   " );
        print_list (file, CYCLE_W(n));
        fprintf (file, "\n\t           " );
        fprintf (file, "   w_cycle   " );
        print_list (file, W_CYCLE(n));
        fprintf (file, "\n\t           " );
        fprintf (file, "   tree_w    " );
        print_list (file, TREE_W(n));
        fprintf (file, "\n\t           " );
        fprintf (file, "   parent %3d",
                PARENT(n) ? PRE_ORDER_NUM(PARENT(n)) : UNDEF );
        fprintf (file, "   fitting %3d",
                FITTING(n) ? PRE_ORDER_NUM(FITTING(n)) : UNDEF );
        fprintf (file, "   *}\n" );
}

/*****************************************************************************

        print_st_edge_attributes

*****************************************************************************/
static        void        print_st_edge_attributes (file, e)
        FILE                *file;
        Sedge                e;
{
        fprintf (file, "\t{* st_edge " );
        fprintf (file, "   mark %s",
                ST_EDGE_MARK(e) == ST_NEW ? "new" : "old" );
        fprintf (file, "   type %s",
                ST_EDGE_TYPE(e) == TREE ? "tree " : "cycle" );
        fprintf (file, "   *}\n" );
}

/*****************************************************************************

        dump_graph

*****************************************************************************/
static        BOOLEAN        dump_graph (g)
        Sgraph        g;
{
        static        int        number = 0;
        static        char        fname[] = "dumpgraph.000 000 000";
        auto        FILE        *out_file;


        sprintf ( fname, "d_st.%03d", number++ );
        if ( !( out_file = fopen (fname , "w") ) )
        {
                printf("error opening %s\n",fname);
                RETURN(FALSE);
        }
        fprintf(out_file, "\t%s\n\n",fname);
        print_graph (out_file, g, print_st_graph_attributes,
                                  print_st_node_attributes,
                                  print_st_edge_attributes        );
        if ( fclose (out_file) )
        {
                printf("error closing %s\n",fname);
                RETURN(FALSE);
        }
        RETURN(TRUE);
}

#endif


/*        routines        *****************************************************/

/*****************************************************************************

        test_st_number

*****************************************************************************/
BOOLEAN        test_undirected_st_number (graph)
        Sgraph        graph;
{
        Snode        n;
        Sedge        e;
        int        is_st, has_lower, has_higher;
        int        max_num        = 1;
        int        min_num = MAXINT;
        int        highest = 0;

        for_all_nodes(graph,n)
        {
                min_num = MIN( min_num, ST_NUMBER(n) );
                max_num = MAX( max_num, ST_NUMBER(n) );
        } end_for_all_nodes(graph,n)

        is_st = ( min_num == 1 );
        for_all_nodes(graph,n)
        {
                has_lower  = FALSE;
                has_higher = FALSE;
                for_sourcelist(n,e)
                        if ( ST_NUMBER(e->tnode) > ST_NUMBER(n) )      has_higher = TRUE;
                        if ( ST_NUMBER(e->tnode) < ST_NUMBER(n) )      has_lower  = TRUE;
                        highest = MAX( highest, ST_NUMBER(e->tnode) );
                end_for_sourcelist(n,e)

                if ( ST_NUMBER(n) == min_num )
                {
                        is_st = is_st && has_higher && !has_lower;
                        is_st = is_st && ( highest == max_num );
                        continue;
                }
                if ( ST_NUMBER(n) == max_num )
                {
                        is_st = is_st && !has_higher && has_lower;
                        continue;
                }

                is_st = is_st && has_higher && has_lower;
        } end_for_all_nodes(graph,n)

        if (is_st)
        {
                printf("Graph ist von %d bis %d ST-Nummeriert !\n",
                                min_num, max_num                        );
        }
        else
        {
                printf("Graph ist  NICHT  ST-Nummeriert !!!\n");
                RETURN(FALSE);
        }
        RETURN(TRUE);
}


/*****************************************************************************

        alloc_graph_attrs

*****************************************************************************/
static        char        *alloc_graph_attrs ()
{
        return ( calloc ( 1, sizeof(st_graph_attributes) ) );
}

/*****************************************************************************

        alloc_node_attrs

*****************************************************************************/
static        char        *alloc_node_attrs ()
{
        return ( calloc ( 1, sizeof(st_node_attributes) ) );
}

/*****************************************************************************

        alloc_edge_attrs

*****************************************************************************/
static        char        *alloc_edge_attrs ()
{
        return ( calloc ( 1, sizeof(st_edge_attributes) ) );
}

/*****************************************************************************

        alloc_st_attrs

*****************************************************************************/
static        void        alloc_st_attrs (g)
        Sgraph        g;
{
        Snode        n;
        Sedge        e;
        st_graph_attributes *g_att;
        st_node_attributes  *n_att;
        st_edge_attributes  *e_att;


        g_att = (st_graph_attributes*)alloc_graph_attrs ();
        g_att->attrs = g->attrs;
        set_graphattrs(g,make_attr(ATTR_DATA,g_att)); 
        for_all_nodes (g,n)
        {
                n_att = (st_node_attributes*)alloc_node_attrs ();
                n_att->attrs = n->attrs;
                set_nodeattrs(n,make_attr(ATTR_DATA,n_att));
                for_unique_sourcelist (n,e)
                        e_att = (st_edge_attributes*)alloc_edge_attrs ();
                        e_att->attrs = e->attrs;
                        set_edgeattrs(e,make_attr(ATTR_DATA,e_att));
                end_for_unique_sourcelist (n,e)
        } end_for_all_nodes (g,n)
}

/*****************************************************************************

        init_graph_attrs

*****************************************************************************/
static        void        init_graph_attrs (g)
        st_graph_attributes        *g;
{
        g -> mark = ST_NEW;
}

/*****************************************************************************

        init_node_attrs

*****************************************************************************/
static        void        init_node_attrs (n)
        st_node_attributes        *n;
{
        n -> mark                = ST_NEW;
        n -> pre_order_num        = UNDEF;
        n -> lv                        = MAXINT;
        INIT_LIST ( n -> cycle_w );
        INIT_LIST ( n -> w_cycle );
        INIT_LIST ( n -> tree_w  );
        n -> parent                = NULL;
        n -> fitting                = NULL;
}

/*****************************************************************************

        init_edge_attrs

*****************************************************************************/
static        void        init_edge_attrs (e)
        st_edge_attributes        *e;
{
        e -> mark = ST_NEW;
        e -> type = CYCLE;
}

/*****************************************************************************

        init_st_attrs

*****************************************************************************/
static        void        init_st_attrs (g)
        Sgraph        g;
{
        Snode        n;
        Sedge        e;

        init_graph_attrs ( ST_GRAPH_ATTRS(g) );
        for_all_nodes (g,n)
        {
                init_node_attrs ( ST_NODE_ATTRS(n) );
                for_unique_sourcelist (n,e)
                        init_edge_attrs ( ST_EDGE_ATTRS(e) );
                end_for_unique_sourcelist (n,e)
        } end_for_all_nodes (g,n)
}

/*****************************************************************************

        free_graph_attrs

*****************************************************************************/
static        void        free_graph_attrs (p)
        char        *p;
{
        free (p);
}

/*****************************************************************************

        free_node_attrs

*****************************************************************************/
static        void        free_node_attrs (p)
        char        *p;
{
        CLEAR_LIST(  ( (st_node_attributes *)p ) -> cycle_w  );
        CLEAR_LIST(  ( (st_node_attributes *)p ) -> w_cycle  );
        CLEAR_LIST(  ( (st_node_attributes *)p ) -> tree_w   );
        free (p);
}

/*****************************************************************************

        free_edge_attrs

*****************************************************************************/
static        void        free_edge_attrs (p)
        char        *p;
{
        free (p);
}

/*****************************************************************************

        free_st_attrs

*****************************************************************************/
static        void        free_st_attrs (g)
        Sgraph        g;
{
        Snode        n;
        Sedge        e;
        st_graph_attributes *g_att;
        st_node_attributes  *n_att;
        st_edge_attributes  *e_att;

        g_att = attr_data_of_type(g,st_graph_attributes*);
        set_graphattrs(g,g_att->attrs);
        free_graph_attrs ((char*)g_att);
        for_all_nodes (g,n)
        {
                n_att = attr_data_of_type(n,st_node_attributes*);
                set_nodeattrs(n,n_att->attrs);
                free_node_attrs ((char*)n_att);
                for_unique_sourcelist (n,e)
                        e_att = attr_data_of_type(e,st_edge_attributes*);
                        set_edgeattrs(e,e_att->attrs);
                        free_edge_attrs ((char*)e_att);
                end_for_unique_sourcelist (n,e)
        } end_for_all_nodes (g,n)
}


/*****************************************************************************

        mark_old

        marks an edge between sn and tn OLD

*****************************************************************************/
void        mark_old (sn,tn)
        Snode        sn, tn;
{
        Sedge        e;

        for_sourcelist(sn,e)
        {
                if ( e->tnode == tn )
                {
                        ST_EDGE_MARK( e ) = OLD;
                }
        } end_for_sourcelist(sn,e);

}


/*****************************************************************************

        mark_tree

        marks an edge between sn and tn TREE

*****************************************************************************/
void        mark_tree (sn,tn)
        Snode        sn, tn;
{
        Sedge        e;

        for_sourcelist(sn,e)
        {
                if ( e->tnode == tn )
                {
                        ST_EDGE_TYPE( e ) = TREE;
                }
        } end_for_sourcelist(sn,e);

}


/*****************************************************************************

        compute_values

*****************************************************************************/
static        void        compute_values (n)
        Snode        n;
{
        Sedge        e;

        /* compute the fieldes cycle_w, w_cycle, tree_w, fitting, parent */

        for_sourcelist (n,e)
        {
                if ( LV(n) == PRE_ORDER_NUM(e->tnode) || LV(n) == LV(e->tnode) )
                {
                        FITTING(n) = e->tnode;
                }
                if ( ST_EDGE_TYPE(e) == TREE )
                {
                        if ( PRE_ORDER_NUM(e->tnode) < PRE_ORDER_NUM(n) )
                        {
                                /* this is parent */
                                PARENT(n) = e->tnode;
                        }
                        else
                        {
                                PUSH_LIST(TREE_W(n),e->tnode);
                        }
                }
                else
                {
                        if ( PRE_ORDER_NUM(e->tnode) < PRE_ORDER_NUM(n) )
                        {
                                PUSH_LIST(W_CYCLE(n),e->tnode);
                        }
                        else
                        {
                                PUSH_LIST(CYCLE_W(n),e->tnode);
                        }
                }
        } end_for_sourcelist (n,e)
}

/*****************************************************************************

        assign_pre_order

        input :        the next pre-order-number pre to assign to the node n
        result:        the value min_lv - the LOW value of the subtree of n

*****************************************************************************/
static        void        assign_pre_order (n,pre,min_lv)
        Snode        n;
        int        *pre;
        int        *min_lv;
{
        Sedge        e;
        int        help_lv;
        int        is_leaf;


        /* assign the next preorder number */
        PRE_ORDER_NUM(n) = (*pre)++;

        /* search all conncted nodes to compute the minimum preorder number
        of the CYCLE connected nodes; this are the nodes, that have not been
        assigned yet
        is_leaf is kept TRUE, if all connected nodes are assigned */
        is_leaf = TRUE;
        help_lv = *pre - 1;
        for_sourcelist (n,e)
        {
                if ( PRE_ORDER_NUM(e->tnode) == UNDEF )
                {
                        is_leaf = FALSE; break;
                }
                else
                {
                        help_lv = MIN( help_lv, PRE_ORDER_NUM(e->tnode) );
                }
        } end_for_sourcelist (n,e)

        if ( ! is_leaf )
        {
                for_sourcelist (n,e)
                {
                        if ( PRE_ORDER_NUM(e->tnode) == UNDEF )
                        {
                                ST_EDGE_TYPE(e) = TREE;
                                assign_pre_order (e->tnode, pre, min_lv);
                                help_lv = MIN( help_lv, *min_lv );
                        }
                        else
                        {
                                help_lv = MIN(help_lv,PRE_ORDER_NUM(e->tnode));
                        }
                } end_for_sourcelist (n,e)
        }

        LV(n)   = help_lv;
        *min_lv = help_lv;


        /* compute the fieldes cycle_w, w_cycle, tree_w, fitting, parent */
        compute_values (n);
}

/*****************************************************************************

        pre_order

*****************************************************************************/
static        void        pre_order (s_number, t_number)
        Snode        s_number, t_number;
{
        int        pre        = 1;
        int        lv        = MAXINT;

        PRE_ORDER_NUM(s_number) = (pre)++;
        PRE_ORDER_NUM(t_number) = (pre)++;
        mark_tree(s_number,t_number);
        assign_pre_order (t_number,&pre,&lv);
        compute_values(t_number);
        compute_values(s_number);
}


/*****************************************************************************

        path_finder

*****************************************************************************/
LIST        path_finder (v)
        Snode        v;
{
        LIST        path;
        Snode        w;


        INIT_LIST(path);

        if ( ! IS_EMPTY_LIST(W_CYCLE(v)) )
        {
                PUSH_LIST(path,v);
                POP_LIST(W_CYCLE(v),w);
                mark_old(v,w);
                PUSH_LIST(path,w);
                return(path);
        }

        if ( ! IS_EMPTY_LIST(TREE_W(v)) )
        {
                PUSH_LIST(path,v);
                POP_LIST(TREE_W(v),w);
                mark_old(v,w);
                PUSH_LIST(path,w);
                while( ST_NODE_MARK(w) == ST_NEW )
                {
                        Snode        x;

                        x = FITTING(w);
                        ST_NODE_MARK(w) = OLD;
                        mark_old(w,x);
                        PUSH_LIST(path,x);
                        w = x;
                }
                return(path);
        }

        if ( ! IS_EMPTY_LIST(CYCLE_W(v)) )
        {
                PUSH_LIST(path,v);
                POP_LIST(CYCLE_W(v),w);
                mark_old(v,w);
                PUSH_LIST(path,w);
                while( ST_NODE_MARK(w) == ST_NEW )
                {
                        Snode        x;

                        x = PARENT(w);
                        ST_NODE_MARK(w) = OLD;
                        mark_old(w,x);
                        PUSH_LIST(path,x);
                        w = x;
                }
                return(path);
        }

        return(path);
}


/*****************************************************************************

        undirected_st_number

        input :        a undirected Sgraph
                a biconnected graph with no multi-edges or loops
                all attrs pointers will be destroyed; so be sure that this
                fields do not contain any valid data
                the ST-Numbering will be placed in the node-fields nr
                the s and t numbers

*****************************************************************************/
BOOLEAN     undirected_st_number ( graph, s_number, t_number )
        Sgraph        graph;
        Snode        s_number, t_number;
{
        Snode        n;
        Sedge        e;
        int        st_num = 1;
        LIST        stack;
        LIST        path;
        BOOLEAN     result;


        alloc_st_attrs (graph);

        init_st_attrs (graph);

#if TEST
        dump_graph (graph);
#endif

        pre_order (s_number, t_number);

#if TEST
        dump_graph (graph);
#endif


        INIT_LIST(stack);
        n = s_number;
        ST_NODE_MARK(n) = OLD;
        CLEAR_LIST(TREE_W(n));
        for_sourcelist(n,e)
        {
                if ( ST_EDGE_TYPE(e) == TREE )
                {
                        ST_EDGE_MARK(e) = OLD;
                        ST_NODE_MARK(e->tnode) = OLD;
                        PUSH_LIST(stack,e->tnode);
                }
        } end_for_sourcelist(n,e)
        PUSH_LIST(stack,n);

        while ( !IS_EMPTY_LIST(stack) )
        {
                POP_LIST(stack,n);
                path = path_finder(n);
                if ( IS_EMPTY_LIST(path) )
                {
                        ST_NUMBER(n) = st_num++;
                }
                else
                {
                        Snode        help;

                        POP_LIST(path,help);
                        while ( !IS_EMPTY_LIST(path) )
                        {
                                POP_LIST(path,help);
                                PUSH_LIST(stack,help);
                        }
                }
        }


#if TEST
{
        dump_graph (graph);
}
#endif

        result = test_undirected_st_number (graph);

        free_st_attrs (graph);
        RETURN(result);
}


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

BOOLEAN get_st_number_from_label(graph)
        Sgraph        graph;
  BEGIN
        Snode          node;

    for_all_nodes(graph,node)
      sscanf(node->label,"%i",&(node->nr));
    end_for_all_nodes(graph,node)
    RETURN(test_undirected_st_number (graph));
  ENDPROC
  
/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

BOOLEAN     get_undirected_st_number ( graph, s_number, t_number )
        Sgraph        graph;
        Snode        s_number, t_number;
  BEGIN
        BOOLEAN result;

    IF get_st_number_mode() == MODE_COMPUTE_ST_NUMBER THEN
      result = undirected_st_number ( graph, s_number, t_number );
     ELSE
      IF get_st_number_mode() == MODE_STNUMBER_FROM_LABEL THEN
        result = get_st_number_from_label(graph);
       ELSE
        result = test_undirected_st_number (graph);
      ENDIF;
    ENDIF;
    RETURN result ;
  ENDPROC

