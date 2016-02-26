/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : dumpgr.c                                       */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/
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
/*                                                               */
/*                                                               */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "modula.h"
#include "pqtree.h"




#define        UNDEF                 -1




extern        int        fprintf();
extern        char        *sprintf();
extern        FILE        *fopen();
extern        int        fclose();

extern        void        print_graph();

extern        Snode        Root_T_S;


/*        typedefs        *****************************************************/



/*        modul-globals        *****************************************************/

static        int        int_dummy;
static        char        *p_char_dummy;



/*        test-routines        *****************************************************/


/*****************************************************************************

        print_pq_tree_attributes

*****************************************************************************/
static        void        print_pq_tree_attributes (file, g)
        FILE                *file;
        Sgraph                g;
{
}

/*****************************************************************************

        print_pq_node_attributes

*****************************************************************************/
static        void        print_pq_node_attributes (file, n)
        FILE                *file;
        Snode                n;
{
if (n->x >= 0 || n->y >= 0 )fprintf(file, "{$ %d %d $}",n->x,n->y);
}

/*****************************************************************************

        print_pq_edge_attributes

*****************************************************************************/
static        void        print_pq_edge_attributes (file, e)
        FILE                *file;
        Sedge                e;
{
}

/*****************************************************************************

        dump_pq_tree

*****************************************************************************/
BOOLEAN        dump_gr (g)
        Sgraph        g;
{
        static        int        number = 0;
        static        char        fname[] = "dump_gr.000 000 000";
        auto        FILE        *out_file;


        p_char_dummy = sprintf ( fname, "dump_gr.%03d", number++ );
        if ( !( out_file = fopen (fname , "w") ) )
        {
                int_dummy = printf("error opening %s\n",fname);
                RETURN(FALSE);
        }
        print_graph (out_file, g, print_pq_tree_attributes,
                                  print_pq_node_attributes,
                                  print_pq_edge_attributes        );
        if ( fclose (out_file) )
        {
                int_dummy = printf("error closing %s\n",fname);
                RETURN(FALSE);
        }
        RETURN(TRUE);
}



