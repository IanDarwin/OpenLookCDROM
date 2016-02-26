/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*        FILE : uwe.c                                           */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/
/*        Die function 'uwe(graph)' fuehrt einen Planaritaetstest*/
/*        durch und gibt das Ergebniss an den Benutzer weiter.   */
/*                                                               */
/*        Wenn der Graph planar ist so werden die einzelnen      */
/*        Knotenlisten ,die bis jetzt nur Knoten mit hoeherer    */
/*        enthalten auf alle Knoten erweitert.                   */
/*                                                               */
/*        Zusaetzlich erhalten die Listen eine neue bessere      */
/*        Strucktur um mit ihnen das planare Zeichnen zu beginnen.*/
/*        Die neue Datenstrucktur steht in 'dfs.c' .             */
/*                                                               */
/*                                                               */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "listen1.h"
#include "listen.h"
#include "modula.h"
#include "sgraphhi.h"
#include "dfs.h"
#include "plzeich.h"
#include "stnumber.h"


BOOLEAN        uwe(graph)
        Sgraph        graph;
BEGIN
        Snode        s, t;
        Sedge        e;

        s = graph -> nodes;
        e = s -> slist;
        if(!e)
        {
                e = s -> tlist;
                t = e -> snode;
        }
        else
        {
                t = e -> tnode;
        }
        undirected_st_number (graph,s,t );

        IF  planar(graph) THEN
            message("The graph ist planar.\n");
            RETURN(TRUE);
           ELSE

            message("The graph is not planar.\n");
            RETURN(FALSE);
         END;
END
