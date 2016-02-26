/* (C) Universitaet Passau 1986-1991 */
/*********************************************************************************************/
/*                                                                                           */
/* FILE: rt_new_layout.c                                                                     */
/* =====                                                                                     */
/*                                                                                           */
/* Inhalt: Umwandlung der Berechneten Koordinaten                                            */
/*                                                                                           */
/* Bemerkungen:                                                                              */
/*                                                                                           */
/*   enthaltene Funktionen: 'calc_new_layout (t)'                                            */
/*                          'calc_window (t)'                                                */
/*                          'calc_new_coordinates (t,step)'                                  */
/*                                                                                           */
/*   benutzte Variablen: 'rt_window'                                                         */
/*                       'grid_size'                                                         */
/*********************************************************************************************/




#include "std.h"
#include "sgraph.h"
#include "rt_def.h"




void calc_window (t)
Bin_Tree t;
/*********************************************************************************************/
/* Berechnet die extremen Werte des Baumes t, dessen Koordinaten aus dem modifizierten Rein- */
/* gold - Tilford Algorithmus stammen.                                                       */
/*********************************************************************************************/
{
  if (t != empty_bin_tree)
    {
       calc_window (t->left_son);
       calc_window (t->right_son);

       rt_window.min_x = minimum (rt_window.min_x,t->xcoord);
       rt_window.max_x = maximum (rt_window.max_x,t->xcoord);
       rt_window.min_y = minimum (rt_window.min_y,t->ycoord);
       rt_window.max_y = maximum (rt_window.max_y,t->ycoord);
    }
}



void calc_new_coordinates (t,step)
Bin_Tree t;
int step;
/*********************************************************************************************/
/* Berechnet die neuen, fuer den GRAPHED relevanten Koordinaten des Baumlayouts. Die Koordi- */
/* nate (min_x,0) aus dem RT-Koordinatensystem entspricht dabei der Koordinate (step,step)   */
/* im GRAPHED. Die Werte werden natuerlich in den dafuer vorgesehenen Variablen der Knoten   */
/* des Eingabegraphen abgelegt.                                                              */
/*********************************************************************************************/
{
  if (t != empty_bin_tree)
    {
      calc_new_coordinates (t->left_son,step);
      calc_new_coordinates (t->right_son,step);
 
      t->node->x = (t->xcoord + (-rt_window.min_x)) * step + step;
      t->node->y = (t->ycoord + (-rt_window.min_y)) * step + step;
    }
}




void calc_new_layout (t)
Bin_Tree t;
/*********************************************************************************************/
/* Umwandlung der Koordinaten aus dem erweiterten RT-Algorithmus in Koordinaten, die GRAPHED */
/* verarbeiten kann.                                                                         */
/*********************************************************************************************/
{
   calc_window (t);
   calc_new_coordinates (t,grid_size);
}
