/* (C) Universitaet Passau 1986-1991 */
/**********************************************************************************************/
/*                                                                                            */
/* FILE: rt_err.c                                                                             */
/* =====                                                                                      */
/*                                                                                            */
/* Inhalt:  Fehlermeldungen von rt.                                                           */
/*                                                                                            */
/* Bemerkungen:                                                                               */
/*                                                                                            */
/*   enthaltene Funktionen : 'handling_of_rt_errors ()'                                          */
/*                                                                                            */
/*   benutzte Variablen:  'nr_of_rt_error'                                                       */
/*                        'rt_error'                                                             */
/*                                                                                            */
/**********************************************************************************************/



#include "std.h"
#include "sgraph.h"
#include "rt_def.h"



void handling_of_rt_errors ()
/**********************************************************************************************/
/* Ausgabe der erforderlichen Fehlermeldung und ruecksetzen der Fehlervariablen.              */
/**********************************************************************************************/
{ 
  switch (nr_of_rt_error) 
    {
      case 1 : error ("%-80s","tree layout : empty graph\n"); break;
      case 2 : error ("%-80s","tree layout : graph is not a tree, illegal edges\n"); break;
      case 3 : error ("%-80s","tree layout : graph is not a tree, not connected\n"); break;
      case 4 : error ("%-80s","tree layout : graph is not a binary tree\n"); break;
      case 5 : error ("%-80s","tree layout : cannot find root\n"); break;

      case 10: error ("%-80s","tree layout : illegal parameter\n"); break; 
      case 11: error ("%-80s","tree layout : illegal minmum distance\n"); break;
      case 12: error ("%-80s","tree layout : illegal parameter\n"); break;
      case 13: error ("%-80s","tree layout : illegal parameter\n"); break;

      case 100 : error ("%80s","tree layout : graph is not directed\n"); break;
    }
  nr_of_rt_error = 0;
  rt_error = false;  
}
