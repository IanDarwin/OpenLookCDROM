/* (C) Universitaet Passau 1986-1991 */
/**********************************************************************************************/
/*                                                                                            */
/* FILE: rt_verify.c                                                                          */
/* =====                                                                                      */
/*                                                                                            */
/* Inhalt: Ueberpruefung, ob ein Graph (gegeben durch die Datenstruktur 'Sgraph') ein Baum ist*/
/*                                                                                            */
/* Bemerkungen:                                                                               */
/*                                                                                            */
/*   enthaltene Funktionen: 'is_binary_tree (g)'                                              */
/*                          'count_edges (g)'                                                 */
/*                          'count_nodes (g)'                                                 */
/*                          'is_binary (g)'                                                   */
/*                          'serve_node (g,nr)'                                               */
/*                          'indirected_graph (g)'                                            */
/*                                                                                            */
/*   benutzte Funktionen: 'IsConnected ()'                                                    */
/*                        'make_graph ()'                                                     */
/*                        'make_node ()'                                                      */
/*                        'make_edge ()'                                                      */
/*                        'remove_graph ()'                                                   */
/*                        'for_all_nodes () ... end_for_all_nodes ()'                         */
/*                        'for_sourcelist () ... end_for_sourcelist ()'                       */
/*                        'for_targetlist () ... end_for_targetlist ()'                       */
/*                                                                                            */
/*   benutzte Variable: 'rt_error'                                                               */
/*                      'nr_of_rt_error'                                                         */
/*                                                                                            */
/**********************************************************************************************/



#include "std.h"
#include "sgraph.h"
#include "rt_def.h"



int count_edges (g)
Sgraph g;
/**********************************************************************************************/
/* Zaehlen der Kanten einem Graphen 'g'.                                                      */
/**********************************************************************************************/
{
  int number_of_edges;
  Snode n;
  Sedge e;

  if (g == empty_graph)
    {
      number_of_edges = 0;
    }
  else
    { 
      number_of_edges = 0;

      for_all_nodes (g,n)
        {
          e = empty_edge;
          for_sourcelist (n,e)
            number_of_edges = number_of_edges + 1;
          end_for_sourcelist (n,e);

          e = empty_edge;
          for_targetlist (n,e)
            number_of_edges = number_of_edges + 1;
          end_for_targetlist (n,e);
        }
      end_for_all_nodes(g,n);

      if (g->directed)
        number_of_edges = number_of_edges / 2;
    }
  
  return (number_of_edges);
}




int count_nodes (g)
Sgraph g;
/**********************************************************************************************/
/* Zaehlen der Knoten in einem Graphen 'g'                                                    */
/**********************************************************************************************/
{ 
  int number_of_nodes;
  Snode n;
  
  if (g == empty_graph)
    {
      number_of_nodes = 0;
    }
  else 
    {
      number_of_nodes = 0;
      for_all_nodes (g,n)
        number_of_nodes = number_of_nodes + 1;
      end_for_all_nodes (g,n);
    }

  return (number_of_nodes);
}




bool is_binary (g)
Sgraph g;
/**********************************************************************************************/
/* Pruefen, ob ein Baum im GRAPHED-Format binaer ist. D.h. fuer jeden Knoten wird geprueft,   */
/* ob er maximalen Grad 3 hat.                                                                */
/**********************************************************************************************/
{
  int max_degree = 3;
  int noe;
  bool ok;
  Snode n;
  Sedge e;
  
  for_all_nodes (g,n)
    { 
      noe = 0;

      for_sourcelist (n,e)
        noe = noe + 1;
      end_for_sourcelist (n,e);

      for_targetlist (n,e)
        noe = noe + 1;
      end_for_targetlist (n,e);

      if (noe <= max_degree)
        ok = true;
      else 
        ok = false;
    }
  end_for_all_nodes (g,n);

  return (ok);
}



Snode serve_node (g,nr)
Sgraph g;
int nr;
/**********************************************************************************************/
/* Liefert aus einem Graphen 'g' den Knoten mit der Nummer 'nr', falls vorhanden. Andernfalls */
/* wird der Wert 'nil' zurueckgegeben.                                                        */
/**********************************************************************************************/
{
  Snode n,help;

  n = help = nil;
  for_all_nodes (g,help)
    if (help->nr == nr)
      n = help;
  end_for_all_nodes (g,help);
  return (n);
}



Sgraph indirected_graph (g)
Sgraph g;
/**********************************************************************************************/
/* Berechnet den zugrundeliegenden, ungerichteten Graphen eines Graphen 'g'.                  */
/**********************************************************************************************/
{
  Sgraph ig;
  Snode n_ig,n_g,help;
  Sedge e,help_edge;

  if (!g->directed)
    {
      return (g);
    }
  else
    {
      ig = make_graph (g->attrs);
      ig->directed = false;

      for_all_nodes (g,n_g)
        help = make_node (ig,n_g->attrs);
        help->nr = n_g->nr;
      end_for_all_nodes (g,n_g);

      for_all_nodes (ig,n_ig)
        n_g = serve_node (g,n_ig->nr); 

        for_sourcelist (n_g,e)
          help = serve_node (ig,e->tnode->nr);
          help_edge = make_edge (n_ig,help,e->attrs);
        end_for_sourcelist (n_g,e);

       for_targetlist (n_g,e)
          help = serve_node (ig,e->snode->nr);
          help_edge = make_edge (help,n_ig,e->attrs);
        end_for_targetlist (n_g,e); 

      n_ig->tlist = nil;
      end_for_all_nodes (ig,n_ig);

      return (ig);
    }
}




bool is_binary_tree (g)
Sgraph g;
/**********************************************************************************************/
/* Prueft nach, ob einer gegebener Graph 'g' ein binaerer Baum ist und besetzt gegebenenfalls */
/* die entsprechenden Fehlervariablen. Das pruefen erfolgt in der Reihenfolge Knoten und Kan- */
/* ten zaehlen und vergleichen, Zusammenhang des zugrundeliegenden, ungerichteten Graphen zei-*/
/* gen und schliesslich pruefen, ob jeder Knoten maximal zwei Nachfolger hat.                 */
/**********************************************************************************************/
{
   int number_of_edges,number_of_nodes;
   Sgraph ig;
   int help;

   number_of_nodes = count_nodes (g);
   number_of_edges = count_edges (g);

   if (number_of_nodes != (number_of_edges + 1))
     {
       rt_error = true;
       nr_of_rt_error = 2;
       return (false);
     }
   else
     { 
       ig = indirected_graph (g);
       help = IsConnected (ig);
       remove_graph (ig);

       if (!help)
         {
           rt_error = true;
           nr_of_rt_error = 3;
           return (false);
         }
       else
         {
           if (is_binary (g) == false)  
             {
               rt_error = true;
               nr_of_rt_error = 4;
               return (false); 
             }
           else
             {
               return (true);
             }
         }
     } 
}
