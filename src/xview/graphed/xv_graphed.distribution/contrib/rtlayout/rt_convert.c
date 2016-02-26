/* (C) Universitaet Passau 1986-1991 */
/**********************************************************************************************/
/*                                                                                            */
/* FILE: rt_convert.c                                                                         */
/* =====                                                                                      */
/*                                                                                            */
/* Inhalt: Einbettung der Datenstruktur 'Sgraph' in die Datenstruktur 'Bin_Tree'              */
/*                                                                                            */
/* Bemerkungen:                                                                               */
/*                                                                                            */
/*   enthaltenen Funktionen: 'calc_rt_tree (g)'                                               */
/*                           'calc_sons (t)'                                                  */
/*                           'left_right (t)'                                                 */
/*                           'is_leaf (t)'                                                    */
/*                           'root_of_graph (g)'                                              */
/*                           'no_output_edges (g,single)'                                     */
/*                           'no_input_edges (g,single)'                                      */
/*                                                                                            */
/*   benutzte Variablen: 'rt_error'                                                              */
/*                       'nr_of_rt_error'                                                        */
/*                       'down'                                                               */
/*                                                                                            */
/**********************************************************************************************/
 


#include "std.h"
#include "sgraph.h"
#include "rt_def.h"




Snode no_input_edges (g,single)
Sgraph g;
bool *single;
/**********************************************************************************************/
/* liefert einen Knoten aus 'g', der keine Eingangskanten besitzt und einen Wert 'single',    */
/* der angibt, ob dieser Knoten als Einziger im Baum diese Eigenschaft hat.                   */
/* Die globale Variable 'down' wird hier verbindlich belegt.                                  */
/**********************************************************************************************/
{
  Snode n,no_in;
  Sedge e;
  int count;

  count = 0;
  no_in = empty_node;
  for_all_nodes (g,n)
    {
      if (n->tlist == empty_edge)
        {
          no_in = n;
          count = count + 1;
        }
    }
  end_for_all_nodes (g,n);
  
  *single = (count == 1);
  if (*single)
    down = true;

  return (no_in);
}




Snode no_output_edges (g,single)
Sgraph g;
bool *single;
/**********************************************************************************************/
/* Analog zu 'no_input_edges ()'                                                              */
/**********************************************************************************************/
{
  Snode n,no_out;
  Sedge e;
  int count;

  count = 0;
  no_out = empty_node;
  for_all_nodes (g,n)
    {
      if (n->slist == empty_edge)
        {
          no_out = n;
          count = count + 1;
        }
    }
  end_for_all_nodes (g,n);
  
  *single = (count == 1);
  if (*single)
    down = false;

  return (no_out);
}




Snode root_of_graph (g)
Sgraph g;
/**********************************************************************************************/
/* Liefert die Wurzel eines Graphen 'g'. Eine Wurzel sei dabei definiert als der einzige Kno- */
/* im Graph, der entweder keine Eingangskanten besitzt oder keine Ausgangskanten besitzt.     */
/* Existiert kein solcher Knoten folgt eine Belegung der Fehlervariablen. Es wird ein leerer  */
/* Knoten zurueckgegeben.                                                                     */
/**********************************************************************************************/
{ 
  Snode help;
  bool root,single;
  
  if (g->directed)
    {
      help = no_input_edges (g,&single);
      root = ((help != empty_node) && single);
      if (root)
        return (help);
      else
        {
          help = no_output_edges (g,&single);
          root = ((help != empty_node) && single);
          if (root)
            return (help);
          else
            {
              rt_error = true;
              nr_of_rt_error = 5;
              return (empty_node);
            }
        }
    }
  else
    {
      rt_error = true;
      nr_of_rt_error = 5;
      return (empty_node);
    }
}




bool is_leaf (t)
Bin_Tree t;
/**********************************************************************************************/
/* Testet, ob ein Knoten t ein Blatt ist. Dies ist der Fall, wenn die Richtung des Baumes von */
/* der Wurzel weggeht und  t keine Ausgangskanten besitzt, oder die Richtung des Baumes auf   */
/* die Wurzel zulaeuft und t keine Eingangskanten besitzt.                                    */
/**********************************************************************************************/
{
  if (t != empty_bin_tree)
    {
      if (down)
        {
          return (t->node->slist == empty_edge);
        }
      else
        {
          return (t->node->tlist == empty_edge);
        }
    }
  else
    {
      return (false);
    }
}




void left_right (t)
Bin_Tree t;
/**********************************************************************************************/
/* Bestimmt den linken und rechten Sohn eines Knoten t; dabei wird auf die Koordinaten aus    */
/* dem GRAPHED zurueckgegriffen.                                                              */
/**********************************************************************************************/
{
  Snode help1,help2;
  int ref;

  ref = t->node->x;
  if (down)
    {
      help1 = t->node->slist->tnode;
      help2 = t->node->slist->ssuc->tnode;
          
    }
  else
    {
      help1 = t->node->tlist->snode;
      help2 = t->node->tlist->tsuc->snode;
    }
 
  if (help1 != empty_node)
        {
          if (help2 != help1)
            {
              t->left_son = new_bin_tree ();
              t->right_son = new_bin_tree ();

              if (help1->x < help2->x)
                {
                  t->left_son->node = help1;
                  t->right_son->node = help2;
                }
              else
                {
                  t->left_son->node = help2;
                  t->right_son->node = help1;
                }

              t->left_son->father = t;
              t->left_son->xcoord = t->left_son->ycoord = 0;
              t->left_son->width_left = t->left_son->width_right = 0;
              t->left_son->offset = 0;
              t->left_son->thread = false;

              t->right_son->father = t;
              t->right_son->xcoord = t->right_son->ycoord = 0;
              t->right_son->width_left = t->right_son->width_right = 0;
              t->right_son->offset = 0;
              t->right_son->thread = false;
            }
          else
            {
              if (help1->x <= ref)
                {
                  t->left_son = new_bin_tree ();
                  t->left_son->father = t;
                  t->left_son->node = help1;
                  t->right_son = empty_bin_tree;
                  t->left_son->xcoord = t->left_son->ycoord = 0;
                  t->left_son->width_left = t->left_son->width_right = 0;
                  t->left_son->offset = 0;
                  t->left_son->thread = false;
                }
              else
                {
                  t->right_son = new_bin_tree ();
                  t->right_son->node = help1;
                  t->right_son->father = t;
                  t->left_son = empty_bin_tree;
                  t->right_son->xcoord = t->right_son->ycoord = 0;
                  t->right_son->width_left = t->right_son->width_right = 0;
                  t->right_son->offset = 0;
                  t->right_son->thread = false;
                }
            }
        }
  else 
        {
          t->left_son = t->right_son = empty_bin_tree;
        }
}




void calc_sons (t)
Bin_Tree t;
/**********************************************************************************************/
/* Berechnet einen knoten t und dann rekursiv seine Soehne.                                   */
/**********************************************************************************************/
{
  if (t != empty_bin_tree)
    {
      if (is_leaf (t))
        {
          t->left_son = empty_bin_tree;
          t->right_son = empty_bin_tree;
        }
      else
        {
          left_right (t);
          t->xcoord = t->ycoord = 0;
          t->offset = 0;
          t->width_left = t->width_right = 0;
          calc_sons (t->left_son);
          calc_sons (t->right_son);
        }
    }
}




Bin_Tree calc_rt_tree (g)
Sgraph g;
/**********************************************************************************************/
/* Liefert einen Baum aus dem Graphen 'g'.                                                    */
/**********************************************************************************************/
{
  Bin_Tree t;

  t = new_bin_tree ();
  t->node = root_of_graph (g);
  
  if (!rt_error)
    {
      t->father = empty_bin_tree;
      t->xcoord = t->ycoord = 0;
      t->offset = 0;
      t->width_left = t->width_right = 0;
      t->thread = false;
      calc_sons (t);
    }
  else
    {
      free (t);
      t = empty_bin_tree;
    }

  return (t);
}
