/* (C) Universitaet Passau 1986-1991 */
/**********************************************************************************************/
/*                                                                                            */
/* FILE: rt_cor.c                                                                             */
/* =====                                                                                      */
/*                                                                                            */
/* Inhalt: Korrektur von RT - Koordinaten                                                     */
/*                                                                                            */
/* Bemerkungen:                                                                               */
/*                                                                                            */
/*   enthaltene Funktionen : 'correct_if_necessary (t)'                                       */
/*                           'correct_tree (t,level,rmost,lmost)'                             */
/*                           'calc_width_rel (t,pw)'                                          */
/*                           'cwr (t,pminx,pmaxx)'                                            */
/*                           'next_bad_left (t)'                                              */
/*                           'next_bad_right (t)'                                             */
/*                                                                                            */
/*   benutzte Funktionen : 'setup ()'                                                         */
/*                                                                                            */
/*   benutzte Variablen: 'max_feasible_difference'                                            */
/*                       'profit'                                                             */
/*                       'cor_of_tree'                                                        */
/*                                                                                            */
/*   Bevor die Korrektur erfolgen kann, muessen die Startkoordinaten (= RT - Koordinaten)     */
/*   bestimmt sein, da sonst der Platzbedarf des Baumes nicht berechnet werden kann.          */
/*                                                                                            */
/**********************************************************************************************/



#include "std.h"
#include "sgraph.h"
#include "rt_def.h"

 
   
void cwr (t,pminx,pmaxx)
Bin_Tree t;
int *pminx,*pmaxx;
/**********************************************************************************************/
/* Berechnet die rekursiv die Gewichte in dem Baum t, schreibt diese in die dafuer vorgesehe- */
/* nen Variablen und liefert den groessten und kleinsten x-Wert des Baumes zurueck.           */
/**********************************************************************************************/
{
  Bin_Tree l,r;
  int hminx_l,hmaxx_l,hminx_r,hmaxx_r;
 
  if (t != empty_bin_tree)
    {
      if (t->thread)
        {
          /* t ist ein Blatt */
          t->width_left = t->width_right = -1;
          *pminx = *pmaxx = 0;
        }
      else
        {
          l = t->left_son;
          r = t->right_son;

          if ((l == empty_bin_tree) && (r == empty_bin_tree))
            {
              t->width_left = t->width_right = -1;
              *pminx = *pmaxx = 0;
            }
          else
            {
              if ((l == empty_bin_tree) && (r != empty_bin_tree))
                {
                  cwr (r,pminx,pmaxx);
                  *pminx = minimum (*pminx + t->offset,0);
                  *pmaxx = maximum (*pmaxx + t->offset,0);
                  t->width_left = 0;
                  t->width_right = maximum (*pmaxx - *pminx,t->offset);
                }
              else
                {
                  if ((l != empty_bin_tree) && (r == empty_bin_tree))
                    {
                      cwr (l,pminx,pmaxx);
                      *pminx = minimum (*pminx - t->offset,0);
                      *pmaxx = maximum (*pmaxx - t->offset,0);
                      t->width_left = maximum (*pmaxx - *pminx,t->offset);
                      t->width_right = 0;
                    }
                  else
                    {
                      cwr (l,&hminx_l,&hmaxx_l);
                      cwr (r,&hminx_r,&hmaxx_r);
                      
                      t->width_left = maximum (hmaxx_l - hminx_l,t->offset - hminx_l);
                      t->width_right = maximum (hmaxx_r - hminx_r,t->offset + hmaxx_r);

                      *pminx = minimum (hminx_l - t->offset,hminx_r + t->offset);
                      *pmaxx = maximum (hmaxx_r + t->offset,hmaxx_l - t->offset);
                    }
                }
            }
        }
    }
  else
    {
      *pminx = *pmaxx = 0;
    }
}




void calc_width_rel (t,pw)
Bin_Tree t;
int *pw;
/**********************************************************************************************/
/* Berechnet die Breite eines Baumes t aus den relativen Positionen der Soehne mit Hilfe der  */
/* Prozedur 'cwr (t,&min_x,&max_x)'                                                           */
/**********************************************************************************************/
{
  int min_x,max_x;
   
  min_x = max_x = 0;
  cwr (t,&min_x,&max_x);
  *pw = max_x - min_x;
}



Bin_Tree next_bad_left (t)
Bin_Tree t;
/**********************************************************************************************/
/* Liefert den Knoten, bei dem linke Teilbaum ein genuegend grosses Uebergewicht hat.         */
/**********************************************************************************************/
{
  int diff;

  if (t != nil)
    {
      diff = t->width_left - t->width_right;

      if (diff >= (max_feasible_difference - profit))
        {
          return (t);
        }
      else
        {
          return (next_bad_left (t->right_son));
        }
    }
  else
    {
      return (nil);
    } 
}



Bin_Tree next_bad_right (t)
Bin_Tree t;
/**********************************************************************************************/
/* Liefert den Knoten, bei dem der rechte Teilbaum ein genuegend grosses Uebergewicht hat.    */
/**********************************************************************************************/
{
  int diff;

  if (t != nil)
    {
      diff = t->width_left - t->width_right;

      if (-diff >= (max_feasible_difference - profit))
        {
          return (t);
        }
      else
        {
          return (next_bad_right (t->left_son));
        }
    }
  else
    {
      return (nil);
    }
}



void correct_tree (t,level,rmost,lmost)
Bin_Tree t;
int level;
Extreme *rmost,*lmost;
/**********************************************************************************************/
/* Korrigiert nach einer Aenderung eines offset im Baum die Positionen aller Knoten. Dies ent-*/
/* aber genau der Prozedur 'setup ()' aus dem Modul 'rt_alg.c'. Man Beachte, dass es notwendig*/
/* ist die Extremverweise neu zu berechnen.                                                   */
/**********************************************************************************************/
{
  setup (t,level,rmost,lmost,false);
}




void reset_tree (t)
Bin_Tree t;
/**********************************************************************************************/
/* Die alten Werte des Baumes werden wieder als aktuelle Werte gesetzt.                       */
/**********************************************************************************************/
{
  if (t != empty_bin_tree)
    {
      t->offset = t->old_offset;
      if (t->thread)
        {
          t->left_son = t->right_son = empty_bin_tree;
        }
      t->thread = t->old_thread;
      if (t->thread)
        {
          t->left_son = t->l_thread;
          t->right_son = t->r_thread;
        }
      else
        {
          reset_tree (t->left_son);
          reset_tree (t->right_son);
        }
    }
}




void set_tree (t)
Bin_Tree t;
/**********************************************************************************************/
/* Speichern wichtiger Werte, die den Baum beschreiben.                                       */
/**********************************************************************************************/
{
  if (t != empty_bin_tree)
    {
      t->old_offset = t->offset;
      t->old_thread = t->thread;
      if (t->thread)
        {
          t->thread = false;
          t->l_thread = t->left_son;
          t->r_thread = t->right_son;
          t->left_son = t->right_son = empty_bin_tree;
        }
      set_tree (t->left_son);
      set_tree (t->right_son);
    }
}




void correct_if_necessary (t,level,rmost,lmost)
Bin_Tree t;
int level;
Extreme *rmost,*lmost;
/**********************************************************************************************/
/* Aendern des Layouts, falls dieses gewuenscht und dann moeglich ist. Beachte, dass die Kor- */
/* rektur des Layouts von t Auswirkungen auf die Extremverweise haben kann, die daher also    */
/* neu berechnet werden muessen.                                                              */
/**********************************************************************************************/
{
  int old_w,new_w;            /* Platzbedarf vor bzw. nach der Korrektur. Die Korrektur wird  */
                              /* nur durchgefuehrt, wenn keine Verschlechterung in der Breite */
                              /* des Baumes auftritt.                                         */
  int old_offset,difference;
  Bin_Tree help;
  Extreme lm,rm;

  if (cor_of_tree)
    {
      if (t != nil)
        {
          calc_width_rel (t,&old_w);
          difference = t->width_left - t->width_right;

          if (abs(difference) >= max_feasible_difference)
            {
              if (difference > 0)
                {
                  help = next_bad_right (t->left_son);
                }
              else
                {
                  help = next_bad_left (t->right_son);
                }

              if (help != nil)
                {
                  set_tree (t);
                  help->offset = help->offset + profit;
                
                  correct_tree (t,level,&rm,&lm);

                  calc_width_rel (t,&new_w);

                  if ((new_w < old_w) || ((new_w <= old_w) && (lower_equal)))
                    {
                      /* set_tree (t);  */ 
                      *lmost = lm;
                      *rmost = rm;
                      correct_if_necessary (t,level,rmost,lmost);
                    }
                  else
                    { 
                       reset_tree (t);
                    }
                }
            }
        }
    }
}
