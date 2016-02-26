/* (C) Universitaet Passau 1986-1991 */
/**********************************************************************************************/
/*                                                                                            */
/* FILE: rt_alg.c                                                                             */
/* =====                                                                                      */
/*                                                                                            */
/* Inhalt:  Der Reingold-Tilford - Algorithmus                                                */
/*          Funktionsweise: siehe  E.M. Reingold & J.S. Tilford                               */
/*                                 'Tidier Drawings of Trees'                                 */
/*                                 IEEE Transactions on Software Engineering                  */
/*                                 Vol. SE-7, No. 2 March 1981                                */
/*                                                                                            */
/* Bemerkungen:                                                                               */
/*                                                                                            */
/*   enthaltene Funktionen:  'modified_rt_algorithm (t)'                                      */
/*                           'setup (t,level,lmost,rmost,call_correction)'                    */
/*                           'petrify (t,xpos)'                                               */
/*                                                                                            */
/*   benutzte Funktionen: 'correct_if_necessary ()'                                           */
/*                                                                                            */
/*   benutzte Variablen: 'minsep'                                                             */
/*                                                                                            */
/**********************************************************************************************/



#include "std.h"
#include "sgraph.h"
#include "rt_def.h"



void petrify (t,xpos)
Bin_Tree t;
int xpos;
/**********************************************************************************************/
/* Aufgabe: Umrechnung von relativen Koordinaten in absolute Koordinaten in einem Vorwaerts-  */
/*          lauf.                                                                             */
/* Parameter: t    ... der zu berechnende Baum                                                */
/*            xpos ... absolute Position der x-Koordinate der Wurzel von t                    */
/**********************************************************************************************/
{
  if (t != nil)
    {
      t->xcoord = xpos;
      if (t->thread)
        {
          t->thread = false;
          t->left_son = t->right_son = nil;
        }
      petrify (t->left_son,xpos - t->offset);
      petrify (t->right_son,xpos + t->offset);
    }
}




Global void setup (t,level,rmost,lmost,call_correction)
Bin_Tree t;
int level;
Extreme *rmost,*lmost;
bool call_correction;
/**********************************************************************************************/
/* Aufgabe: Berechnung der relativen Koordinaten der Knoten eines Binaerbaumes                */
/* Parameter: t               ... der zu berechnende Binaerbaum                               */
/*            level           ... die aktuelle Hoehe im Baum (= y-Koordinate der Wurzel von t)*/
/*            rmost           ... der rechteste Knoten des Baumes                             */
/*            lmost           ... der linkeste Knoten des Baumes                              */
/*            call_correction ... Verzweigungsparameter, ob die Korrektur-Prozedur aufgerufen */
/*                                werden soll.                                                */
/*                                ACHTUNG: Dieser Parameter hat nichts mit dem globalen Wert  */
/*                                         von 'cor_of_tree', der angibt ob eine Korrektur    */
/*                                         gewuenscht ist zu tun. Er wird lediglich zur Ter-  */
/*                                         minierung benoetigt, da 'setup' die Funktion 'cor- */
/*                                         rect_if_necessary' und diese wiederum 'setup' auf- */
/*                                         ruft.                                              */
/**********************************************************************************************/
{
  Bin_Tree l,r;          /* Linker bzw. rechter Teilbaum (Hilsfvariable)                      */
  Extreme lr,ll,rr,rl;   /* lr = 'rechtester' Knoten auf niedrigstem Level im linken Teilbaum */
                         /* Restlichen Definitionen analog                                    */
  int cursep;            /* Abstand waehrend Brechnung auf aktuellem Level                    */
  int rootsep;           /* Abstand zum Knoten t                                              */
  int loffsum,roffsum;

  if (t == nil)
    {
      lmost->level = rmost->level = -1;
    }
  else 
    {
      
      l = t->left_son;
      r = t->right_son;
      t->ycoord = level;


      /****************************************************************************************/
      /*                 Berechnen des linken und rechten Teilbaumes                          */
      /****************************************************************************************/
      setup (l,level+1,&lr,&ll,call_correction);
      setup (r,level+1,&rr,&rl,call_correction);


      /****************************************************************************************/
      /* Berechnen des offset von t; d.h. zusammenschieben der Teilbaeume soweit wie moeglich */
      /****************************************************************************************/
      if ((r == nil) && (l == nil))
        { 
          /* t ist ein Blatt */ 
          rmost->link = lmost->link = t;
          rmost->level = lmost->level = level;
          rmost->off = lmost->off = 0;
          t-> offset = 0;
        }
      else 
        {
           /* Teilbaeume zusammenschieben */
          cursep = rootsep = minsep;
          loffsum = roffsum = 0;

          while ((l != nil) && (r != nil))
            {
              if (cursep < minsep)
                {
                  rootsep = rootsep + (minsep - cursep);
                  cursep = minsep;
                }

              if (l->right_son != nil)
                {
                  loffsum = loffsum + l->offset;
                  cursep = cursep - l->offset;
                  l = l->right_son;
                }
              else 
                {  
                  loffsum = loffsum - l->offset;
                  cursep = cursep + l->offset;
                  l = l->left_son;
                }

              if (r->left_son != nil)
                {
                  roffsum = roffsum - r->offset;
                  cursep = cursep - r->offset;
                  r = r->left_son;
                }
              else
                {
                  roffsum = roffsum + r->offset;
                  cursep = cursep + r->offset;
                  r = r->right_son;
                }
            } /* of while */

            /**********************************************************************************/
            /* ACHTUNG: An dieser Stelle Aenderung gegenueber der Prozedur aus obigem Litera- */
            /*          turverweis.                                                           */
            /**********************************************************************************/
            if (call_correction)
              /********************************************************************************/
              /* Normale Berechnung des Offsets der Wurzel nach dem RT-Algorithmus            */
              /********************************************************************************/

              t->offset = (rootsep + 1) / 2;
            else
              /********************************************************************************/
              /* 'setup' wurde von 'correct_if_necessary' aufgerufen, d.h. der Offset muss    */
              /* eventuell geaendert werden, da im Baum korrigiert wurde. Der Offset kann nur */
              /* groesser werden.                                                             */
              /********************************************************************************/      

              t->offset = maximum (t->offset,((rootsep + 1)/2)); 
             
            /**********************************************************************************/
            /*            Berechnen der Rueckgaben 'rmost' und 'lmost'                        */
            /**********************************************************************************/
            loffsum = loffsum - t->offset;
            roffsum = roffsum + t->offset;

            if ((rl.level > ll.level) || (t->left_son == nil))
              {
                *lmost = rl;
                lmost->off = lmost->off + t->offset;
              }
            else
              {
                *lmost = ll;
                lmost->off = lmost->off - t->offset;
              }

            if ((lr.level > rr.level) || (t->right_son == nil))
              {
                *rmost = lr;
                rmost->off = rmost->off - t->offset;
              }
            else
              {
                *rmost = rr;
                rmost->off = rmost->off + t->offset;
              }

            
 
            /**********************************************************************************/
            /*                      Berechnen der Kontur des Baumes                           */
            /**********************************************************************************/
            if ((l != nil) && (l != t->left_son))
              {
                rr.link->thread = true;
                rr.link->offset = abs((rr.off + t->offset) - loffsum);
                if ((loffsum - t->offset) <= rr.off)
                  rr.link->left_son = l;
                else
                  rr.link->right_son = l;
              }
            else
              {
                if ((r != nil) && (r != t->right_son))
                  {
                    ll.link->thread = true;
                    ll.link->offset = abs((ll.off - t->offset) - roffsum);
                    if ((roffsum + t->offset) >= ll.off)
                      ll.link->right_son = r;
                    else
                      ll.link->left_son = r; 
                  }
              }


            /**********************************************************************************/
            /*        Korrektur des Baumes, falls gewuenscht und moeglich                     */
            /**********************************************************************************/
            if (call_correction)
              {
                correct_if_necessary (t,level,rmost,lmost);
              }
         }
    }
}




Global void modified_rt_algorithm (t)
Bin_Tree t;
/**********************************************************************************************/
/* Aufgabe: Steuerung des modifizierten RT-Algorithmus                                        */
/* Parameter: t ... der zu berechnende Baum                                                   */
/**********************************************************************************************/
{
  Extreme rm,lm;
  int lev;

  if (t != nil)
    {
      lev = 0;
       
      setup (t,lev,&rm,&lm,true);
      petrify (t,0);
    }
}
