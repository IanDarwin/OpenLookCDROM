/* (C) Universitaet Passau 1986-1991 */
/* 1990-91 von Stefan Jockenhoevel und Gerd Nienhaus.*/
/* Stand : 23.02.1991 */

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE PossibleConvexDraw.c                                       **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

#include "paths.h"
#include "std.h"
#include "sgraph.h"
#include "slist.h"
#include "graphed.h"
#include "PossibleConvexDraw.h"
#include "NodeEdgeAttrs.h"
#include "Utilities.h"    

/********************************************************************************/
/*                                                                              */
/*      Dieses Modul ueberprueft Grundvoraussetzungen, ob ein Graph convex      */
/*      gezeichnet werden kann oder nicht.                                      */
/*      Der Graph wird als planar und zweifachzusammenhangend vorausgestetzt.   */
/*       Ueberprueft wird :                                                     */
/*      a) Existiert ueberhaupt ein Graph ?                                     */
/*      b) Hat der Graph mindestens eine Mehrfachkante ?                        */
/*      c) Hat der Graph eine Kante von einem Knoten auf sich selbst ?          */
/*                                                                              */
/********************************************************************************/

int NumberOfEdges(TheGraph)
  /* bestimmt die Anzahl der Kanten von TheGraph */
Sgraph TheGraph;

  {
   Snode TheNode;
   Sedge TheEdge;
   int   numberofedges = 0 ;
   
   for_all_nodes(TheGraph,TheNode)     /* zaehlen der Kanten */
      for_sourcelist(TheNode,TheEdge)
         numberofedges++;
      end_for_sourcelist(TheNode,TheEdge);
   end_for_all_nodes(TheGraph,TheNode);

   if (TheGraph->directed)
        return numberofedges; 
   else return (numberofedges/2); 
   
  }  /* end of NumberOfEdges */  
  
/********************************************************************************/

int NumberOfNodes(TheGraph)
  /* bestimmt die Anzahl der Knoten von TheGraph */
Sgraph TheGraph;

  {
   Snode TheNode;
   int   numberofnodes = 0;
   
   for_all_nodes(TheGraph,TheNode)   /* zaehlen der Knoten */
     numberofnodes++;
   end_for_all_nodes(TheGraph,TheNode);

   return numberofnodes;
   
  }  /* end of NumberOfNodes */  
  
/********************************************************************************/

bool contains_element(TheEdgeList,ActEdge)
  /*
   * gibt TRUE zurueck, wenn ActEdge in TheEdgeList ist 
   * gibt FALSE zurueck, wenn ActEdge nicht in TheEdgeList ist 
   */
Slist TheEdgeList;
Sedge ActEdge;

 {
  Slist ActEdgeListElem;
  bool  ok = FALSE;
  
  for_slist(TheEdgeList,ActEdgeListElem)
    if (((EDGEpointer(ActEdgeListElem)->snode == ActEdge->snode) && 
         (EDGEpointer(ActEdgeListElem)->tnode == ActEdge->tnode)) || 
        ((EDGEpointer(ActEdgeListElem)->snode == ActEdge->tnode) && 
         (EDGEpointer(ActEdgeListElem)->tnode == ActEdge->snode)))
        {
         ok = TRUE;
         break;
        }
  end_for_slist(TheEdgeList,ActEdgeListElem);
  
  return ok;
  
 }  /*end of contains_element*/

/********************************************************************************/

int NoEdgeOnNode(TheGraph)
  /* prueft, ob eine Kante von einem Knoten auf sich selbst zeigt */
Sgraph TheGraph;

  {
   Snode TheNode;
   Sedge TheEdge;
   bool  ok = TRUE;
   
   for_all_nodes(TheGraph,TheNode)
     for_sourcelist(TheNode,TheEdge)
       if (TheEdge->tnode == TheEdge->snode)
          {
           ok = FALSE;     /* es wurde eine Kante von einem Knoten auf sich selbst gefunden */
           break;
          }
      end_for_sourcelist(TheNode,TheEdge);

      if (!ok) break;
    end_for_all_nodes(TheGraph,TheNode);

    return ok;
   
  }  /* end of NoEdgeOnNode */

/****************************************************************************/

bool NoMultipleEdges(TheGraph)
  /*
   *  Die Funktion prueft, ob eine Mehrfachkante in TheGraph vorhanden ist.
   *  Bei einem gerichtetem Graphen wird jede Kante mit einer Sliste der schon
   *  betrachteten Kanten vergliechen. Wenn eine Kante zum zweiten Mal auf-
   *  tritt, wird die Funktion abgebrochen und FALSE zurueckgegeben, ansonsten
   *  wird die Kante in die Sliste eingetragen. Wenn keine Mehrfachkante 
   *  gefunden wird, wird TRUE zurueckgegeben.
   *  Im ungerichteten Fall wird mit 2 Slisten gearbeitet und beim dritten auf-
   *  treten einer Kante abgebrochen.
   */
Sgraph TheGraph;

  {
   Snode TheNode;
   Sedge TheEdge;
   Slist TheEdgeList = empty_slist,TheUndirEdgeList = empty_slist;
   bool  ok = TRUE,contains;
   
   if (TheGraph->directed)      /* gerichteter Graph */
       {
        for_all_nodes(TheGraph,TheNode)
         for_sourcelist(TheNode,TheEdge)
           if (TheEdgeList == empty_slist)
                TheEdgeList = new_slist(make_attr(ATTR_DATA, (char *)TheEdge));
           else{ 
                if (contains_element(TheEdgeList,TheEdge))
                    {
                     ok = FALSE;
                     break;
                    }
                add_immediately_to_slist(TheEdgeList,make_attr(ATTR_DATA, (char *)TheEdge));
               } /*else*/
          end_for_sourcelist(TheNode,TheEdge);
 
          if (!ok) break;
         end_for_all_nodes(TheGraph,TheNode);
        } /*then*/
    else                      /* ungerichteter Graph */   
       {
        for_all_nodes(TheGraph,TheNode)
         for_sourcelist(TheNode,TheEdge)
           if (TheEdgeList == empty_slist)
                TheEdgeList = new_slist(make_attr(ATTR_DATA, (char *)TheEdge));
           else{ 
                if (TheUndirEdgeList == empty_slist)
                     TheUndirEdgeList = new_slist(make_attr(ATTR_DATA, (char *)TheEdge));
                else{
                     contains = contains_element(TheEdgeList,TheEdge);
                     if ((contains) && (contains_element(TheUndirEdgeList,TheEdge)))
                         {
                          ok = FALSE;   /* es wurde eine Mehrfachkante gefunden */
                          break;
                         }
                     if (!(contains))
                          add_immediately_to_slist(TheEdgeList,make_attr(ATTR_DATA, (char *)TheEdge));
                     else add_immediately_to_slist(TheUndirEdgeList,make_attr(ATTR_DATA, (char *)TheEdge));
                    } /*else*/
                } /*else*/
         end_for_sourcelist(TheNode,TheEdge);
 
         if (!ok) break;
        end_for_all_nodes(TheGraph,TheNode);
        free_slist(TheUndirEdgeList);
       } /*else*/

   free_slist(TheEdgeList);
   return ok;
   
  }  /* end of NoMultipleEdges */

/********************************************************************************/

bool DrawConvexPossible(TheGraph)
    /*
     * Die Funktion gibt FALSE zurueck, wenn nichts mehr zu berechnen ist, d.h.
     * enweder kann der Graph nicht convex gezeichnet werden oder er hat weniger
     * als 4 Knoten und wird direkt ausgewertet.
     * Bei der Rueckgabe von TRUE hat TheGraph einen Vortest ueberstanden,
     * muss aber noch weiter bearbeitet werden.
     */
Sgraph TheGraph;
 
   {
     int   V;                  /* Anzahl der Knoten von TheGraph */
     int   E;                  /* Anzahl der Kanten von TheGraph */
     bool  noEdgeonNode;

     if (TheGraph->nodes == EmptySnode)
            {
             warning ("There is no graph.\n");
             return FALSE;
            }
        else
            {
              V = NumberOfNodes(TheGraph);
              E = NumberOfEdges(TheGraph);
              message("  \n");
              message(" number of nodes : %d \n",V);
              message(" number of edges : %d \n",E);
              message("  \n");
              
              switch(V)
               {
                case 1: {
                         if (NoEdgeOnNode(TheGraph)) 
                              message("%s\n",OK1);
                         else message("%s\n%s\n",NOK,TYP2a);
                         return FALSE;
                         break;
                        };
                case 2: {
                         noEdgeonNode = NoEdgeOnNode(TheGraph);
                         if (noEdgeonNode && NoMultipleEdges(TheGraph))
                               message("%s\n",OK1);
                         else  if(!(noEdgeonNode))
                                   message("%s\n%s\n",NOK,TYP2a); 
                               else message("%s\n%s\n",NOK,TYP1);
                         return FALSE;
                         break;
                        }; 
                case 3: {
                         noEdgeonNode = NoEdgeOnNode(TheGraph);
                         if (noEdgeonNode && NoMultipleEdges(TheGraph))
                              message("%s\n",OK1);
                         else  if(!(noEdgeonNode))
                                   message("%s\n%s\n",NOK,TYP2a); 
                               else message("%s\n%s\n",NOK,TYP1);
                         return FALSE;
                         break;
                         }; 
                default: {
                          noEdgeonNode = NoEdgeOnNode(TheGraph);
                          if (noEdgeonNode && NoMultipleEdges(TheGraph))
                              {
                               message("%s\n",OK2);
                               return TRUE;
                               break;
                              }
                          else  if(!(noEdgeonNode))
                                     message("%s\n%s\n",NOK,TYP2a); 
                                else message("%s\n%s\n",NOK,TYP1);
                          return FALSE;
                          break;
                         }; 

               };   /* switch */
         };         /* else */         
  }                 /* end of PossibleConvexDraw */          

/********************************************************************************/
