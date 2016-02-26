/* (C) Universitaet Passau 1986-1991 */
/*1990-91 von Stefan Jockenhoevel and Gerd Nienhaus.*/

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE Utilities.c                                                **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

#include "paths.h"
#include STDH
#include SGRAPHH
#include SLISTH
#include GRAPHEDH
#include "NodeEdgeAttrs.h" 
#include "Utilities.h"
#include "ConvexTest.h"


/***************************************************/
/*                                                 */
/*               GetTargetNode                     */
/*                                                 */
/***************************************************/


Snode  GetTargetNode(TheEdgelistElem)
Slist  TheEdgelistElem;

/*******************************************************************************/
/* Die Funktion bestimmt, anhand des Attributs 'EdgeOfSourceList', den         */
/* Zielknoten der in 'TheEdgeListElem' abgespeicherten Kante. Dies wird        */
/* benoetigt, da in der 'AdjacencyList' Kanten aus der 'Sourcelist' und aus    */
/* der 'Targetlist' abgespeichert sind.                                        */
/* Die Funktion arbeitet auf 'Slist's' mit Attributen vom Typ                  */
/* 'MyEdgeListAttrs'.                                                          */
/*******************************************************************************/

  {
    if (EdgeOfSourceList(TheEdgelistElem))
             return(EdgePointer(TheEdgelistElem)->tnode);
        else return(EdgePointer(TheEdgelistElem)->snode);
  }/*end of GetTargetNode*/
 
  
/***************************************************/
/*                                                 */
/*               GetSourceNode                     */
/*                                                 */
/***************************************************/


Snode  GetSourceNode(TheEdgelistElem)
Slist  TheEdgelistElem;

/*******************************************************************************/
/* Die Funktion bestimmt, anhand des Attributs 'EdgeOfSourceList', den         */
/* Quellknoten der in 'TheEdgeListElem' abgespeicherten Kante. Dies wird       */
/* benoetigt, da in der 'AdjacencyList' Kanten aus der 'Sourcelist' und aus    */
/* der 'Targetlist' abgespeichert sind.                                        */
/* Die Funktion arbeitet auf 'Slist's' mit Attributen vom Typ                  */
/* 'MyEdgeListAttrs'.                                                          */
/*******************************************************************************/

  {
    if (EdgeOfSourceList(TheEdgelistElem))
             return(EdgePointer(TheEdgelistElem)->snode);
        else return(EdgePointer(TheEdgelistElem)->tnode);
  }/*end of GetSourceNode*/


/***************************************************/
/*                                                 */
/*               TargetNodeGet                     */
/*                                                 */
/***************************************************/


Snode  TargetNodeGet(TheEdgelistElem)
Slist  TheEdgelistElem;

/*******************************************************************************/
/* Die Funktion bestimmt, anhand des Attributs 'EdgeOfTargetList', den         */
/* Zielknoten der in 'TheEdgeListElem' abgespeicherten Kante. Dies wird        */
/* benoetigt, da in der 'OrderedEdgeList' Kanten aus der 'Sourcelist' und aus  */
/* der 'Targetlist' abgespeichert sind.                                        */
/* Die Funktion arbeitet auf 'Slist's' mit Attributen vom Typ                  */
/* 'OrderedEdgeListAttrs'.                                                          */
/*******************************************************************************/

  {
    if (EdgeOfTargetList(TheEdgelistElem))
             return(Edge(TheEdgelistElem)->snode);
        else return(Edge(TheEdgelistElem)->tnode);
  }/*end of TargetNodeGet*/
 
  
/***************************************************/
/*                                                 */
/*               SourceNodeGet                     */
/*                                                 */
/***************************************************/


Snode  SourceNodeGet(TheEdgelistElem)
Slist  TheEdgelistElem;

/*******************************************************************************/
/* Die Funktion bestimmt, anhand des Attributs 'EdgeOfTargetList', den         */
/* Quellknoten der in 'TheEdgeListElem' abgespeicherten Kante. Dies wird       */
/* benoetigt, da in der 'OrderedEdgeList' Kanten aus der 'Sourcelist' und aus  */
/* der 'Targetlist' abgespeichert sind.                                        */
/* Die Funktion arbeitet auf 'Slist's' mit Attributen vom Typ                  */
/* 'OrderedEdgeListAttrs'.                                                          */
/*******************************************************************************/

  {
    if (EdgeOfTargetList(TheEdgelistElem))
             return(Edge(TheEdgelistElem)->tnode);
        else return(Edge(TheEdgelistElem)->snode);
  }/*end of SourceNodeGet */
  
  
/***************************************************/
/*                                                 */
/*              AppendEdgeStack                    */
/*                                                 */
/***************************************************/


void AppendEdgeStack(TheStack,NewEdge)
EdgeStack  *TheStack;
Slist      NewEdge;

/*******************************************************************************/
/* Die Prozedur legt in der Variablen 'TheStack' einen Stack von 'Slist'-      */
/* Elementen an.                                                               */
/*******************************************************************************/ 
  {
    EdgeStack  Help;
    
    Help = talloc(struct edgestack);
    Help->edgelistelem = NewEdge;
    Help->suc = (*TheStack);
    (*TheStack) = Help;
  }/* end of AppendEdgeStack*/ 

 
/***************************************************/
/*                                                 */
/*              AppendSedgeStack                    */
/*                                                 */
/***************************************************/


void AppendSedgeStack(TheStack,NewEdge)
SedgeStack  *TheStack;
Sedge      NewEdge;

/*******************************************************************************/
/* Die Prozedur legt in der Variablen 'TheStack' einen Stack von 'Sedge'-      */
/* Elementen an.                                                               */
/*******************************************************************************/ 

  {
    SedgeStack  Help;
    
    Help = talloc(struct sedgestack);
    Help->edge = NewEdge;
    Help->suc = (*TheStack);
    (*TheStack) = Help;
  }/* end of AppendSedgeStack*/ 
 
  
/***************************************************/
/*                                                 */
/*              FindNode                           */
/*                                                 */
/***************************************************/


Snode FindNode(TheGraph,IdentAttr,IdentNr)
Sgraph TheGraph;
AttrType  IdentAttr;
int    IdentNr;

/*******************************************************************************/
/* Die Funktion sucht den Knoten mit der Identifikationsnummer 'IdentNr'im     */
/* Graphen 'TheGraph'. Als Identifikationsattribut kann entweder NEWNUM oder   */
/* NUMBER gewaehlt werden. 'IdentNr' bezieht sich auf 'IdentAttr'.             */
/* Der Zeiger auf den gefundenen Knoten wird zurueckgegeben.                   */                                                
/*******************************************************************************/   

  {
    Snode  ActNode;
    
    for_all_nodes(TheGraph,ActNode)
       switch(IdentAttr){
       case NEWNUM : if (NewNum(ActNode) == IdentNr)
                              return(ActNode);
                     break;
       case NUMBER : if (Number(ActNode) == IdentNr)
                              return(ActNode);
                     break;                                
       }/*switch*/                       
    end_for_all_nodes(TheGraph,ActNode); 
    return((Snode)NULL);
  }/*end of FindNode*/             
 
 



/***************************************************/
/*                                                 */
/*          FindNodeWithMinXKoor                   */
/*                                                 */
/***************************************************/

Snode FindNodeWithMinXKoor(TheGraph)
Sgraph TheGraph;

/*******************************************************************************/
/* Die Funktion bestimmt den Knoten mit minimaler x-Koordinate im Graphen      */
/* 'TheGraph'.                                                                 */
/*******************************************************************************/

  {
    Snode  ActNode;
    int    minkoor = 10000;
    Snode  MinNode = empty_snode;
    
    for_all_nodes(TheGraph,ActNode)
       if (snode_x(ActNode) < minkoor)
               {
                 minkoor = snode_x(ActNode);
                 MinNode = ActNode;
               }
    end_for_all_nodes(TheGraph,ActNode); 
    return(MinNode);
  } 
   
/***************************************************/
/*                                                 */
/*              FindFacialCycle                    */
/*                                                 */
/***************************************************/

Slist FindFacialCycle(TheGraph)
Sgraph  TheGraph;

/*******************************************************************************/
/* Die Funktion sucht eine Flaechenbegrenzung im Graphen 'TheGraph'. Um eine   */
/* Flaechenbegrenzung mit moeglichst vielen Knoten zu finden, wird die Suche   */
/* am Knoten mit niedrigster x-Koordinate gestartet.                           */
/*******************************************************************************/

  {
    Slist  ActEdgeListElem;
    Slist  TheFacialCycle1 = empty_slist,TheFacialCycle2 = empty_slist;
    Snode  StartNode;
    int    counter1,counter2;
    
    StartNode = FindNodeWithMinXKoor(TheGraph);  /* Bestimmung des Knotens mit minimaler x-Koordinate. */
    ActEdgeListElem = OrderedEdgeList(StartNode); /* Festlegen der Startkante. */
    
    /* Suchen einer Flaechenbegrenzung in "suc-"Richtung. */
    
    counter1 = 0;
    while(TargetNodeGet(ActEdgeListElem) != StartNode)
       {
         counter1++;
         if (TheFacialCycle1 == empty_slist)
                  TheFacialCycle1 = new_slist(ActEdgeListElem->attrs);
            else add_immediately_to_slist(TheFacialCycle1,ActEdgeListElem->attrs);
         ActEdgeListElem = EdgeCopy(ActEdgeListElem)->suc;

       }/* while */
    add_immediately_to_slist(TheFacialCycle1,ActEdgeListElem->attrs); 
    
    ActEdgeListElem = OrderedEdgeList(StartNode);
    
    /* Suchen einer Flaechenbegrenzung in "pre-"Richtung. */
    
    counter2 = 0;
    while(TargetNodeGet(ActEdgeListElem) != StartNode)
       {
         counter2++;
         if (TheFacialCycle2 == empty_slist)
                  TheFacialCycle2 = new_slist(ActEdgeListElem->attrs);
            else add_immediately_to_slist(TheFacialCycle2,ActEdgeListElem->attrs);
         ActEdgeListElem = EdgeCopy(ActEdgeListElem)->pre;

       }/* while */
    add_immediately_to_slist(TheFacialCycle2,ActEdgeListElem->attrs);
    
    if (counter1 > counter2)
           {
             free_slist(TheFacialCycle2);
             return(TheFacialCycle1);
           }
       else{
             free_slist(TheFacialCycle1);
             return(TheFacialCycle2);
           }       
  }/* end of FindFacialCycle */         


  
/***************************************************/
/*                                                 */
/*             DrawNodeBetweenAB                   */
/*                                                 */
/***************************************************/
              
void DrawNodeBetweenAB(TheNode,A,B,lambda)
Snode   TheNode;
Point   A,B;
double  lambda;

/*******************************************************************************/
/* Die Prozedur setzt den Knoten 'TheNode' auf die Strecke zwischen A und B    */
/* mit Abstand lambda*|B-A| von A.                                             */
/*******************************************************************************/

  {
    snode_x(TheNode) =  A.x + (int)(lambda * (B.x - A.x));
    snode_y(TheNode) =  A.y + (int)(lambda * (B.y - A.y));
    
  }/* end of DrawNodeBetweenAB */          


  
