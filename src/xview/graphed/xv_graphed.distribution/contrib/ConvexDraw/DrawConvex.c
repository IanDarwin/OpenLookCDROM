/* (C) Universitaet Passau 1986-1991 */
/*1990-91 by Stefan Jockenhoevel and Gerd Nienhaus.*/

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE DrawConvex.c                                               **/
/**                                                                                     **/
/** Das Modul implementiert die im folgenden Artikel beschriebenen Funktionen und       **/
/** Prozeduren:                                                                         **/
/**                   N.Chiba, K.Onoguchi und T.Nishizeki:                              **/
/**                                                                                     **/
/**                       Drawing Plane Graphs Nicely                                   **/
/**                                                                                     **/
/**            (erschienen in : Acta Informatica 22, S.187 - 201,1985)                  **/
/**                                                                                     **/
/** Dieses Modul bezieht sich auf den Abschnitt 3 des Artikels:                         **/
/**                       'Convex Drawing Algorithm' [CDA]                              **/
/**                                                                                     **/
/** Exportiert wird die Prozedur:                                                       **/
/**   ExtendFacialCycle(Sgraph  TheGraph,Slist TheFacialCycle,bool  edit)               **/
/*****************************************************************************************/
/*****************************************************************************************/

#include <math.h>

#include "paths.h"
#include STDH
#include SGRAPHH
#include SLISTH
#include GRAPHEDH
#include "NodeEdgeAttrs.h" 
#include "Utilities.h"
#include "DrawConvex.h"
#include "ConvexTest.h"
    
/*****************************************************************************************/



/***************************************************/
/*                                                 */
/*               CheckEdgeline                     */
/*                                                 */
/***************************************************/

static void CheckEdgeline(TheEdgeLine)
  /* eventuelle 'non straightline' Kanten werden begradigt */
Edgeline TheEdgeLine;

 {
  if (!(is_single_edgeline(TheEdgeLine)))                 /* die Kante ist nicht gerade */
       { 
        remove_from_edgeline(edgeline_suc(TheEdgeLine));  /* (x,y)-Koordinaten eines "Knickes" werden entfernt */
        CheckEdgeline(TheEdgeLine);
       }

 } /*end of CheckEdgeline*/



/***************************************************/
/*                                                 */
/*         DrawFacialCycleConvex                   */
/*                                                 */
/***************************************************/

static void DrawFacialCycleConvex(TheGraph,FacialCycle,edit)
Sgraph TheGraph;
Slist  FacialCycle;
bool   edit;

  /**************************************************/ 
  /*  Der Umkreis des Graphen wird neu gezeichnet.  */
  /*  Dazu wird die n-te Einheitswurzel berechnet,  */
  /*  wobei n gleich der Anzahl der Knoten ist,     */
  /*  welche auf dem Umkreis liegen. Dann werden    */
  /*  die (x,y)-Koordinaten mit einem Radius multi- */
  /*  pliziert, wobei der Radius gleich dem Max aus */
  /*  [(Xmax - Xmin)/2,(Ymax - Ymin)/2,250] ist.    */
  /*  Jetzt wird der Mittelpunkt noch aus (0,0)     */
  /*  nach (2000,2000) verschoben.                  */
  /**************************************************/ 

 {
  Slist    ActElem;
  Snode    ActNode;
  Sedge    ActEdge;
  Edgeline TheEdgeLine;
  double   radius,angle,X,Y,x,y;
  int      numberofnodes=0,k,Xmax=0,Xmin=4000,Ymax=0,Ymin=4000;
  double   pi = 3.14159265;

  for_all_nodes(TheGraph,ActNode)
   if (Xmax < snode_x(ActNode)) Xmax = snode_x(ActNode);
   if (Xmin > snode_x(ActNode)) Xmin = snode_x(ActNode);
   if (Ymax < snode_y(ActNode)) Ymax = snode_y(ActNode);
   if (Ymin > snode_y(ActNode)) Ymin = snode_y(ActNode);
   for_sourcelist(ActNode,ActEdge)
    if (graphed_edge(ActEdge) != (Graphed_edge)NULL)
           {
             TheEdgeLine = (Edgeline)edge_get(graphed_edge(ActEdge),EDGE_LINE);
             CheckEdgeline(TheEdgeLine);
             if (!edit)
                 edge_set(graphed_edge(ActEdge),ONLY_SET,EDGE_LABEL_VISIBILITY,FALSE,EDGE_ARROW_LENGTH,1,0);
           } 
      
   end_for_sourcelist(ActNode,ActEdge);
 end_for_all_nodes(TheGraph,ActNode);
  
  for_slist(FacialCycle,ActElem)
    numberofnodes++;                              /* Knotenzahl berechnen*/
  end_for_slist(FacialCycle,ActElem);
  
  X = (double)((Xmax-Xmin)/2);                    /*  Radius   */
  Y = (double)((Ymax-Ymin)/2);                    /* berechnen */
  if ((X > Y) && (X > 200.0))                     /*   bzw.    */
       radius = X;                                /* bestimmen */
     else if (Y > 200.0)
              radius = Y;
           else radius = 200.0;
                          
  X = 2000.0;                                     /* Mittelpunkt */
  Y = 2000.0;                                     /*  festlegen  */

  angle=2.0 * pi / (double)numberofnodes;         /* Winkel berechnen */
  k = numberofnodes; 
   
  for_slist(FacialCycle,ActElem)
   x=cos((double)k*angle);
   y=sin((double)k*angle);
   snode_x(SourceNodeGet(ActElem)) = (int)(x * radius + X);       /*       neue (x,y)      */
   snode_y(SourceNodeGet(ActElem)) = (int)(-y * radius + Y);      /* Koordinaten berechnen */
   k--;
   Drawn(SourceNodeGet(ActElem)) = 1;
   Apex(SourceNodeGet(ActElem)) = TRUE;
   Traversed(ActElem) = TRUE;
   Traversed(EdgeCopy(ActElem)) = TRUE;
  end_for_slist(FacialCycle,ActElem);

 } /*end of DrawFacialCycleConvex*/


/***************************************************/
/*                                                 */
/*           ComputeAreaOfTriangle                 */
/*                                                 */
/***************************************************/
 
 
static double ComputeAreaOfTriangle(M,A,B)
Snode    M,A,B;

/*******************************************************************************/
/* Die Flaech des Dreiecks (M,A,B) wird mit Hilfe der Determinante             */
/* |A-M|B-M| berechnet.                                                        */
/*******************************************************************************/

  {
    return(0.5 * (double) abs( (snode_x(A)-snode_x(M))*(snode_y(B)-snode_y(M)) - (snode_y(A)-snode_y(M))*(snode_x(B)-snode_x(M)) ) );
  }/* end of ComputeAreaOfTriangle */  


/***************************************************/
/*                                                 */
/*           ComputeAreaOfBlock                    */
/*                                                 */
/***************************************************/

static double ComputeAreaOfBlock(V,FirstNode,TheCycle)
Snode      V,FirstNode;
Slist      TheCycle;

/*******************************************************************************/
/* Die fuer den Block, der durch 'TheCycle' gegeben ist,zur Verfuegung stehende*/
/* Flaeche wird berechnet. Dies ist die Flaeche, die durch den Ausschnitt des  */
/* umgebenden Blocks und den Kanten (V,FirstNode) sowie                        */
/* (V,SourceNodeGet(TheCycle)) begrenzt ist. DerAusschnitt des umgebenden      */
/* Blocks sind genau die Kanten in 'TheCycle', die schon zum, in der Rekursion,*/
/* vorherigen Block gehoerten.                                                 */
/*******************************************************************************/

  {
    Slist    ActElem,StartEdge;
    double      Area = 0.0;
    
    if (TheCycle != TheCycle->suc)
           {
             for_slist(TheCycle,ActElem)
                if (SourceNodeGet(ActElem) == FirstNode)
                       {
                         StartEdge = ActElem;
                         break;
                       }
             end_for_slist(TheCycle,ActElem);
             
             while(StartEdge != TheCycle)
                {
                  Area = Area + ComputeAreaOfTriangle(V,SourceNodeGet(StartEdge),TargetNodeGet(StartEdge));
                  StartEdge = StartEdge->suc;
                }
             return(Area);                 
           }
      else return(0.0);
  }/* end of ComputeAreaOfBlock */
  
             
/***************************************************/
/*                                                 */
/*                 ComputeWeight                   */
/*                                                 */
/***************************************************/

static double ComputeWeight(areaofblock,weightoffaces,TheCycle,V,LastNode,apexcounter)
double     areaofblock,weightoffaces;
Slist      TheCycle;
Snode      V,LastNode;
int        apexcounter;

  {
    Snode     V1;
    Slist     ActEdge;
    int       i;
    double    Area = 0.0;
    Point     HV;
    void      DrawEntryConvex();
    
    if (TheCycle != TheCycle->suc)
           {
             for(i = 1;i < 10;i++)
                {
                  Area = 0.0; 
                  HV.x = snode_x(V);
                  HV.y = snode_y(V);
                  DrawEntryConvex(TheCycle,HV,SourceNodeGet(TheCycle),LastNode,((double)i/10.0),apexcounter);
                  
                  V1 = SourceNodeGet(TheCycle);
                  ActEdge = TheCycle;
                  while(SourceNodeGet(ActEdge->suc) != LastNode)
                    {
                      while(!Apex(SourceNodeGet(ActEdge->suc)) &&  SourceNodeGet(ActEdge->suc) != LastNode)
                         ActEdge = ActEdge->suc;
                      
                      if (Apex(SourceNodeGet(ActEdge->suc)) &&  SourceNodeGet(ActEdge->suc) != LastNode)
                             {
                               Area = Area + ComputeAreaOfTriangle(V,V1,SourceNodeGet(ActEdge->suc));
                               V1 = SourceNodeGet(ActEdge->suc);
                               ActEdge = ActEdge->suc;
                             }
                         else  if (SourceNodeGet(ActEdge->suc) != LastNode)
                                       ActEdge = ActEdge->suc;         
                    }/*while*/
                  Area = Area + ComputeAreaOfTriangle(V,V1,LastNode);
                  
                  if (Area < (weightoffaces*areaofblock))
                          return((double)(i/10.0));
                }/*for*/
             return(0.9);   
           }/*then*/
  }/*end of ComputeWeight */           

 
 

                     



 
/***************************************************/
/*                                                 */
/*            Intersection                         */
/*                                                 */
/***************************************************/

static Point Intersection(b1,e1,b2,e2)
  /* Es wird der Schnitt der Geraden (b1,e1) und (b2,e2) berechnet */
Point b1,e1,b2,e2;

/*******************************************************************************/
/* Es kann hier der Fall auftreten, dass die sich die beiden Geraden           */
/* ueberlagern. Dieser Fall tritt dann auf ,wenn die "Groesse" des neuen       */
/* Graphen nicht ausreicht, um alle Flaechen korrekt zu zeichnen. Um den       */
/* Absturz des Programms zu veryhindern, wird ein Endpunkt einer der beiden    */
/* Geraden als Schnittpunkt zurueckgegeben. Ausserdem wird eine Fehlermeldung  */
/* generiert.(siehe auch 'ExtendFacialCycle...' unten)                         */
/*******************************************************************************/
  
 {
  double    deltax1,deltax2,deltay1,deltay2;
  Point  help_point ;
  double    change;
  
  
  deltax1 = (double)(e1.x - b1.x);
  deltax2 = (double)(e2.x - b2.x);
  deltay1 = (double)(e1.y - b1.y);
  deltay2 = (double)(e2.y - b2.y);
  
  if (deltax1*deltay2 == deltax2*deltay1)
      {
        return(e1);
      }  
  
  if (deltax1 == 0.0)        /* um die Division durch 0 zu verhindern werden die Geraden getauscht */
       {
        help_point = b1;
        b1 = b2;
        b2 = help_point;
        help_point = e1;
        e1 = e2;
        e2 = help_point;
        change = deltax1;
        deltax1 = deltax2;
        deltax2 = change;
        change = deltay1;
        deltay1 = deltay2;
        deltay2 = change;
       }
       
  help_point.x = b2.x + (int)(deltax2 * (((double)(b1.y - b2.y) + (((double)(b2.x -b1.x))/deltax1) * deltay1)/(deltay2 - (deltax2/deltax1) * deltay1) ));
  help_point.y = b2.y + (int)(deltay2 * (((double)(b1.y - b2.y) + (((double)(b2.x -b1.x))/deltax1) * deltay1)/(deltay2 - (deltax2/deltax1) * deltay1)) );
  return(help_point);

 } /*end of Intersection*/
 
/***************************************************/
/*                                                 */
/*            New_v1_v2                            */
/*                                                 */
/***************************************************/

static Slist New_v1_v2(FacialCycle,v1,v2,new_v1,new_v2) 
  /* es werden die beiden naechsten zu zeichnenden Knoten berechnet */
Slist FacialCycle;
Snode v1,v2,*new_v1,*new_v2;

 {
  Slist ActElem;
  bool  next = FALSE,ok = TRUE;
  
  for_slist(FacialCycle,ActElem)
   if (ok && (SourceNodeGet(ActElem) == v1))
       { 
        next = TRUE;
        ok = FALSE;
       }
   if (next && (Apex(SourceNodeGet((ActElem)->suc))))
        {
         (*new_v1) = SourceNodeGet((ActElem)->suc); 
         next = FALSE;
        }
   if (Apex(SourceNodeGet(ActElem)))
        (*new_v2) = SourceNodeGet(ActElem); 
   if (SourceNodeGet((ActElem)->suc) == v2)
        break;
  end_for_slist(FacialCycle,ActElem);
  
 } /*end of New_v1_v2*/
 
 

 
 
/***************************************************/
/*                                                 */
/*              DrawUnconnectedNodes               */
/*                                                 */
/***************************************************/

static void DrawUnconnectedNodes(FacialCycle,v1,v2)
  /* die nicht mit v verbundenen Knoten werden auf die Kante (A,B) zweier mit v verbundener Knoten gezeichnet */
Slist FacialCycle;
Snode v1,v2;

 {
  int   numberofnodes = 0,nodecounter;
  Point A,B;
  Slist help_one,help_two;
   
   if (FacialCycle->suc != FacialCycle)
      {
        help_two = FacialCycle;
        help_one = help_two;
        while (SourceNodeGet(help_two) != v2)
             {
              while (!(Apex(SourceNodeGet(help_one->suc))) && (SourceNodeGet(help_one->suc) != v2))
                  {
                   numberofnodes++;
                   help_one = help_one->suc;
                  }  /*while*/
              help_one = help_one->suc;

              if (numberofnodes > 0)
                  {
                   nodecounter = numberofnodes + 1;
                   A.x = snode_x(SourceNodeGet(help_two));
                   A.y = snode_y(SourceNodeGet(help_two));
                   B.x = snode_x(SourceNodeGet(help_one));
                   B.y = snode_y(SourceNodeGet(help_one));
                   help_two = help_two->suc;
                   while(help_two != help_one)
                        {
                         DrawNodeBetweenAB(SourceNodeGet(help_two),A,B,(double)(nodecounter-numberofnodes)/(double)nodecounter);
                         help_two = help_two->suc;
                         numberofnodes--;
                        }  /*while*/
                  } /*then*/
              else
                  {
                   help_one = help_two->suc;
                   help_two = help_two->suc;
                  }  /*else*/
             }  /*while*/
      }/*then*/
 }  /*end for DrawUnconnectedNodes*/

   
/***************************************************/
/*                                                 */
/*               DrawEntryConvex                   */
/*                                                 */
/***************************************************/

static void DrawEntryConvex(FacialCycle,v,v1,v2,weight,numberofnodes)
  /* die Knoten der Sliste FacialCycle "zwischen" v1 und v2, die mit v verbunden sind, werden in das Dreieck (v,v1,v2) gezeichnet */
  /* ist numberofnodes = 0, so wird NICHTS gemacht */

Slist  FacialCycle;
Point  v;
Snode  v1,v2;
double weight;             /* Hoehe der Verschiebung */
int    numberofnodes ;     /* Anzahl der Knoten, die mit v verbunden sind */

 {
  int   x,y;
  Point right,left,A,B,C,D,new_v;
  Snode new_v1,new_v2;
  
  if (numberofnodes > 0)
       {
        x = abs(snode_x(v1) - snode_x(v2)) / (numberofnodes + 1);
        y = abs(snode_y(v1) - snode_y(v2)) / (numberofnodes + 1);
        if (snode_x(v1) <= snode_x(v2))
             right.x = snode_x(v2) - x;
        else right.x = snode_x(v2) + x;
        if (snode_y(v1) >= snode_y(v2))
             right.y = snode_y(v2) + y;
        else right.y = snode_y(v2) - y;
      
        New_v1_v2(FacialCycle,v1,v2,&new_v1,&new_v2);
        DrawNodeBetweenAB(new_v2,right,v,weight);
       
        if (numberofnodes >= 2)
              {
               if (snode_x(v1) <= snode_x(v2))
                    left.x = snode_x(v1) + x;
               else left.x = snode_x(v1) - x;
               if (snode_y(v1) >= snode_y(v2))
                    left.y = snode_y(v1) - y;
               else left.y = snode_y(v1) + y;
   
               DrawNodeBetweenAB(new_v1,left,v,weight);
              
               if (numberofnodes > 2)
                   {
                    A.x  = snode_x(new_v1);
                    A.y  = snode_y(new_v1);
                    B.x  = snode_x(new_v2);
                    B.y  = snode_y(new_v2);
                    C.x  = snode_x(v1);
                    C.y  = snode_y(v1);
                    D.x  = snode_x(v2);
                    D.y  = snode_y(v2);
                    new_v = Intersection(C,A,D,B);
                    DrawEntryConvex(FacialCycle,new_v,new_v1,new_v2,weight,numberofnodes - 2);
                   }
              }
        }
  
 } /*end of DrawEntryConvex*/
  

/***************************************************/
/*                                                 */
/*           EleminateNodesOfDegree2               */
/*                                                 */
/***************************************************/

static void EleminateNodesOfDegree2(TheGraph)
Sgraph  TheGraph;

/*******************************************************************************/
/* Alle Knoten vom Grad 2 im Graphen 'TheGraph', die nicht auf der aeusseren   */
/* Flaechenbegrenzung liegen, werden "eliminiert". D.h., sei A ein Knoten      */
/* vom Grad 2 und seien B bzw. C die mit A verbundenen Knoten, dann werden     */
/* die Kanten (A,B) sowie (A,C) durch die Kante (B,C) ersetzt.                 */
/* Die 'Sgraph'-Struktur wird dabei nicht veraendert.                          */
/*******************************************************************************/

  {
    Snode    ActNode;
    Sedge    NewEdge;
    
    for_all_nodes(TheGraph,ActNode)
       if ((Deg(ActNode) == 2) && (Drawn(ActNode) == 0))
              {
                NewEdge = talloc(struct sedge);
                NewEdge->snode = TargetNodeGet(OrderedEdgeList(ActNode));
                NewEdge->tnode = TargetNodeGet(OrderedEdgeList(ActNode)->suc);
                EdgeOfTargetList(EdgeCopy(OrderedEdgeList(ActNode))) = FALSE;
                EdgeOfTargetList(EdgeCopy(OrderedEdgeList(ActNode)->suc)) = TRUE;
                EdgeCopy(EdgeCopy(OrderedEdgeList(ActNode))) = EdgeCopy(OrderedEdgeList(ActNode)->suc);
                EdgeCopy(EdgeCopy(OrderedEdgeList(ActNode)->suc)) = EdgeCopy(OrderedEdgeList(ActNode));
                Edge(EdgeCopy(OrderedEdgeList(ActNode))) = NewEdge;
                Edge(EdgeCopy(OrderedEdgeList(ActNode)->suc)) = NewEdge;
              }
    end_for_all_nodes(TheGraph,ActNode);                       
  }/* end of EleminateNodesOfDegree2 */
  
 
 
/***************************************************/
/*                                                 */
/*           DrawDegree2Node                       */
/*                                                 */
/***************************************************/
 
static void DrawDegree2Node(TheNode)
Snode    TheNode;


  {
    Point     A,B;
    
    if (Drawn(TheNode) == 0)
           {
             if (Drawn(TargetNodeGet(OrderedEdgeList(TheNode))) == 0 && Drawn(TargetNodeGet(OrderedEdgeList(TheNode)->suc)) == 0)
                    {
                      DrawDegree2Node(TargetNodeGet(OrderedEdgeList(TheNode)));
                      DrawDegree2Node(TargetNodeGet(OrderedEdgeList(TheNode)->suc));
                    }
                else  if (Drawn(TargetNodeGet(OrderedEdgeList(TheNode))) == 0)
                              DrawDegree2Node(TargetNodeGet(OrderedEdgeList(TheNode)));
                         else if (Drawn(TargetNodeGet(OrderedEdgeList(TheNode)->suc)) == 0)
                                      DrawDegree2Node(TargetNodeGet(OrderedEdgeList(TheNode)->suc)); 
             A.x = snode_x(TargetNodeGet(OrderedEdgeList(TheNode)));                                         
             A.y = snode_y(TargetNodeGet(OrderedEdgeList(TheNode)));
             B.x = snode_x(TargetNodeGet(OrderedEdgeList(TheNode)->suc));                                         
             B.y = snode_y(TargetNodeGet(OrderedEdgeList(TheNode)->suc)); 
             DrawNodeBetweenAB(TheNode,A,B,0.5);
           }/* then */  
  }/*end of DrawDegree2Node */                                              
                             
                      
/***************************************************/
/*                                                 */
/*             DrawNodesOfDegree2                  */
/*                                                 */
/***************************************************/

static void DrawNodesOfDegree2(TheGraph)
Sgraph     TheGraph;

/*******************************************************************************/
/* Die Funktion 'DrawNodesOfDegree2' zeichnet die Knoten vom Grad 2, die mit   */
/* Hilfe der Funktion 'EleminateNodesOfDegree2' eleminiert wurden, auf die     */
/* Gerade zwischen den ,mit dem Knoten von Grad 2 verbundenen Knoten.          */
/* Da eventuell eine Kette von Knoten vom Grad 2 auftreten kann, wird die      */
/* rekursive Funktion 'DrawDegree2Node' verwendet, um den ersten               */
/* "zeichenbaren" Knoten vom Grad 2 zu finden. Zeichenbar heisst hier,         */
/* dass beide Nachbarknoten des zu zeichnenden Knotens schon gezeichnet sein   */
/* muessen.                                                                    */
/*******************************************************************************/

  {
    Snode    ActNode;
    
    for_all_nodes(TheGraph,ActNode)
       if (Deg(ActNode) == 2 && Drawn(ActNode) == 0)
              DrawDegree2Node(ActNode);
    end_for_all_nodes(TheGraph,ActNode);
  }/* end of DrawNodesOfDegree2 */            



/***************************************************/
/*                                                 */
/*              FindBlockCyclePre                  */
/*                                                 */
/***************************************************/

static Slist FindBlockCyclePre(FirstEdge,LastEdge,nr,apexcounter)
Slist   FirstEdge;
Slist   *LastEdge;
int     nr,*apexcounter;

  {
    Slist  FacialEdge,TheBlockCycle = empty_slist;
    
    (*apexcounter) = 0;
    FacialEdge = FirstEdge;
    
    while( Drawn(TargetNodeGet(FacialEdge)) == 0)
       {
         /* Der "neue" Teil des Blockumkreises wird aufgebaut. */
         
         if (TheBlockCycle == empty_slist)
                 TheBlockCycle = new_slist(FacialEdge->attrs);
            else add_to_slist(TheBlockCycle,FacialEdge->attrs);
            
         if (!Removed(TargetNodeGet(EdgeCopy(FacialEdge)->pre)))
                 FacialEdge = EdgeCopy(FacialEdge)->pre;   /* Die mit dem "geloeschten" Knoten verbundenen Kanten werden nicht beachtet. */
            else{
                  FacialEdge = EdgeCopy(FacialEdge)->pre->pre;
                  Apex(SourceNodeGet(FacialEdge)) = TRUE;
                  (*apexcounter)++;
                 } 
       }/* while */
       (*LastEdge) = EdgeCopy(FacialEdge);
       if (!Traversed(FacialEdge))
              {
                if (TheBlockCycle == empty_slist)
                        TheBlockCycle = new_slist(FacialEdge->attrs);
                   else add_to_slist(TheBlockCycle,FacialEdge->attrs);
                
                /* Schliessen des Blockumkreises mit Hilfe des entsprechenden Aussschnittes,des */
                /* in der Rekursion, umgebenden Blockes.                                        */
                   
                FacialEdge = EdgeCopy(FacialEdge)->suc;
                while(!Traversed(FacialEdge))
                   FacialEdge = FacialEdge->suc;
                while(TargetNodeGet(FacialEdge) != SourceNodeGet(FirstEdge))
                  {
                    add_to_slist(TheBlockCycle,FacialEdge->attrs);
                    FacialEdge = EdgeCopy(FacialEdge)->suc;
                    while(!Traversed(FacialEdge))
                            FacialEdge = FacialEdge->suc;     
                    
                  }/*while*/
                add_to_slist(TheBlockCycle,FacialEdge->attrs);
                return(TheBlockCycle);
              }/*then*/
          else  return(empty_slist);
  }/* end of FindBlockCyclePre */                
                         
                
       
/***************************************************/
/*                                                 */
/*              FindBlockCycleSuc                  */
/*                                                 */
/***************************************************/

static Slist FindBlockCycleSuc(FirstEdge,LastEdge,nr,apexcounter)
Slist   FirstEdge;
Slist   *LastEdge;
int     nr,*apexcounter;

  {
    Slist  FacialEdge,TheBlockCycle = empty_slist;
    
    
    (*apexcounter) = 0;
    FacialEdge = FirstEdge;
    
    while( Drawn(TargetNodeGet(FacialEdge)) == 0)
       {
         /* Der "neue" Teil des Blockumkreises wird aufgebaut. */
         
         if (TheBlockCycle == empty_slist)
                 TheBlockCycle = new_slist(FacialEdge->attrs);
            else add_to_slist(TheBlockCycle,FacialEdge->attrs);
            
         if (!Removed(TargetNodeGet(EdgeCopy(FacialEdge)->suc)))
                 FacialEdge = EdgeCopy(FacialEdge)->suc;   /* Die mit dem "geloeschten" Knoten verbundenen Kanten werden nicht beachtet. */
            else{
                  FacialEdge = EdgeCopy(FacialEdge)->suc->suc;
                  Apex(SourceNodeGet(FacialEdge)) = TRUE;
                  (*apexcounter)++;
                }  
       }/* while */
       (*LastEdge) = EdgeCopy(FacialEdge);
       if (!Traversed(FacialEdge))
              {
                if (TheBlockCycle == empty_slist)
                        TheBlockCycle = new_slist(FacialEdge->attrs);
                   else add_to_slist(TheBlockCycle,FacialEdge->attrs);
                   
                /* Schliessen des Blockumkreises mit Hilfe des entsprechenden Aussschnittes,des */
                /* in der Rekursion, umgebenden Blockes.                                        */
   
                FacialEdge = EdgeCopy(FacialEdge)->pre;
                while(!Traversed(FacialEdge))
                   FacialEdge = FacialEdge->pre;
                while(TargetNodeGet(FacialEdge) != SourceNodeGet(FirstEdge))
                  {
                    add_to_slist(TheBlockCycle,FacialEdge->attrs);
                    FacialEdge = EdgeCopy(FacialEdge)->pre;
                    while(!Traversed(FacialEdge))
                            FacialEdge = FacialEdge->pre;
                   
                  }/*while*/
                add_to_slist(TheBlockCycle,FacialEdge->attrs);
                return(TheBlockCycle);
              }/*then*/
          else  return(empty_slist);
  }/* end of FindBlockCycleSuc */                              
         

/***************************************************/
/*                                                 */
/*             SearchNodesAndEdges                 */
/*                                                 */
/***************************************************/

static void SearchNodesAndEdges(FirstEdge,nodecounter,edgecounter,nr)
Slist   FirstEdge;
int     *nodecounter,*edgecounter,nr;

  {
    Slist   ActElem;
    
    if (EdgeVisited((FirstEdge)) != nr)
           {
             (*edgecounter)++;
             EdgeVisited((FirstEdge)) = nr;
             EdgeVisited(EdgeCopy(FirstEdge)) = nr;
             if (NodeVisited(TargetNodeGet(FirstEdge)) != nr && Drawn(TargetNodeGet(FirstEdge)) != nr)
                    {
                      NodeVisited(TargetNodeGet(FirstEdge)) = nr;
                      (*nodecounter)++;
                      for_slist(OrderedEdgeList(TargetNodeGet(FirstEdge)),ActElem)
                         SearchNodesAndEdges(ActElem,nodecounter,edgecounter,nr);
                      end_for_slist(OrderedEdgeList(TargetNodeGet(FirstEdge)),ActElem);
                    }
           }             
  }/* end of SearchNodesAndEdges */
  
  
/***************************************************/
/*                                                 */
/*           CountFacesOfBlockSuc                  */
/*                                                 */
/***************************************************/

static int CountFacesOfBlockSuc(TheCycle,nr)
Slist    TheCycle;
int      nr;

/*******************************************************************************/
/* Mit Hilfe des Satzes von Euler wird die Anzahl der Flaechen, die innerhalb  */
/* von 'TheCycle' liegen, berechnet. ...Suc bedeutet, dass die gesuchten       */
/* Flaechen auf der "suc"-Seite der Kanten von 'TheCycle' liegen.              */
/*******************************************************************************/

  {
    Slist   ActElem,StartEdge;
    int     nodecounter = 0,edgecounter = 0;
    
    if (TheCycle != empty_slist && TheCycle != TheCycle->suc)
            {
              for_slist(TheCycle,ActElem)
              nodecounter++;
              edgecounter++;
       
              StartEdge = EdgeCopy(ActElem)->pre;
       
              while(!Traversed(StartEdge))
                 {
                   SearchNodesAndEdges(StartEdge,&nodecounter,&edgecounter,nr);
                   StartEdge = StartEdge->pre;
                 }/*while*/  
             end_for_slist(TheCycle,ActElem);
             return(2-nodecounter+edgecounter-1);
           }
       else  return(0);   
           
  }/* end of CountFacesOfBlockSuc */
  


/***************************************************/
/*                                                 */
/*           CountFacesOfBlockPre                  */
/*                                                 */
/***************************************************/

static int CountFacesOfBlockPre(TheCycle,nr)
Slist    TheCycle;
int      nr;

/*******************************************************************************/
/* Mit Hilfe des Satzes von Euler wird die Anzahl der Flaechen, die innerhalb  */
/* von 'TheCycle' liegen, berechnet. ...Pre bedeutet, dass die gesuchten       */
/* Flaechen auf der "pre"-Seite der Kanten von 'TheCycle' liegen.              */
/*******************************************************************************/

  {
    Slist   ActElem,StartEdge;
    int     nodecounter = 0,edgecounter = 0;
    
    if (TheCycle != empty_slist && TheCycle != TheCycle->suc)
            {
              for_slist(TheCycle,ActElem)
              nodecounter++;
              edgecounter++;
       
              StartEdge = EdgeCopy(ActElem)->suc;
       
              while(!Traversed(StartEdge))
                 {
                   SearchNodesAndEdges(StartEdge,&nodecounter,&edgecounter,nr);
                   StartEdge = StartEdge->suc;
                 }/*while*/  
              end_for_slist(TheCycle,ActElem);
              return(2-nodecounter+edgecounter-1);
            }
       else  return(0);             
  }/* end of CountFacesOfBlockPre */
  


/***************************************************/
/*                                                 */
/*             NodesAreKollinear                   */
/*                                                 */
/***************************************************/

static bool NodesAreKollinear(A,B,C)
Snode   A,B,C;

/*******************************************************************************/
/* Die Funktion bestimmt, ob die Knoten A,B und C auf einer Geraden liegen     */
/* oder nicht. Falls die Knoten kolinear sind liefert die Funktion 'TRUE',     */
/* ansonsten 'FALSE'.                                                          */
/*******************************************************************************/

  {
    return( ( (snode_x(C)-snode_x(A)) * (snode_y(B)-snode_y(A)) ) == ( (snode_y(C)-snode_y(A)) * (snode_x(B)-snode_x(A)) ) );
  }/* end of NodesAreKollinear */  
  
     
/***************************************************/
/*                                                 */
/*             ExtendFacialCyclePre                */
/*                                                 */
/***************************************************/

static void ExtendFacialCyclePre(TheFacialCycle,nr)
Slist    TheFacialCycle;
int      nr;

  {
    Snode     StartNode,TargetNode;
    Slist     TheCycle,ActEdge,LastEdge,Start,ActElem;
    int       apexcounter,numberoffacesinblock;
    void      ExtendFacialCycleSuc();
    Point     v,M;
    double    weight,areaofblock;
    
    if (TheFacialCycle != empty_slist && TheFacialCycle->suc != TheFacialCycle)
           {
             nr++;
             Removed(SourceNodeGet(TheFacialCycle)) = TRUE; /* Kennzeichnen des Knotens v. (siehe [CDA]) */
             StartNode = TargetNodeGet(TheFacialCycle);
             TargetNode = SourceNodeGet(TheFacialCycle->pre);
             LastEdge = EdgeCopy(TheFacialCycle);
             

             while(StartNode != TargetNode)    
                {
                  /* Berechnen eines neuen Blocks. */
                  
                  if (!Removed(TargetNodeGet(LastEdge->suc)))
                          TheCycle = FindBlockCycleSuc(LastEdge->suc,&LastEdge,nr,&apexcounter);
                     else TheCycle = FindBlockCycleSuc(LastEdge->suc->suc,&LastEdge,nr,&apexcounter);
                  
    
                           
/*                printf("BlockCyclePre : %d ",nr); */
                  for_slist(TheCycle,ActEdge)
                      /*printf("%s ",SourceNodeGet(ActEdge)->label);*/   
                      Drawn(SourceNodeGet(ActEdge)) = nr;
                      Traversed(ActEdge) = TRUE;                      /* Kennzeichnen des Blockumkreises als "besucht". */
                      Traversed(EdgeCopy(ActEdge)) = TRUE;
                  end_for_slist(TheCycle,ActEdge);
                  numberoffacesinblock = CountFacesOfBlockSuc(TheCycle,nr);
/*                printf("   FL %d\n\n",numberoffacesinblock); */

                  
                                  
                  if  (TheCycle != empty_slist)
                               {
                                /* Zeichnen der noch nicht gesetzten Knoten von 'TheCycle'. */
                                
                                v.x = snode_x(SourceNodeGet(TheFacialCycle));
                                v.y = snode_y(SourceNodeGet(TheFacialCycle));
                                weight = (double)((double)(apexcounter+1))/((double)(numberoffacesinblock+apexcounter+1));
                                areaofblock = ComputeAreaOfBlock(SourceNodeGet(TheFacialCycle),SourceNodeGet(LastEdge),TheCycle);
                                weight = ComputeWeight(areaofblock,weight,TheCycle,SourceNodeGet(TheFacialCycle),SourceNodeGet(LastEdge),apexcounter);
                                if (NodesAreKollinear(SourceNodeGet(TheFacialCycle),SourceNodeGet(TheCycle),SourceNodeGet(LastEdge)))
                                       warning("Es tritt ein entartetes Dreieck auf !\n");
                                          
                                DrawEntryConvex(TheCycle,v,SourceNodeGet(TheCycle),SourceNodeGet(LastEdge),weight,apexcounter);
                                DrawUnconnectedNodes(TheCycle,SourceNodeGet(TheCycle),SourceNodeGet(LastEdge)); 
                               }
                               
                  Start = TheCycle; 
                  
                  /* Bestimmung des Knotens v ,als Startknoten fuer den rekursiven Aufruf. Hiermit wird, */
                  /* wenn moeglich, verhindert, dass das Dreieck (v,v1,vn),(siehe[CDA]), entartet ist.   */
                  
                  if (TheCycle != empty_slist && TheCycle->suc != TheCycle)
                      while(Start->suc != TheCycle && NodesAreKollinear(SourceNodeGet(Start),TargetNodeGet(Start),SourceNodeGet(Start->pre)))
                          Start = Start->suc;
        
                  ExtendFacialCycleSuc(Start,nr);  
                  free_slist(TheCycle);
                  StartNode = SourceNodeGet(LastEdge);
                }/* while */
                  
           }/*then*/
  }/* end of ExtendFacialCyclePre */                     
                  


/***************************************************/
/*                                                 */
/*             ExtendFacialCycleSuc                */
/*                                                 */
/***************************************************/

void ExtendFacialCycleSuc(TheFacialCycle,nr)
Slist    TheFacialCycle;
int      nr;

  {
    Snode     StartNode,TargetNode;
    Slist     TheCycle,ActEdge,LastEdge,Start,ActElem;
    int       apexcounter,numberoffacesinblock;
    Point     v,M;
    double    weight,areaofblock;
    
    if (TheFacialCycle != empty_slist && TheFacialCycle->suc != TheFacialCycle)
           {
          
                  
             nr++;
             Removed(SourceNodeGet(TheFacialCycle)) = TRUE;  /* Kennzeichnen des Knotens v. (siehe [CDA]) */
             StartNode = TargetNodeGet(TheFacialCycle);
             TargetNode = SourceNodeGet(TheFacialCycle->pre);
             LastEdge = EdgeCopy(TheFacialCycle);
             

             while(StartNode != TargetNode)    
                {
                  /* Berechnen eines neuen Blocks. */

                  if (!Removed(TargetNodeGet(LastEdge->pre)))
                          TheCycle = FindBlockCyclePre(LastEdge->pre,&LastEdge,nr,&apexcounter);
                     else TheCycle = FindBlockCyclePre(LastEdge->pre->pre,&LastEdge,nr,&apexcounter);
                     
/*                printf("BlockCyclePre : %d ",nr); */
                  for_slist(TheCycle,ActEdge)
                      /*printf("%s ",SourceNodeGet(ActEdge)->label);*/
                      Drawn(SourceNodeGet(ActEdge)) = nr;
                      Traversed(ActEdge) = TRUE;                 /* Kennzeichnen des Blockumkreises als "besucht". */
                      Traversed(EdgeCopy(ActEdge)) = TRUE;
                  end_for_slist(TheCycle,ActEdge);
                  numberoffacesinblock = CountFacesOfBlockPre(TheCycle,nr);
/*                printf("   FL %d\n\n",numberoffacesinblock); */

                                
                          
                  if  (TheCycle != empty_slist)
                               {
                                /* Zeichnen der noch nicht gesetzten Knoten von 'TheCycle'. */
                                
                                v.x = snode_x(SourceNodeGet(TheFacialCycle));
                                v.y = snode_y(SourceNodeGet(TheFacialCycle)); 
                                weight = (double)((double)(apexcounter+1))/((double)(numberoffacesinblock+apexcounter+1));
                                areaofblock = ComputeAreaOfBlock(SourceNodeGet(TheFacialCycle),SourceNodeGet(LastEdge),TheCycle);
                                weight = ComputeWeight(areaofblock,weight,TheCycle,SourceNodeGet(TheFacialCycle),SourceNodeGet(LastEdge),apexcounter);
                                if (NodesAreKollinear(SourceNodeGet(TheFacialCycle),SourceNodeGet(TheCycle),SourceNodeGet(LastEdge)))
                                       warning("Es tritt ein entartetes Dreieck auf !\n");
                                       
                                DrawEntryConvex(TheCycle,v,SourceNodeGet(TheCycle),SourceNodeGet(LastEdge),weight,apexcounter);
                                DrawUnconnectedNodes(TheCycle,SourceNodeGet(TheCycle),SourceNodeGet(LastEdge)); 
                               }
                               
                  Start = TheCycle;
                  
                  /* Bestimmung des Knotens v ,als Startknoten fuer den rekursiven Aufruf. Hiermit wird, */
                  /* wenn moeglich, verhindert, dass das Dreieck (v,v1,vn),(siehe[CDA]), entartet ist.   */

                  if (TheCycle != empty_slist && TheCycle->suc != TheCycle)
                       while(Start->suc != TheCycle && NodesAreKollinear(SourceNodeGet(Start),TargetNodeGet(Start),SourceNodeGet(Start->pre)))
                          Start = Start->suc;
             
                  ExtendFacialCyclePre(Start,nr);
                  free_slist(TheCycle);
                  StartNode = SourceNodeGet(LastEdge);
                }/* while */
                  
           }/*then*/
  }/* end of ExtendFacialCycleSuc */
  
  


  
/***************************************************/
/*                                                 */
/*             ExtendFacialCycle                   */
/*                                                 */
/***************************************************/


void ExtendFacialCycle(TheGraph,TheFacialCycle,edit)
Sgraph TheGraph;
Slist  TheFacialCycle;
bool   edit;

/*******************************************************************************/
/* Die Prozedur 'ExtendFacialCycle' entspricht im wesentlichen der in [CDA]    */
/* beschriebenen Prozedur CONVEX-DRAW. Fuer die Eingabe wird vorausgesetzt,    */
/* dass der Graph 'TheGraph' die Funktion 'ConvexityTest'(siehe ConvexTest.c)  */
/* durchlaufen hat. 'TheFacialCycle' ist das Ergebnis von 'ConvexityTest' und  */
/*'edit' legt fest, wie der Graph spaeter aussehen soll.                       */
/* Hier bestehen 2 Optionen:                                                   */
/*                                                                             */
/* (1) edit = TRUE                                                             */
/*	Der Graph wird konvex gezeichnet, wobei die Knoten- bzw. Kantentypen   */
/*	erhalten bleiben; d.h. der Graph sieht rein "aeusserlich" genauso      */
/*	aus, wie das Original.                                                 */
/* (2) edit = FALSE                                                            */
/*	Der Graph wird konvex gezeichnet und die Knoten werden so verkleinert, */
/*	dass sie nicht mehr erkennbar sind. Ausserdem werden die Pfeile, bei   */
/*	gerichteten, Graphen entfernt. Anschliessend besteht der Graph nur     */
/*	noch aus Linien, d.h. die Struktur des Graphen ist hier besser zu      */
/*	erkennen als bei der Option (1).                                       */
/*                                                                             */
/* Das Durchlaufen des Graphen sowie das Auffinden der Bloecke gewaehrleisten  */
/* die Prozeduren 'ExtendFacialCycleSuc' bzw. '-Pre'. Es werden hier zwei      */
/* (im wesentlichen gleiche) Prozeduren benoetigt, da nicht im Voraus          */
/* feststeht, wo die gefundene Flaechenbegrenzung liegt. Es gibt hier 2        */
/* Moeglichkeiten. Entweder liegt der Graph innerhalb oder ausserhalb der      */
/* Flaechenbegrenzung. Die beiden Begriffe innerhalb bzw. ausserhalb sind      */
/* natuerlich relativ bzgl. 'TheFacialCycle'. Fuer das Programm ist das eine   */
/* die "suc"-Seite und das andere die "pre"-Seite. Weiss man nun einmal,       */
/* wo der Graph bzgl. des gefundenen Umkreises liegt, so ist automatisch klar  */
/* auf welcher Seite die gefundenen Bloecke bzgl. ihrer Umkreise liegen.       */
/* Das Auffinden dieser Blockumkreise erledigen die Funktionen                 */
/* 'FindBlockCyclePre' bzw. '-Suc'.                                            */
/*                                                                             */
/* Das eigentliche Setzen der "neuen" Knoten eines Blockumkreise geschieht in  */
/* den Prozeduren 'ExtendFacialCycle...' mit Hilfe der Prozeduren              */
/* 'DrawUnconnectedNodes' und 'DrawEntryConvex'. Hierbei besteht nun eine      */
/* gewisse Wahlfreiheit durch den Parameter 'weight', der in gewissem Sinne    */
/* festlegt, wie gross die Flaeche des Blockes wird.                           */
/* Es wird hier versucht, die Flaechen innerhalb des Polygons (v,vi,..,vi+1)   */
/* (siehe [CDA] Bild) zu balancieren. Hierzu wird die Groesse dieses Polygons  */
/* sowie die Anzahl der Flaechen innerhalb des Blocks Bi berechnet. Die        */
/* Anzahl der restlichen Flaechen im Polygon kann mit Hilfe der Anzahl der     */
/* neu zu zeichnenden Ecken ('apexcounter') abgeschaetzt werden. Kennt man     */
/* nun die Groesse des Polygons sowie das Verhaeltnis der darauf aufzuteilenden*/
/* Flaechen (Flaechen im Block : restliche Flaechen im Polygon), so kann man   */
/* 'weight' so bestimmen,dass die beiden Flaechen des PolyGons (v,vi,..,vi+1), */
/* die durch die "neuen" Knoten festgelegt werden, ungefaehr gleich gross sind.*/
/* Die Berechnung von 'weight' geschieht mit Hilfe der Funktion                */
/* 'ComputeWeight' (s.o.).                                                     */
/*******************************************************************************/
    
  {
    Slist    StartEdge,ActElem;
    Snode    ActNode;
    bool     onlycycle = FALSE;
    
    if (TheFacialCycle != empty_slist)
           {

             message("Extendible Facial Cycle :\n");
             for_slist(TheFacialCycle,ActElem)
                message(" %s ",SourceNodeGet(ActElem)->label);
             end_for_slist(TheFacialCycle,ActElem);
             message("\n");
             
             StartEdge = TheFacialCycle;
             
             /* Test auf Knoten und Kanten innerhalb der gefundenen Flaechenbegrenzung 'TheFacialCycle'. */
             
             while(Deg(TargetNodeGet(StartEdge)) == 2 && !onlycycle)
                 {
                   StartEdge = StartEdge->suc;
                   if (StartEdge == TheFacialCycle)
                          onlycycle = TRUE;        /* Der Graph besteht nur aus einem Kreis. */
                 } 
                    
             DrawFacialCycleConvex(TheGraph,TheFacialCycle,edit);
     
                  
             EleminateNodesOfDegree2(TheGraph);
                     
             if (!Traversed(EdgeCopy(StartEdge)->pre))
                     ExtendFacialCycleSuc(StartEdge,1);
                else ExtendFacialCyclePre(StartEdge,1);
             
             free_slist(TheFacialCycle);
                
             DrawNodesOfDegree2(TheGraph);
              
              if (!edit)            
                     {
                       for_all_nodes(TheGraph,ActNode)
                         if (graphed_node(ActNode) != (Graphed_node)NULL)
                                 node_set(graphed_node(ActNode),ONLY_SET,NODE_SIZE,2,2,0);
                            else node_set(create_graphed_node_from_snode(ActNode),ONLY_SET,NODE_SIZE,2,2,0);     
                       end_for_all_nodes(TheGraph,ActNode);
                     }
             
             
             
             RemoveOrderedNEAttrs(TheGraph);
             
             message("Redrawing the graph ...\n");
           }
               
  }/* end of ExtendFacialCycle */                  



/*****************************************************************************************/
