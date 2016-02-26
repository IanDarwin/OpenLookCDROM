/* (C) Universitaet Passau 1986-1991 */
/*1990-91 von Stefan Jockenhoevel and Gerd Nienhaus.*/

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE FindSepPairs.c                                             **/
/**                                                                                     **/
/** Das Modul implementiert die im folgenden Artikel beschriebenen Funktionen und       **/
/** Prozeduren:                                                                         **/
/**                J.E.Hopcroft und R.E. Tarjan:                                        **/
/**                                                                                     **/
/**                DIVIDING A GRAPH INTO TRICONNECTED COMPONENTS                        **/
/**                                                                                     **/
/**          (erschienen in : SIAM J. Comput. Vol. 2, Nr. 3 ,September 1973)            **/
/**                                                                                     **/
/** Dieses Modul bezieht sich auf den Abschnitt 3 des Artikels:                         **/
/**          'Finding seperation pairs.'                                                **/
/**                                                                                     **/
/**                                                                                     **/
/**   Exportiert werden die Prozeduren  FindSeperationPairs(Sgraph  TheGraph)           **/
/**                                     RemoveSplitAttrs(Sgraph  TheGraph)              **/
/*****************************************************************************************/
/*****************************************************************************************/
  

#include "paths.h"
#include STDH
#include SGRAPHH
#include SLISTH
#include "NodeEdgeAttrs.h"
#include "Utilities.h"
#include "FindSepPairs.h"

 
/***************************************************/
/*                                                 */
/*               InitEdgeAttrs                     */
/*                                                 */
/***************************************************/


static void InitEdgeAttrs(TheGraph)
Sgraph  TheGraph;

/*******************************************************************************/
/*  Die Prozedur initialisiert die, im Modul NodeEdgeAttrs.h naeher            */
/*  beschriebenen, Kantenattribute.                                            */
/*******************************************************************************/
   
  {
    Sedge           ActEdge;
    Snode           ActNode;
    MyEdgeAttrs     ActEdgeAttrs;
    
    for_all_nodes(TheGraph,ActNode)
       for_sourcelist(ActNode,ActEdge)
          ActEdgeAttrs = talloc(struct myedgeattrs);
          ActEdgeAttrs->edgetype = DELETE;
          ActEdgeAttrs->firstedgeofpath = FALSE;
          ActEdgeAttrs->virtualedgenr = 0;
          ActEdgeAttrs->splitcomp = empty_slist;
          ActEdgeAttrs->correspondingedge = (Sedge) NULL;
          ActEdgeAttrs->activ = TRUE;
          ActEdge->attrs = make_attr(ATTR_DATA,(char *)ActEdgeAttrs);
       end_for_sourcelist(ActNode,ActEdge);
    end_for_all_nodes(TheGraph,ActNode);   
  }/*end of InitNodeAttrs*/

      
/***************************************************/
/*                                                 */
/*               InitNodeAttrs                     */
/*                                                 */
/***************************************************/


static void InitNodeAttrs(TheGraph)
Sgraph  TheGraph;

/*******************************************************************************/
/*  Die Prozedur initialisiert die, im Modul NodeEdgeAttrs.h naeher            */
/*  beschriebenen, Knotenattribute.                                            */
/*******************************************************************************/

  {
    Sedge           TheEdge;
    MyEdgeListAttrs TheAttrs;
    Snode           ActNode;
    MyNodeAttrs     ActNodeAttrs;
    
    for_all_nodes(TheGraph,ActNode)
       ActNodeAttrs = talloc(struct mynodeattrs);
       ActNodeAttrs->nr = 0;
       ActNodeAttrs->flag = FALSE;
       ActNodeAttrs->lowpt1 = EmptySnode;
       ActNodeAttrs->lowpt2 = EmptySnode;
       ActNodeAttrs->nd = 0;
       ActNodeAttrs->father = ActNode;
       ActNodeAttrs->newnum = 0;
       ActNodeAttrs->degree = 0;
       ActNodeAttrs->highpt = 0;
       ActNodeAttrs->A = NULL;
       ActNodeAttrs->firstson = EmptySnode;
       ActNodeAttrs->seppair = empty_slist;
       for_sourcelist(ActNode,TheEdge)
           TheAttrs = talloc(struct myedgelistattrs);
           TheAttrs->edgeofsourcelist = TRUE;
           TheAttrs->edge = TheEdge;
           TheAttrs->reached = FALSE;
           if (ActNodeAttrs->A == NULL)
                    ActNodeAttrs->A = new_slist(make_attr(ATTR_DATA,(char *)TheAttrs));
              else  add_immediately_to_slist(ActNodeAttrs->A,make_attr(ATTR_DATA,(char *)TheAttrs));
       end_for_sourcelist(ActNode,TheEdge);
       if (TheGraph->directed) 
              {
                for_targetlist(ActNode,TheEdge)
                   TheAttrs = talloc(struct myedgelistattrs);
                   TheAttrs->edgeofsourcelist = FALSE;
                   TheAttrs->edge = TheEdge;
                   TheAttrs->reached = FALSE;
                   if (ActNodeAttrs->A == NULL)
                            ActNodeAttrs->A = new_slist(make_attr(ATTR_DATA,(char *)TheAttrs));
                      else  add_immediately_to_slist(ActNodeAttrs->A,make_attr(ATTR_DATA,(char *)TheAttrs));
                end_for_targetlist(ActNode,TheEdge);
              }/*then*/  
       ActNode->attrs = make_attr(ATTR_DATA,(char *)ActNodeAttrs);
    end_for_all_nodes(TheGraph,ActNode);   
  }/*end of InitNodeAttrs*/

/***************************************************/
/*                                                 */
/*             RemoveEdgeAttrs                     */
/*                                                 */
/***************************************************/

static void RemoveEdgeAttrs(TheEdge)
Sedge TheEdge;

/*******************************************************************************/
/*  Die Prozedur loescht die durch InitEdgeAttrs angelegten Kantenattribute.   */
/*******************************************************************************/

  {
    free(TheEdge->attrs.data);
  }/* end of RemoveEdgeAttrs */  
    
/***************************************************/
/*                                                 */
/*             RemoveAdjacencyList                 */
/*                                                 */
/***************************************************/

static void RemoveAdjacencyList(TheSlist)
Slist  TheSlist;

/*******************************************************************************/
/*  Die Procedur loescht die in InitNodeAttrs angelegte Adjazentsliste.        */
/*******************************************************************************/

  {
    Slist ActElem;
    
    for_slist(TheSlist,ActElem)
       RemoveEdgeAttrs(EdgePointer(ActElem));
       free(ActElem->attrs.data);
    end_for_slist(TheSlist,ActElem);
    free(TheSlist);
  }/* end of Remove AdjacencyList */     

    
/***************************************************/
/*                                                 */
/*             RemoveNodeAttrs                     */
/*                                                 */
/***************************************************/

static void RemoveNodeAttrs(TheNode)
Snode TheNode;

/*******************************************************************************/
/*  Die Prozedur loescht die durch InitNodeAttrs angelegten Knotenattribute.   */
/*******************************************************************************/

  {
    RemoveAdjacencyList(AdjacencyList(TheNode));
    free(TheNode->attrs.data);
  }/* end of RemoveNodeAttrs */

  
/***************************************************/
/*                                                 */
/*             RemoveSplitAttrs                     */
/*                                                 */
/***************************************************/

void RemoveSplitAttrs(TheGraph)
Sgraph  TheGraph;

  {
    Snode  ActNode;
    
    for_all_nodes(TheGraph,ActNode)
       RemoveNodeAttrs(ActNode);
    end_for_all_nodes(TheGraph,ActNode);
  }/*end of RemoveSplitAttrs*/     
      
/***************************************************/
/*                                                 */
/*            MarkPtreeEdge                        */
/*                                                 */
/***************************************************/


static void  MarkPtreeEdge(TheSlistElem,TheEdgeType)
Slist  TheSlistElem;
EdgeType  TheEdgeType;

/*******************************************************************************/
/*  Die Prozedur markiert die Kanten des Graphen als 'ARC', d.h als Ast des    */
/*  erzeugten 'palm tree', oder als 'FROND'.(Genaue Definitionen fuer 'arc' ,  */
/*  'frond' und 'palm tree' findet man im oben genannten Artikel.)             */
/*  Ausserdem wird jede markierte Kante als 'Reached' gekennzeichnet. Dies     */
/*  dient dazu, die fuer den 'palm tree' wirklich benoetigten Kanten zu        */
/*  markieren. Aufgrund der gegebenen Sgraph-Struktur, in der jede Kanten      */
/*  in zwei Adjazentslisten vorkommt, ist dies notwendig, damit die nicht      */
/*  erreichten Kanten spaeter geloescht werden koennen.                        */
/*******************************************************************************/

  {
    TypeOfEdge(EdgePointer(TheSlistElem)) = TheEdgeType;
    Reached(TheSlistElem) = TRUE;
  }/*end of MarkPtreeEdge*/

  
/***************************************************/
/*                                                 */
/*                   DFS                           */
/*                                                 */
/***************************************************/


static void  DFS(n,SonNode,FatherNode)
int     *n;
Snode   SonNode,FatherNode;

/*******************************************************************************/
/*  Die Prozedur DFS ueberfuert den Graphen, dessen "erster" Knoten 'SonNode'  */
/*  ist, in einen 'palm tree'.                                                 */
/*  DFS beruht, wie der Name schon sagt, auf der bakannten Tiefensuche.        */
/*  Es werden die in NodeEdgeAttrs.h beschriebenen Attribute Nd,LowPt1,LowPt2  */
/*  sowie Father und Number berechnet.                                         */
/*  Das verwendete Makro 'MIN' ist im zugehoerigen Header-File definiert und   */
/*  dokumentiert.                                                              */
/*******************************************************************************/

  {
    Slist   ActElem;
    Snode   TargetNode;
    
    (*n) = Number(SonNode) = (*n)+1;
    LowPt1(SonNode) = LowPt2(SonNode) = SonNode;
    Nd(SonNode) = 1;
    for_slist(AdjacencyList(SonNode),ActElem)
       TargetNode = GetTargetNode(ActElem);
       if (Number(TargetNode) == 0)
              {
                MarkPtreeEdge(ActElem,ARC);
                DFS(n,TargetNode,SonNode);
                if (Number(LowPt1(TargetNode)) < Number(LowPt1(SonNode)))
                       {
                         LowPt2(SonNode) = MIN(LowPt1(SonNode),LowPt2(TargetNode));
                         LowPt1(SonNode) = LowPt1(TargetNode);
                       }/*then*/
                   else if (Number(LowPt1(TargetNode)) == Number(LowPt1(SonNode)))
                                 LowPt2(SonNode) = MIN(LowPt2(SonNode),LowPt2(TargetNode));
                           else  LowPt2(SonNode) = MIN(LowPt2(SonNode),LowPt1(TargetNode));
                Nd(SonNode) = Nd(SonNode)+Nd(TargetNode);
                Father(TargetNode) = SonNode;
              }/*then*/
          else{
                if ((Number(TargetNode) < Number(SonNode)) && ((TargetNode != FatherNode) || (Flag(SonNode) == TRUE)))
                       {
                         MarkPtreeEdge(ActElem,FROND);
                         if (Number(TargetNode) < Number(LowPt1(SonNode)))
                                {
                                  LowPt2(SonNode) = LowPt1(SonNode);
                                  LowPt1(SonNode) = TargetNode;
                                }/*then*/
                            else{
                                  if (Number(TargetNode) > Number(LowPt1(SonNode)))
                                           LowPt2(SonNode) = MIN(LowPt2(SonNode),TargetNode);
                                }/*else*/
                       }/*then*/
              }/*else*/
       if (TargetNode == FatherNode)
                Flag(SonNode) = TRUE;
    end_for_slist(AdjacencyList(SonNode),ActElem);
  }/*end of DFS*/
  
  
/***************************************************/
/*                                                 */
/*           DeleteDoubleEdges                     */
/*                                                 */
/***************************************************/


static void DeleteDoubleEdges(TheGraph)
Sgraph  TheGraph;

/*******************************************************************************/
/*  Hier werden die "ueberfluessigen"(d.h. die Kopien der Kanten, die in der   */
/*  Prozedur DFS nicht erreicht wurden) Kanten des Graphen geloescht.          */
/*  Da beim Durchlaufen einer Slist die Elemente nicht direkt geloescht werden */
/*  sollten, wird ein Kanten-Stack verwendet. Auf dem Stack werden fuer jeden  */
/*  Knoten die zu loeschenden Kanten gesammelt und erst anschliessend          */
/*  geloescht.                       */
/*                                                                             */
/*  Die Datenstruktur und die Prozeduren fuer den Stack sind im Modul          */
/*  Utilities.c bzw. Utilities.h definiert.                                    */
/*******************************************************************************/

  {
    Snode  ActNode;
    Slist  ActElem;
    EdgeStack  EStack;
    
    EStack = NULL;
    for_all_nodes(TheGraph,ActNode)
    
       /* Aufsammeln der "ueberfluessigen" Kanten. */ 
       
       for_slist(AdjacencyList(ActNode),ActElem)
          if (!Reached(ActElem))
                 AppendEdgeStack(&EStack,ActElem);
       end_for_slist(AdjacencyList(ActNode),ActElem);
      
       /* Loeschen der Kanten. */
       
       while(EStack != EmptyStack)
         {
           AdjacencyList(ActNode) = subtract_from_slist(AdjacencyList(ActNode),(EStack->edgelistelem)->attrs);
           
           free((EStack->edgelistelem)->attrs.data);   /* Freigeben des Speichers fuer die Listenattribute. */
           EStack = EStack->suc;
         }/*while*/
    end_for_all_nodes(TheGraph,ActNode);
  }/*end of DeleteDoubleEdges*/                     
  

/***************************************************/
/*                                                 */
/*                    phi                          */
/*                                                 */
/***************************************************/


static int phi(TheSlistElem)
Slist TheSlistElem;

/*********************************************************/
/* Die Prozedur berechnet den Wert der Funktion phi.     */
/*********************************************************/

   {
    if (TypeOfEdge(EdgePointer(TheSlistElem)) == FROND)
              return(2*Number(GetTargetNode(TheSlistElem))+1);
        else
            if (Number(LowPt2(GetTargetNode(TheSlistElem))) < Number(GetSourceNode(TheSlistElem)))
                   return(2*Number(LowPt1(GetTargetNode(TheSlistElem))));
              else return(2*Number(LowPt1(GetTargetNode(TheSlistElem)))+1);
   } /* end of phi */


/***************************************************/
/*                                                 */
/*            BuildAdjacencyLists                  */
/*                                                 */
/***************************************************/


static void BuildAdjacencyLists(TheGraph,V)
Sgraph TheGraph;
int    V;            /* Anzahl der Knoten von TheGraph */

/*******************************************************************************/
/* Die Prozedur baut die neu sortierte Adjazenzliste von TheGraph auf.         */
/* Die Sortierung erfolgt mit Hilfe der Funktion phi.                          */
/*******************************************************************************/

  {
    Slist  ActElem,TheEdgeListElem;
    int    stelle;
    Snode  TheNode;
    Slist  *BucketList;  
    
    
    BucketList = (Slist *)malloc(((2*V+1)*sizeof(Slist)));
    for (stelle=0;stelle<=2*V;stelle++)
     BucketList[stelle] = empty_slist;
       
    for_all_nodes(TheGraph,TheNode)       /* neue Adjazenzliste berechnen */
       for_slist(AdjacencyList(TheNode),TheEdgeListElem)
           if (BucketList[phi(TheEdgeListElem)-1] == empty_slist)
                  {
                    BucketList[phi(TheEdgeListElem)-1] = new_slist(TheEdgeListElem->attrs);
                  }  
              else{
                    add_immediately_to_slist(BucketList[phi(TheEdgeListElem)-1],TheEdgeListElem->attrs);
                  }  
      end_for_slist(AdjacencyList(TheNode),TheEdgeListElem); 
   end_for_all_nodes(TheGraph,TheNode);

   for_all_nodes(TheGraph,TheNode)         /* alte Adjazenzliste loeschen */
      {
       free_slist(AdjacencyList(TheNode));
       AdjacencyList(TheNode)=empty_slist;
      }
   end_for_all_nodes(TheGraph,TheNode);
    
   for (stelle=0;stelle<=2*V;stelle++)    /* neue Adjazenzliste zurueckschreiben */
      {
        for_slist(BucketList[stelle],ActElem)  
             if (ActElem != empty_slist)
                     if (AdjacencyList(GetSourceNode(ActElem)) == empty_slist)
                              AdjacencyList(GetSourceNode(ActElem)) = new_slist(ActElem->attrs);    
                         else add_immediately_to_slist(AdjacencyList(GetSourceNode(ActElem)),ActElem->attrs);
        end_for_slist(BucketList[stelle],ActElem);
      } 
   free(BucketList);   
         
  } /* end of BuildAdjacencyLists  */
  
  
/***************************************************/
/*                                                 */
/*               PathFinder                        */
/*                                                 */
/***************************************************/


static void PathFinder(TheGraph,FirstNode,Ess,m)
Sgraph  TheGraph;
Snode   FirstNode,*Ess;
int     *m;

/*******************************************************************************/
/*  Die Prozedur generiert eine Menge von Pfaden im Baum 'TheGraph' in         */
/*  folgender Weise:                                                           */
/*    Die Prozedur durchlaeuft den Baum nach dem Prinzip der Tiefensuche.      */
/*    Beim Traversieren eines Astes wird dieser an den aktuellen Pfad          */
/*    angehaengt. Jedesmal, wenn ein 'frond' traversiert wird, wird der 'frond'*/
/*    zur letzten Kante des Pfades und ein neuer Pfad beginnt. D.h. jeder Pfad */
/*    besteht aus einer Folge von Aesten und einem abschliessenden 'frond'.    */
/*  Weiterhin werden die im Modul NodeEdgeAttrs.h beschriebenen Attribute      */
/*  NewNum und HighPt gesetzt.                                                 */
/*  Da im weiteren Programmablauf nur interessant ist, ob eine Kante die erste */
/*  Kante eines Pfades ist oder nicht, werden die entsprechenden Kanten als    */
/*  'FirstEdgeOfPath' markiert.                                                */
/*                                                                             */
/*  Die Funktionen FindNode sowie GetTargetNode sind im Modul Utilities.c      */
/*  definiert.                                                                 */
/*******************************************************************************/

  {
    Snode  TheNode;
    Slist  ActElem;
    
    SetNewNum(FirstNode) = (*m)-Nd(FirstNode)+1;
    for_slist(AdjacencyList(FirstNode),ActElem);
       if ((*Ess) == NULL)
                {
                  (*Ess) = FirstNode;
                  FirstEdgeOfPath(EdgePointer(ActElem)) = TRUE;
                }/*then*/
       if (TypeOfEdge(EdgePointer(ActElem)) == ARC)
              {
                PathFinder(TheGraph,GetTargetNode(ActElem),Ess,m);
                (*m) = (*m) -1;           
              }/*then*/
          else{
                TheNode = FindNode(TheGraph,NEWNUM,NewNum(GetTargetNode(ActElem)));
                if (HighPt(TheNode) == 0)
                         HighPt(TheNode) = NewNum(FirstNode);
                (*Ess) = (Snode)NULL;
              }/*else*/
    end_for_slist(AdjacencyList(FirstNode),ActElem);
   
  }/* end of PathFinder*/ 
  
   
/***************************************************/
/*                                                 */
/*           DetermineDegreeOfNodes                */
/*                                                 */
/***************************************************/


static void DetermineDegreeOfNodes(TheGraph)
Sgraph  TheGraph;

/*******************************************************************************/
/*  Es wird der Grad aller Knoten des Graphen 'TheGraph' berechnet.            */
/*******************************************************************************/

  {
    Snode ActNode;
    Slist ActElem;
  
    for_all_nodes(TheGraph,ActNode)
       for_slist(AdjacencyList(ActNode),ActElem)
          Degree(GetSourceNode(ActElem))++;
          Degree(GetTargetNode(ActElem))++;
       end_for_slist(AdjacencyList(ActNode),ActElem);   
    end_for_all_nodes(TheGraph,ActNode);
  } 
  
   
/***************************************************/
/*                                                 */
/*          FindFirstSonOfNodes                    */
/*                                                 */
/***************************************************/


static void FindFirstSonOfNodes(TheGraph)
Sgraph  TheGraph;

/*******************************************************************************/
/*  Es wird fuer alle Knoten des Graphen 'TheGraph' der "erste" Sohn gesetzt.  */
/*  Naeheres zum Attribut FirstSon findet man im Modul NodeEdgeAttrs.h.        */
/*  Die Funktion GetTargetNode ist im Modul Utilities.c definiert.             */
/*******************************************************************************/

  {
    Snode  ActNode;
    
    for_all_nodes(TheGraph,ActNode)
       if (TypeOfEdge(EdgePointer(AdjacencyList(ActNode))) == ARC)
                FirstSon(ActNode) = GetTargetNode(AdjacencyList(ActNode));
           else FirstSon(ActNode) = EmptySnode;
    end_for_all_nodes(TheGraph,ActNode);
  }            


/***************************************************/
/*                                                 */
/*       FindSeperationPairs                       */
/*                                                 */
/***************************************************/


void FindSeperationPairs(TheGraph)
Sgraph  TheGraph;

  {
     int n = 0;
     Snode Ess = EmptySnode;
     
     InitNodeAttrs(TheGraph);
     InitEdgeAttrs(TheGraph);
     DFS(&n,TheGraph->nodes,EmptySnode);
     DeleteDoubleEdges(TheGraph);
     BuildAdjacencyLists(TheGraph,n);
     PathFinder(TheGraph,TheGraph->nodes,&Ess,&n);
     DetermineDegreeOfNodes(TheGraph);
     FindFirstSonOfNodes(TheGraph);
   }  
