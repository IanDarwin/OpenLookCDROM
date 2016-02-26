/* (C) Universitaet Passau 1986-1991 */
/*1990-91 von Stefan Jockenhoevel und Gerd Nienhaus.*/
/* Stand : 15.01.1991 */

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE ConvexTest.c                                               **/
/**                                                                                     **/
/**                                                                                     **/
/**   In diesem Modul sind die Aussagen des Kapitels 4 des Textes:                      **/
/**                                                                                     **/
/**           DRAWING PLANAR GRAPHS    [DPG4]                                           **/
/**                                                                                     **/
/**   im wesentlichen implementiert.                                                    **/
/**                                                                                     **/
/**   Exportiert werden die Funktionen:                                                 **/
/**        ConvexityTest(Sgraph  TheGraph)                                              **/
/**        RemoveOrderedNEAttrs(Sgraph  TheGraph)                                       **/
/**                                                                                     **/
/**   ACHTUNG: In diesem Modul wird die Funktion 'embed' verwendet, die nicht von       **/
/**            den beiden oben genannten Autoren stammt (siehe ConvexityTest).          **/
/*****************************************************************************************/
/*****************************************************************************************/

#include "paths.h"
#include STDH
#include SGRAPHH
#include SLISTH
#include GRAPHEDH
#include "NodeEdgeAttrs.h" 
#include "Utilities.h"
#include "FindSplitComp.h"
#include "FindSepPairs.h"
#include "ConvexTest.h" 
#include "embed.h" 


/***************************************************/
/*                                                 */
/*                 BuildComp                       */
/*                                                 */
/***************************************************/


static void BuildComp(ActElem)
  /* ueberprueft, ob eine virtuelle Kante 'aktiv' ist */
  /* wenn eine nichtaktive Kante gefunden wird, so wird rekursiv die zugehoerige EdgeList ueberprueft */
Slist ActElem;

 {
   Slist EdgeList;
  
   for_slist(SplitCompEdges(ActElem),EdgeList)
    if ((VirtualEdgeNr(EdgeOfSplitComp(EdgeList)) != 0)
          && (TypeOfSplitComp(ActElem) == TypeOfSplitComp(BelongsToSplitComp(CorrespondingEdge(EdgeOfSplitComp(EdgeList)))))
          && (IsActiv(EdgeOfSplitComp(EdgeList))))
         {
          IsActiv(EdgeOfSplitComp(EdgeList)) = FALSE;
          IsActiv(CorrespondingEdge(EdgeOfSplitComp(EdgeList))) = FALSE;
          IsReached(BelongsToSplitComp(CorrespondingEdge(EdgeOfSplitComp(EdgeList)))) = TRUE;
          BuildComp(BelongsToSplitComp(CorrespondingEdge(EdgeOfSplitComp(EdgeList))));
         }   
   end_for_slist(SplitCompEdges(ActElem),EdgeList);
    
 } /*end of BuildComp*/
 
 
/***************************************************/
/*                                                 */
/*        FindThreeConnectedComp                   */
/*                                                 */
/***************************************************/


static void FindThreeConnectedComp(TheGraphList)
  /* Symbolischer Aufbau der 3-fach Zusammenhangskomponenten */
Slist TheGraphList;

 {
  Slist ActElem;
  
  for_slist(TheGraphList,ActElem)
   IsReached(ActElem) = FALSE;
  end_for_slist(TheGraphList,ActElem)
  
  for_slist(TheGraphList,ActElem)
   if (((! IsReached(ActElem)) && (TypeOfSplitComp(ActElem) != THREECONCOMP)))
       {
        BuildComp(ActElem);
       }
  end_for_slist(TheGraphList,ActElem);
  
 } /*end of FindThreeConnectedComp*/


/***************************************************/
/*                                                 */
/*              ContainsRealEdge                   */
/*                                                 */
/***************************************************/

static bool ContainsRealEdge(TheComp)
  /* ueberprueft, ob eine Splitcomponente eine "echt" Kante enthaelt */
Slist   TheComp;

  {
    Slist  ActEdge;
    
    for_slist(SplitCompEdges(TheComp),ActEdge)
       if (VirtualEdgeNr(EdgeOfSplitComp(ActEdge)) == 0)
             return(TRUE);
    end_for_slist(SplitCompEdges(TheComp),ActEdge);
    return(FALSE);
  }           

    
/***************************************************/
/*                                                 */
/*            DeterminePrimeSepPairs               */
/*                                                 */
/***************************************************/


static void DeterminePrimeSepPairs(SepPairsListElem)
  /* es wird ueberprueft, ob das Spaltungspaar Prime ist */
Slist SepPairsListElem;

 {
  Slist ActElem;
  
  for_slist(VirtualEdgeList(SepPairsListElem),ActElem)
    if IsActiv(VirtualEdge(ActElem))
         {
          if (ContainsRealEdge(BelongsToSplitComp(VirtualEdge(ActElem))))
                 {
                   IsPrime(SepPairsListElem) = TRUE;
                   break;
                 }  
         }
  end_for_slist(VirtualEdgeList(SepPairsListElem),ActElem);
                    
 } /*end of DeterminePrimeSepPairs*/          

/***************************************************/
/*                                                 */
/*                RemoveRing                       */
/*                                                 */
/***************************************************/

static void RemoveRing(TheRing)
  /* entfernen eines Ringes */
SedgeStack   TheRing;

  {
    SedgeStack  Help;
    
    while(TheRing != EmptySedgeStack)
       {
         Help = TheRing;
         TheRing = TheRing->suc;
         free(Help);
       } 
  }/* end of RemoveRing */        

/***************************************************/
/*                                                 */
/*                   AddToRing                     */
/*                                                 */
/***************************************************/


static void AddToRing(SplitComp,TheRing)
  /* Aufbau eines Ringes */
Slist        SplitComp;
SedgeStack   *TheRing;

  {
    Slist  ActEdge;
    
    for_slist(SplitCompEdges(SplitComp),ActEdge)
       if (VirtualEdgeNr(EdgeOfSplitComp(ActEdge)) == 0)
              AppendSedgeStack(TheRing,EdgeOfSplitComp(ActEdge));
    end_for_slist(SplitCompEdges(SplitComp),ActEdge);
  }  
 
              
/***************************************************/
/*                                                 */
/*                 CheckType                       */
/*                                                 */
/***************************************************/


static void CheckTyp(SepPairsElem,VirtEdge,counter3con,countertriangle,counterbond,OneRealEdge,TheRing,RingFound)
  /* Feststellung des Typen der Splicomponente */
Slist SepPairsElem;
int   *counter3con,*countertriangle,*counterbond;
bool  *OneRealEdge,*RingFound;
Sedge VirtEdge;
SedgeStack  *TheRing;

 {
  Slist ActElem;
  bool  examined = FALSE;
  
  if (! (*OneRealEdge))
        (*OneRealEdge) = ContainsRealEdge(BelongsToSplitComp(VirtEdge));
  for_slist(SplitCompEdges(BelongsToSplitComp(VirtEdge)),ActElem)
   if (!IsActiv(EdgeOfSplitComp(ActElem)) 
    && (EdgeOfSplitComp(ActElem) != VirtEdge) 
    && (!(IsReached(BelongsToSplitComp(CorrespondingEdge(EdgeOfSplitComp(ActElem)))))))
          {
           examined = TRUE;
           IsReached (BelongsToSplitComp(CorrespondingEdge(EdgeOfSplitComp(ActElem)))) = TRUE;
           
           if (TypeOfSplitComp(BelongsToSplitComp(VirtEdge)) == THREECONCOMP)
                 (*counter3con)++;
            else if (TypeOfSplitComp(BelongsToSplitComp(VirtEdge)) == TRIANGLE)  
                      {
                        (*countertriangle)++;
                        if ((!(*RingFound)) && (((*counter3con) == 0) && ((*counterbond) == 0)))
                             AddToRing(BelongsToSplitComp(VirtEdge),TheRing);
                      }       
                 else (*counterbond)++;      
           CheckTyp(SepPairsElem,CorrespondingEdge(EdgeOfSplitComp(ActElem)),counter3con,countertriangle,counterbond,OneRealEdge,TheRing,RingFound);
          }
  end_for_slist(SplitCompEdges(BelongsToSplitComp(VirtEdge)),ActElem);
  
  if (!examined)
       {
         if (TypeOfSplitComp(BelongsToSplitComp(VirtEdge)) == THREECONCOMP)
                 {
                   (*counter3con)++;
                 }  
            else if (TypeOfSplitComp(BelongsToSplitComp(VirtEdge)) == TRIANGLE)  
                      {
                        (*countertriangle)++;
                        if ((!(*RingFound)) && (((*counter3con) == 0) && ((*counterbond) == 0)))
                             AddToRing(BelongsToSplitComp(VirtEdge),TheRing);
                      }
                 else {
                        (*counterbond)++;
                      } 
       }           
 } /*end of CheckTyp*/ 

 
/***************************************************/
/*                                                 */
/*           DetermineTypOfPrime                   */
/*                                                 */
/***************************************************/


static void DetermineTypOfPrime(SepPairsListElem)
  /* es wird ueberprueft, ob ein Spaltungspaar, welches Prime ist, auch Forbidden oder Critical ist  */
Slist SepPairsListElem;

 {
  Slist ActElem;
  int NrSplitComp = 0;
  int counter3con,countertriangle,counterbond,Ring = 0,Bond = 0;
  bool OneRealEdge,RingFound = FALSE;
  SedgeStack  TheRing = EmptySedgeStack;
  
  for_slist(VirtualEdgeList(SepPairsListElem),ActElem)
   OneRealEdge = FALSE;
   counter3con = countertriangle = counterbond = 0;
   if (!IsReached(BelongsToSplitComp(VirtualEdge(ActElem))))
          {
            IsReached(BelongsToSplitComp(VirtualEdge(ActElem))) = TRUE;
            CheckTyp(SepPairsListElem,VirtualEdge(ActElem),&counter3con,&countertriangle,&counterbond,&OneRealEdge,&TheRing,&RingFound);
            if (OneRealEdge)
                 {
                   NrSplitComp++;   
                   if ((counter3con == 0) && (countertriangle == 0))
                           Bond++;
                      else if ((counter3con == 0) && (counterbond == 0))
                                  {
                                    Ring++;
                                    RingFound = TRUE;
                                  }  
                              else{
                                    if (!RingFound)
                                        {
                                          RemoveRing(TheRing);
                                          TheRing = EmptySedgeStack;
                                        }  
                                  }     
                  } 
           }                  

   end_for_slist(VirtualEdgeList(SepPairsListElem),ActElem);
        

    if ((NrSplitComp >= 4) || (NrSplitComp == 3 && ((Ring == 0) && (Bond == 0) )))
              IsForbidden(SepPairsListElem) = TRUE;
         else if ((NrSplitComp == 3) || ((NrSplitComp == 2) && (Ring == 0)))
                      {
                      IsCritical(SepPairsListElem) = TRUE;
                      if (Ring ==1) 
                               HasOneRing(SepPairsListElem) = TheRing;
                         else{
                               HasOneRing(SepPairsListElem) = EmptySedgeStack;
                               RemoveRing(TheRing);
                               TheRing = EmptySedgeStack;
                             }     
                      }
    
          
   } /*end of DetermineTypOfPrime*/          


/***************************************************/
/*                                                 */
/*                CheckSepPairs                    */
/*                                                 */
/***************************************************/


static Slist CheckSepPairs(SepPairsList,SplitList,NumberOfCriticalPairs)
  /* Hauptprozedur zur Ueberpruefung, ob ein Spaltungspaar Prime, Critical, Forbidden ist         */
  /* wenn es ein verbotenes Spaltungspaar gibt, wird dieses zurueckgegeben, ansonsten empty_slist */
Slist SepPairsList;
Slist SplitList;
int   *NumberOfCriticalPairs;

 {
  Slist ActElem,ActComp,ActEdge;
  
  for_slist(SepPairsList,ActElem)
      DeterminePrimeSepPairs(ActElem);
  end_for_slist(SepPairsList,ActElem);
  
  for_slist(SplitList,ActComp)
    for_slist(SplitCompEdges(ActComp),ActEdge)
      if (VirtualEdgeNr(EdgeOfSplitComp(ActEdge)) != 0)
            IsActiv(EdgeOfSplitComp(ActEdge)) = FALSE;
    end_for_slist(SplitCompEdges(ActComp),ActEdge);
  end_for_slist(SplitList,ActComp);
    
  for_slist(SepPairsList,ActElem)
  
      for_slist(SplitList,ActComp)
        IsReached(ActComp) = FALSE;
      end_for_slist(SplitList,ActComp);
      
      for_slist(VirtualEdgeList(ActElem),ActEdge)
        IsActiv(VirtualEdge(ActEdge)) = TRUE;
      end_for_slist(VirtualEdgeList(ActElem),ActEdge);
   
     if IsPrime(ActElem)
           {
             DetermineTypOfPrime(ActElem);
             if (IsForbidden(ActElem))
                      return(ActElem);
                else if IsCritical(ActElem)
                            (*NumberOfCriticalPairs)++;     
           }        
       
     for_slist(VirtualEdgeList(ActElem),ActEdge)
        IsActiv(VirtualEdge(ActEdge)) = FALSE;
     end_for_slist(VirtualEdgeList(ActElem),ActEdge);
               
  end_for_slist(SepPairsList,ActElem);
  return(empty_slist);
  
 } /*end of CheckSepPairs*/
 
/*******************************************************************************************************************************/
/*******************************************************************************************************************************/
/*******************************************************************************************************************************/


    

/***************************************************/
/*                                                 */
/*        InitEmbededNodeEdgeAttrs                 */
/*                                                 */
/***************************************************/


static void InitEmbededNodeEdgeAttrs(TheGraph)
Sgraph   TheGraph;

/*******************************************************************************/
/*  Die Funktion 'InitEmbededNodeEdgeAttrs' initialisiert die in ConvexTest.h  */
/*  beschriebenen 'OrderedNodeAttrs' bzw 'OrderedEdgeListAttrs'. Als Eingabe   */
/*  wird ein 'Sgraph' benoetigt ,in dessen Knotenattributen eine 'Slist'       */
/*  abgelegt ist. In den Attributen der 'Slist' (EmbededList(TheNode)) sollten */
/*  jeweils die Kanten ,die mit dem Knoten verbunden sind, in geordneter Weise */
/*  abgespeichert sein. Die Ordnung der Kanten sollte im Uhrzeigersinn um      */
/*  'TheNode' sein.                                                            */
/*******************************************************************************/

  {
    Sedge                 ActEdge;
    Slist                 OrderedList,ActElem;
    Snode                 ActNode;
    OrderedEdgeListAttrs  TheEdgeListAttrs;
    OrderedNodeAttrs      TheNodeAttrs;
    OrderedEdgeAttrs      TheEdgeAttrs;
    
    /* Initialisierung von 'EdgeEntry(TheEdge)'. EdgeEntry wird verwendet, um die beiden Kopien einer Kante */
    /* miteinander zu verbinden.                                                                            */
    
    for_all_nodes(TheGraph,ActNode)
       for_sourcelist(ActNode,ActEdge)
          TheEdgeAttrs = talloc(struct orderededgeattrs);
          TheEdgeAttrs->edgeentry = empty_slist;
          set_edgeattrs(ActEdge,make_attr(ATTR_DATA,(char *)TheEdgeAttrs));
       end_for_sourcelist(ActNode,ActEdge);   
    end_for_all_nodes(TheGraph,ActNode);
    
    /* Initialisierung der Knotenattribute. */
        
    for_all_nodes(TheGraph,ActNode)
       OrderedList = EmbededList(ActNode);
       TheNodeAttrs = talloc(struct orderednodeattrs);
       TheNodeAttrs->degree = 0;
       TheNodeAttrs->drawn = 0;
       TheNodeAttrs->nodevisited = 0;
       TheNodeAttrs->removed = FALSE;
       TheNodeAttrs->apex = FALSE;
       TheNodeAttrs->orderededgelist = empty_slist;
       set_nodeattrs(ActNode,make_attr(ATTR_DATA,(char *)TheNodeAttrs));
       
       /* Initialisierung der geordneten Kantenliste. */
       
       for_slist(OrderedList,ActElem)
            Deg(ActNode)++;
            TheEdgeListAttrs = talloc(struct orderededgelistattrs);
            TheEdgeListAttrs->traversed = FALSE;
            TheEdgeListAttrs->edgevisited = 0;
            if (EmbededEdge(ActElem)->snode == ActNode)
                    TheEdgeListAttrs->edgeoftargetlist = FALSE;
               else TheEdgeListAttrs->edgeoftargetlist = TRUE;
            TheEdgeListAttrs->edge = EmbededEdge(ActElem);
            if (OrderedEdgeList(ActNode) == empty_slist)
                    OrderedEdgeList(ActNode) = new_slist(make_attr(ATTR_DATA,(char *)TheEdgeListAttrs));
               else add_immediately_to_slist(OrderedEdgeList(ActNode),make_attr(ATTR_DATA,(char *)TheEdgeListAttrs));
            if (EdgeEntry(EmbededEdge(ActElem)) == empty_slist)
                     EdgeEntry(EmbededEdge(ActElem)) = OrderedEdgeList(ActNode)->pre;
               else{
                     EdgeCopy(OrderedEdgeList(ActNode)->pre) = EdgeEntry(EmbededEdge(ActElem));
                     EdgeCopy(EdgeEntry(EmbededEdge(ActElem)))  = OrderedEdgeList(ActNode)->pre;
                     free(EmbededEdge(ActElem)->attrs.data);
                   }
       end_for_slist(OrderedList,ActElem);
       free_slist(OrderedList);       /* Loeschen des Speicherplatzes fuer die urspruengliche Kantenliste 'EmbededList'. */
    end_for_all_nodes(TheGraph,ActNode);
    
    
  }/* end of InitEmbededNodeEdgeAttrs */
  
/*******************************************************************************************************************************/
/*******************************************************************************************************************************/
/*******************************************************************************************************************************/
                                                    
/***************************************************/
/*                                                 */
/*                 FindPath                        */
/*                                                 */
/***************************************************/

static bool FindPath(ThePath,TheEdge,TNode)
Slist  *ThePath,TheEdge;
Snode  TNode;

  {
    if (TargetNodeGet(TheEdge->suc) == TNode)
           {
             add_immediately_to_slist((*ThePath),(TheEdge->suc)->attrs);
             return(TRUE);
           }/*then*/
       else{
             if (Deg(TargetNodeGet(TheEdge->suc)) == 2)
                    {
                      add_immediately_to_slist((*ThePath),(TheEdge->suc)->attrs);
                      return(FindPath(ThePath,EdgeCopy(TheEdge->suc),TNode));
                    }  
                else  return(FALSE);
           }/*else*/
  }/* end of FindPath */

                                
/***************************************************/
/*                                                 */
/*               DeleteOneXYPath                   */
/*                                                 */
/***************************************************/

static void DeleteOneXYPath(SNode,TNode)
Snode  SNode,TNode;

/*******************************************************************************/
/* Diese Funktion wird von FacialCycleOfSevenPossibleGraphs aufgerufen.        */
/* Es wird ein Pfad von 'SNode' nach 'TNode' gesucht,der nur Knoten mit Grad 2 */
/* enthaelt. Die gefundenen Kanten werden ,mit Hilfe des Attributes 'Deleted', */
/* als geloescht gekennzeichnet.                                               */
/*******************************************************************************/

  {
    Slist  ActEdge,ThePath = empty_slist;
    bool   found = FALSE;
    
    for_slist(OrderedEdgeList(SNode),ActEdge)
       if (Deg(TargetNodeGet(ActEdge)) == 2)
             {
               ThePath = new_slist(ActEdge->attrs);
               found = FindPath(&ThePath,EdgeCopy(ActEdge),TNode);
               if (found)
                      {  
                        for_slist(ThePath,ActEdge)
                           Deleted(Edge(ActEdge)) = 1;
                        end_for_slist(ThePath,ActEdge);
                        free_slist(ThePath);
                        break;
                      }
                  else{
                        free_slist(ThePath);
                        ThePath = empty_slist;
                      }
             }/*then*/
    end_for_slist(OrderedEdgeList(SNode),ActEdge);
  }/* end of DeleteOneXYPath */
                                       

/***************************************************/
/*                                                 */
/*               IsEdgeThenDelete                  */
/*                                                 */
/***************************************************/


static bool IsEdgeThenDelete(SNode,TNode)
Snode  SNode,TNode;

/*******************************************************************************/
/* Die Funktion wird von FacialCycleOfSevenPossibleGraphs aufgerufen.          */
/* Sie sucht eine Kante von 'SNode' nach 'TNode' im Graphen und kennzeichnet   */
/* diese als geloescht. Wird keine solche Kante gefunden, liefert die Funktion */
/* 'FALSE'.                                                                    */
/*******************************************************************************/

  {
    Slist  ActEdge;
    
    for_slist(OrderedEdgeList(SNode),ActEdge)
       if (TargetNodeGet(ActEdge) == TNode)
          {
            Deleted(Edge(ActEdge)) = 1;
            return(TRUE);
          }
    end_for_slist(OrderedEdgeList(SNode),ActEdge);
    return(FALSE);
 }/* end of IsEdgeThenDelete */   
            
          
/***************************************************/
/*                                                 */
/*                 DeleteXYPath                    */
/*                                                 */
/***************************************************/


static void DeleteXYPath(TheRing)
SedgeStack TheRing;

/*******************************************************************************/
/* Diese Prozedur wird von FacialCycleOfSevenPossibleGraphs aufgerufen(s.u.).  */
/* Sie kennzeichnet alle Kanten eines Ringes als 'Deleted'.                    */
/*******************************************************************************/

  {
    while (TheRing != EmptySedgeStack)
       {
         Deleted(TheRing->edge) = 1;
         TheRing = TheRing->suc;
       }/*while*/
  }/* end of DeleteXYPath */
  
                     
/***************************************************/
/*                                                 */
/*           FindFacialCycleWithXY                 */
/*                                                 */
/***************************************************/

static Slist FindFacialCycleWithXY(x,y)
Snode   x,y;

/*******************************************************************************/
/* Die Funktion sucht eine Flaechenbegrenzung im Graphen, die die Knoten x     */
/* und y enthaelt. Kanten die als 'Deleted' gekennzeichnet sind werden als     */
/* nicht vorhanden betrachtet !                                                */
/*******************************************************************************/

  {
    Slist   ActEdge,FacialEdge,TheFacialCycle = empty_slist;
    bool    found = FALSE;
    

    
    /* Suchen des 'FacialCycle' . */
    
    for_slist(OrderedEdgeList(x),ActEdge)
       if (Deleted(Edge(ActEdge)) != 1)  /* Start der Suche bei x mit einer nicht "geloeschten" Kante. */
             {
               FacialEdge = ActEdge;
               while(TargetNodeGet(FacialEdge) != x)   
                  {
                    if (TargetNodeGet(FacialEdge) == y) /* Die aktuelle Flaechenbegrenzung enthaelt y. */
                            found = TRUE;
                    if (TheFacialCycle == empty_slist)
                            TheFacialCycle = new_slist(FacialEdge->attrs);
                       else add_immediately_to_slist(TheFacialCycle,FacialEdge->attrs);
                    FacialEdge = EdgeCopy(FacialEdge)->suc;    
                    while(Deleted(Edge(FacialEdge)) == 1)
                       FacialEdge = FacialEdge->suc;
                  }/* while */
               add_immediately_to_slist(TheFacialCycle,FacialEdge->attrs);   
               if (found)
                        return(TheFacialCycle);  /* Gesuchte Flaechenbegrenzung gefunden ! */
                  else{
                        free_slist(TheFacialCycle);    /* Die gefundene Flaechenbegrenzung enthaelt y nicht ! */
                        TheFacialCycle = empty_slist;
                      }/* else */
             }/* then */
    end_for_slist(OrderedEdgeList(x),ActEdge);                                                                     
    return(empty_slist);
  }/* end of FindFacialCycleWithXY */
  
  
  
/***************************************************/
/*                                                 */
/*     FacialCycleOfSevenPossibleGraphs            */
/*                                                 */
/***************************************************/
  
static Slist FacialCycleOfSevenPossibleGraphs(TheGraph,SepPairList)
Sgraph TheGraph;
Slist  SepPairList;

/*******************************************************************************/
/* Die Funktion berechnet die 'extendible' Flaechenbegrenzung im Graphen       */
/* 'TheGraph', falls der Graph genau ein kritisches Spaltungspaar {x,y}        */
/* besitzt. In diesen Fall gibt es genau 7 verschiedene Moeglichkeiten fuer    */
/* die Struktur des Graphen bzgl. der vorhandenen {x,y}-Splitkomponenten.      */
/* Bezeichne T eine Dreifachzusammenhangskomponente, R einen Ring und B einen  */
/* 'Bond',dann gibt es die folgenden 7 Moeglichkeiten fuer die Verteilung      */
/* der {x,y}-Splitkomponenten:                                                 */
/*   TT,TRT,TBT,TRR,TBR,RRR,RBR                                                */
/* (siehe auch [DPG4] Fig.4.5 bzw Lemma4.4)                                    */
/* Die 'extendible' Flaechenbegrenzung wird in diesen Faellen wie folgt        */
/* bestimmt:                                                                   */
/*   Falls die Kante (x,y) ex wird sie geloescht                               */
/*     Sonst,falls genau eine {x,y}-Splitkomponente vom Typ Ring ex., werden   */
/*                alle nicht-virtuellen Kanten des Ringes geloescht            */
/*             Sonst, wird ein Pfad von x nach y gesucht, der nur Knoten       */
/*                    vom Grad 2 enthaelt und dieser wird dann geloescht       */
/* (Zu beachten ist ,dass hier Kanten nicht wirklich geloescht werden,         */
/* sondern mit Hilfe des Attributes, 'Deleted', als solche gekennzeichnet      */
/* werden.)                                                                    */
/* Nach Bearbeiten des Graphen wie oben beschrieben, gibt es noch genau        */
/* 2 Flaechenbegrenzungen im Graphen die x und y enthalten. Eine davon wird    */
/* nun mit Hilfe der Funktion 'FindFacialCycleWithXY' gefunden und als         */
/* Ergebnis zurueckgegeben.                                                    */
/*******************************************************************************/

  {
    Slist   TheCriticalPair,ActElem,TheFacialCycle;
    Snode   ActNode;
    int     *flag;
    Sedge   ActEdge;
    
    for_all_nodes(TheGraph,ActNode)
       for_sourcelist(ActNode,ActEdge)
          flag = talloc(int);
          (*flag) = 0;
          set_edgeattrs(ActEdge,make_attr(ATTR_DATA,(char *)flag));
       end_for_sourcelist(ActNode,ActEdge);
    end_for_all_nodes(TheGraph,ActNode);
    
    /* Suchen des kritischen Spaltungspaares in der Liste der Spaltungspaare. */
       
    for_slist(SepPairList,ActElem)
       if (IsCritical(ActElem))
           {
             TheCriticalPair = ActElem;
             break;
           }/*then*/
    end_for_slist(SepPairList,ActElem);
    
    /* Vorbereiten des Graphen und suchen der 'extendible' Flaechenbegrenzung. */
    
    if (!IsEdgeThenDelete(SepPairA(TheCriticalPair),SepPairB(TheCriticalPair)))
            if (HasOneRing(TheCriticalPair) != EmptySedgeStack)
                      DeleteXYPath(HasOneRing(TheCriticalPair));
               else DeleteOneXYPath(SepPairA(TheCriticalPair),SepPairB(TheCriticalPair));
    TheFacialCycle = FindFacialCycleWithXY(SepPairA(TheCriticalPair),SepPairB(TheCriticalPair));
    
    /* Freigeben des Speicherplatzes fuer 'Deleted'. */
    
    for_all_nodes(TheGraph,ActNode)
       for_sourcelist(ActNode,ActEdge)
          free(ActEdge->attrs.data);
       end_for_sourcelist(ActNode,ActEdge);
    end_for_all_nodes(TheGraph,ActNode);
    
    return(TheFacialCycle);           
  }/* end of FacialCycleOfSevenPossibleGraphs */                               
 

/***************************************************/
/*                                                 */
/*                   CopySedge                     */
/*                                                 */
/***************************************************/


static Sedge CopySedge(TheEdge)
Sedge TheEdge;

/*******************************************************************************/
/* Die Funktion 'CopySedge' erzeugt eine neue Kante vom Typ 'Sedge' und        */
/* kopiert die Elemte der 'Sedge-structure' der Kante 'TheEdge' in die         */
/* Struktur der neuen Kante. Ausserdem werden mit Hilfe von 'Info' die         */
/* "Graphed-Parameter" der Kante gerettet, und in die Attribute der neuen      */
/* Kante abgelegt.                                                             */
/*******************************************************************************/

  {
    Sedge  NewEdge;
    EdgeInfos  Info;
    
    NewEdge = talloc(struct sedge);
    NewEdge->label = strsave(TheEdge->label);
    Info = talloc(struct edgeinfos);
    Info->edgetype = (int)edge_get(graphed_edge(TheEdge),EDGE_TYPE);
    Info->edgearrowlength = (int)edge_get(graphed_edge(TheEdge),EDGE_ARROW_LENGTH);
    Info->edgearrowangle = (float)((long)edge_get(graphed_edge(TheEdge),EDGE_ARROW_ANGLE));
    Info->edgefont = (int)edge_get(graphed_edge(TheEdge),EDGE_FONT);
    Info->edgelabelvisibility = (bool)edge_get(graphed_edge(TheEdge),EDGE_LABEL_VISIBILITY);
    Info->edgecolor = (int)edge_get(graphed_edge(TheEdge),EDGE_COLOR);

    NewEdge->attrs = make_attr(ATTR_DATA,(char *)Info);

    return(NewEdge);
  }/* end of CopySedge */
  
                  
/***************************************************/
/*                                                 */
/*           IsEdgeThenRealyDelete                 */
/*                                                 */
/***************************************************/

static bool IsEdgeThenRealyDelete(TheGraph,SNode,TNode,DeletedEdges)
Sgraph  TheGraph;
Snode   SNode,TNode;
Slist   *DeletedEdges;

/*******************************************************************************/
/* Die Funktion arbeitet analog zur obigen Funktion 'IsEdgeThenDelete'. Die    */
/* Funktion 'IsEdgeThenRealyDelete' loescht die Kante (SNode,TNode) jedoch     */
/* wirklich aus der 'Sgraph'-Struktur (falls sie ex.) und sichert sie mit      */
/* Hilfe der 'Slist' 'DeletedEdges'. Da hier nur eine Kante geloescht wird,    */
/* reicht es aus , die beiden Endknoten zu Speichern, um die Kante erfolgreich */
/* widerherstellen zu koennen.                                                 */
/*******************************************************************************/
  
  {
    Sedge            ActEdge;
    DeletedListAttrs TheAttrs;
    
    for_sourcelist(SNode,ActEdge)
       if (ActEdge->tnode == TNode)
              {
                TheAttrs = talloc(struct deletedlistattrs);
                TheAttrs->ringtarget = TNode;
                TheAttrs->ringsource = SNode;
                TheAttrs->savededge = CopySedge(ActEdge);
                TheAttrs->savededgeslist = empty_slist;
                if ((*DeletedEdges) == empty_slist)
                        (*DeletedEdges) = new_slist(make_attr(ATTR_DATA,(char *)TheAttrs));
                   else add_immediately_to_slist((*DeletedEdges),make_attr(ATTR_DATA,(char *)TheAttrs));
                   
                remove_edge(ActEdge);
                return(TRUE);
              }
    end_for_sourcelist(SNode,ActEdge);
    if (TheGraph->directed)
           {
             for_targetlist(SNode,ActEdge)
                if (ActEdge->snode == TNode)
                        {
                          TheAttrs = talloc(struct deletedlistattrs);
                          TheAttrs->ringtarget = SNode;
                          TheAttrs->ringsource = TNode;
                          TheAttrs->savededge = CopySedge(ActEdge);
                          TheAttrs->savededgeslist = empty_slist;
                          if ((*DeletedEdges) == empty_slist)
                                  (*DeletedEdges) = new_slist(make_attr(ATTR_DATA,(char *)TheAttrs));
                             else add_immediately_to_slist((*DeletedEdges),make_attr(ATTR_DATA,(char *)TheAttrs));
                          remove_edge(ActEdge);
                          return(TRUE);
                        }
             end_for_targetlist(SNode,ActEdge);
           }
    return(FALSE);       
  }/* end of IsEdgeThenRealyDelete */             
                        

/***************************************************/
/*                                                 */
/*                   CopySnode                     */
/*                                                 */
/***************************************************/


static Snode CopySnode(TheNode)
Snode TheNode;

/*******************************************************************************/
/* Die Funktion 'CopySnode' erzeugt einen neuen Knoten vom Typ 'Snode' und     */
/* kopiert die Elemte der 'Snode-structure' des Knotens 'TheNode' in die       */
/* Struktur des neuen Knotens. Ausserdem werden mit Hilfe von 'Info' die       */
/* "Graphed-Parameter" des Knotens gerettet, und in die Attribute des neuen    */
/* Knotens abgelegt.                                                           */
/*******************************************************************************/

  {
    Snode  NewNode;
    NodeInfos  Info;
    
    NewNode = talloc(struct snode);
    NewNode->label = strsave(TheNode->label);
    NewNode->x = TheNode->x;
    NewNode->y = TheNode->y;
    NewNode->nr = TheNode->nr;
    NewNode->iso = TheNode->iso;
    Info = talloc(struct nodeinfos);
    Info->nodewidth = (int)node_get(graphed_node(TheNode),NODE_WIDTH);
    Info->nodeheight = (int)node_get(graphed_node(TheNode),NODE_HEIGHT);
    Info->nodenei = (int)node_get(graphed_node(TheNode),NODE_NEI);
    Info->nodenlp = (int)node_get(graphed_node(TheNode),NODE_NLP);
    Info->nodefont = (int)node_get(graphed_node(TheNode),NODE_FONT);
    Info->nodetype = (int)node_get(graphed_node(TheNode),NODE_TYPE);
    Info->nodecolor = (int)node_get(graphed_node(TheNode),NODE_COLOR);
    Info->nodelabelvisibility = (bool)node_get(graphed_node(TheNode),NODE_LABEL_VISIBILITY);

    set_nodeattrs(NewNode,make_attr(ATTR_DATA,(char *)Info));

    return(NewNode);
  }/* end of CopySnode */
  
    
/***************************************************/
/*                                                 */
/*              DeleteXYPathRealy                  */
/*                                                 */
/***************************************************/

static void DeleteXYPathRealy(SepPair,TheRing,DeletedEdges)
Slist      SepPair;
SedgeStack TheRing;
Slist      *DeletedEdges;

/*******************************************************************************/
/* Die Funktion 'DeleteXYPathRealy' arbeitet analog zur Funktion               */
/* 'DeleteXYPath' (s.o.), nur werden hier die Kanten und Knoten des Ringes,    */
/* 'TheRing', wirklich geloescht und mit Hilfe der 'Slist','DeletedEdges',     */
/* gesichert.                                                                  */
/* Die einzelenen Kanten werden auch hier mit Hilfe ihrer Endknoten gesichert, */
/* jedoch ist es hier auch notwendig Kopien der Knoten anzulegen, da diese     */
/* ebenfalls geloescht werden muessen. Da jeder Knoten 'innerhalb' des         */
/* geloeschten Ringes zu 2 Kanten gehoert, ist darauf zu achten, dass die      */
/* Kanten mit Hilfe derselben Kopie eines Knotens gesichert werden. Hierdurch  */
/* wird auch die korrekte Widerherstellung des Pfades gewaehrleistet.          */
/* (Das Loeschen der Knoten erfolgt in der Funktion 'ComputeGraphG1'.)         */
/*******************************************************************************/

  {
    DeletedListAttrs     TheDLAttrs;
    SavedEdgesListAttrs  TheSELAttrs;
    Snode                LastNode = empty_snode;  /* LastNode ist der aktuelle Anschlussknoten. */
    
    TheDLAttrs = talloc(struct deletedlistattrs);
    TheDLAttrs->ringsource = SepPairA(SepPair);
    TheDLAttrs->ringtarget = SepPairB(SepPair);
    TheDLAttrs->savededgeslist = empty_slist;
    if ((*DeletedEdges) == empty_slist)
                 (*DeletedEdges) = new_slist(make_attr(ATTR_DATA,(char *)TheDLAttrs));
            else add_immediately_to_slist((*DeletedEdges),make_attr(ATTR_DATA,(char *)TheDLAttrs));
            
    while(TheRing != EmptySedgeStack)
       {

         TheSELAttrs = talloc(struct savededgeslistattrs);
         if (SavedEdgesList((*DeletedEdges)->pre) == empty_slist)
                 SavedEdgesList((*DeletedEdges)->pre) = new_slist(make_attr(ATTR_DATA,(char *)TheSELAttrs));
            else add_immediately_to_slist(SavedEdgesList((*DeletedEdges)->pre),make_attr(ATTR_DATA,(char *)TheSELAttrs));
   
            
         if ((TheRing->edge->snode == SepPairA(SepPair)) || (TheRing->edge->snode == SepPairB(SepPair)))
                {
                  if (LastNode == empty_snode)
                          TheSELAttrs->target = LastNode = CopySnode(TheRing->edge->tnode);    /* Sichern der ersten Kante des Pfades. */
                     else TheSELAttrs->target = LastNode;     /* Sichern der letzten Kante des Pfades. */
                  TheSELAttrs->source = TheRing->edge->snode;            
                }                           
            else if ((TheRing->edge->tnode == SepPairA(SepPair)) || (TheRing->edge->tnode == SepPairB(SepPair)))
                         {
                           if (LastNode == empty_snode)
                                   TheSELAttrs->source = LastNode = CopySnode(TheRing->edge->snode);  /* Sichern der ersten Kante des Pfades. */
                              else TheSELAttrs->source = LastNode;          /* Sichern der letzten Kante des Pfades. */
                           TheSELAttrs->target = TheRing->edge->tnode;
                         }                 
                     else if (TheRing->edge->snode->nr == LastNode->nr)   /* Sichern der uebrigen Kanten des Pfades. */
                                 {
                                   TheSELAttrs->source = LastNode;
                                   TheSELAttrs->target = LastNode = CopySnode(TheRing->edge->tnode);
                                 }/*then*/
                             else{
                                   TheSELAttrs->target = LastNode;
                                   TheSELAttrs->source = LastNode = CopySnode(TheRing->edge->snode);
                                 }/*else*/
         TheSELAttrs->lsavededge = CopySedge(TheRing->edge);
         remove_edge(TheRing->edge);
         TheRing = TheRing->suc;
       }/* while */
  }/* end of DeleteXYPathRealy */



/***************************************************/
/*                                                 */
/*               ComputeGraphG1                    */
/*                                                 */
/***************************************************/

static Slist ComputeGraphG1(TheGraph,SepPairList)
Sgraph  TheGraph;
Slist   SepPairList;

/*******************************************************************************/
/* Die Funktion erzeugt aus dem urspruenglichen Graphen G den Graphen G1.      */
/* (Naeheres zur Struktur von G1 ist unter der Funktion 'ConvexityTest', oder  */
/* in [DPG4] zu finden.                                                        */
/*******************************************************************************/

  {
    Slist   ActElem,DeletedEdges = empty_slist;
    Snode   ActNode; 
    
    for_slist(SepPairList,ActElem)
       if (IsCritical(ActElem))
               if(!IsEdgeThenRealyDelete(TheGraph,SepPairA(ActElem),SepPairB(ActElem),&DeletedEdges))
                     if (HasOneRing(ActElem) != EmptySedgeStack)
                            DeleteXYPathRealy(ActElem,HasOneRing(ActElem),&DeletedEdges);
    end_for_slist(SepPairList,ActElem);
    
    /* Loeschen der Knoten, die nach Bearbeitung des Graphen G zu keiner Kante mehr gehoeren. */
    
    for_all_nodes(TheGraph,ActNode)
       if (ActNode->slist == (Sedge)NULL)
               if (TheGraph->directed)
                       {
                         if (ActNode->tlist == (Sedge)NULL)
                               remove_node(ActNode);
                       }        
                  else remove_node(ActNode);
    end_for_all_nodes(TheGraph,ActNode);                                     
    
    return(DeletedEdges);
  }/* end of ComputeGraphG1 */    
       

/***************************************************/
/*                                                 */
/*               ComputeGraphG2                    */
/*                                                 */
/***************************************************/


static void ComputeGraphG2(TheGraph,SepPairList,DeletedEdges,NewNode)
Sgraph  TheGraph;
Slist   SepPairList,*DeletedEdges;
Snode   *NewNode;

/*******************************************************************************/
/* Die Funktion erzeugt aus dem Graphen G1 (s.o.) den Graphen G2.              */
/* (Naeheres zur Struktur von G2 ist unter der Funktion 'ConvexityTest', oder  */
/* in [DPG4] zu finden.                                                        */
/*******************************************************************************/

  {
    Attributes    attrs;  
    Slist         ActElem;
    Snode         ActNode;
    
    (*DeletedEdges) = ComputeGraphG1(TheGraph,SepPairList);  /* Generieren von G1.*/

    /* Hinzufuegen des neuen Knotens. Die 'Flag' wird dazu benoetigt, dass entstehen von Mehrfachkanten */
    /* zu verhindern. Diese koennen auftreten, da ein Knoten zu mehreren kritischen Spaltungspaaren     */
    /* gehoeren kann.                                                                                   */
    
    for_all_nodes(TheGraph,ActNode)
        set_nodeattrs(ActNode,make_attr(ATTR_FLAGS,0));
    end_for_all_nodes(TheGraph,ActNode);
             
    (*NewNode) = make_node(TheGraph,attrs);
    
    for_slist(SepPairList,ActElem)
       if (IsCritical(ActElem))
              {
                if (attr_flags(SepPairA(ActElem)) == 0)
                       {
                         make_edge((*NewNode),SepPairA(ActElem),attrs);
                         attr_flags(SepPairA(ActElem)) = 1;
                       } 
                if (attr_flags(SepPairB(ActElem)) == 0)
                       {
                         make_edge((*NewNode),SepPairB(ActElem),attrs);
                         attr_flags(SepPairB(ActElem)) = 1;
                       }                         
              }  
    end_for_slist(SepPairList,ActElem);
  }/*end for ComputeGraphG2 */


/***************************************************/
/*                                                 */
/*               CopyNodeEntries                   */
/*                                                 */
/***************************************************/

static void CopyNodeEntries(SNode,TNode)
Snode  SNode,TNode;

  {
    TNode->label = strsave(SNode->label);
    TNode->x = SNode->x;
    TNode->y = SNode->y;
    TNode->nr = SNode->nr;
    TNode->iso = SNode->iso;
  }/*end of CopyNodeEntries*/ 
  
 
 
/***************************************************/
/*                                                 */
/*               CopyEdgeEntries                   */
/*                                                 */
/***************************************************/

static void CopyEdgeEntries(SEdge,TEdge)
Sedge  SEdge,TEdge;

  {
    TEdge->label = strsave(SEdge->label);
  }/*end of CopyEdgeEntries*/ 
   
   

       
/***************************************************/
/*                                                 */
/*              RecoverOldGraph                    */
/*                                                 */
/***************************************************/

static void RecoverOldGraph(TheGraph,DeletedEdges,NewNode)
Sgraph  TheGraph;
Slist   DeletedEdges;
Snode   NewNode;

/*******************************************************************************/
/* Die Funktion erzeugt mit Hilfe der 'Slist','DeletedEdges', den alten        */
/* Graphen G aus G2 (siehe auch ConvexityTest unten).                          */
/*******************************************************************************/

  {
    Slist      ActEdge,ActElem;
    Attributes attrs;
    Snode      InsertedNode,LastNode;
    Sedge      InsertedEdge;
    
    remove_node(NewNode);
    
    for_slist(DeletedEdges,ActElem)
       LastNode = empty_snode;
       if (SavedEdgesList(ActElem) == empty_slist)
              {            
                      /* Einfuegen einer einfachen Kante. */
                      
                InsertedEdge = make_edge(RingSource(ActElem),RingTarget(ActElem),attrs);
                CopyEdgeEntries(SavedEdge(ActElem),InsertedEdge);
                edge_set(create_graphed_edge_from_sedge(InsertedEdge),
                         EDGE_TYPE,GraphedEdgeInfos(SavedEdge(ActElem))->edgetype,
                         EDGE_ARROW_LENGTH,GraphedEdgeInfos(SavedEdge(ActElem))->edgearrowlength,
                         EDGE_ARROW_ANGLE,GraphedEdgeInfos(SavedEdge(ActElem))->edgearrowangle,
                         EDGE_FONT,GraphedEdgeInfos(SavedEdge(ActElem))->edgefont,
                         EDGE_LABEL_VISIBILITY,GraphedEdgeInfos(SavedEdge(ActElem))->edgelabelvisibility,
                         EDGE_COLOR,GraphedEdgeInfos(SavedEdge(ActElem))->edgecolor,0);
                free(SavedEdge(ActElem)->attrs.data);
                free(SavedEdge(ActElem));
              }  
          else{
                 /* Einfuegen eines Pfades zwischen 'RingSource' und 'RingTarget'. */
                 
                for_slist(SavedEdgesList(ActElem),ActEdge)
                   if (Source(ActEdge) == RingSource(ActElem) || Source(ActEdge) == RingTarget(ActElem)) 
                            if (LastNode == empty_snode)
                                   {
                                        /* Einfuegen der ersten Kante des Pfades. */
                                        
                                     InsertedNode = LastNode = make_node(TheGraph,attrs);
                                     node_set(create_graphed_node_from_snode(InsertedNode),
                                              NODE_SIZE,GraphedNodeInfos(Target(ActEdge))->nodewidth,GraphedNodeInfos(Target(ActEdge))->nodeheight,
                                              NODE_NEI,GraphedNodeInfos(Target(ActEdge))->nodenei,
                                              NODE_NLP,GraphedNodeInfos(Target(ActEdge))->nodenlp,
                                              NODE_FONT,GraphedNodeInfos(Target(ActEdge))->nodefont,
                                              NODE_TYPE,GraphedNodeInfos(Target(ActEdge))->nodetype,
                                              NODE_COLOR,GraphedNodeInfos(Target(ActEdge))->nodecolor,
                                              NODE_LABEL_VISIBILITY,GraphedNodeInfos(Target(ActEdge))->nodelabelvisibility,0);
                                     free(Target(ActEdge)->attrs.data);
                                     CopyNodeEntries(Target(ActEdge),InsertedNode);
                                     InsertedEdge = make_edge(Source(ActEdge),InsertedNode,attrs);
                                   }/*then*/
                               else{
                                     InsertedEdge = make_edge(Source(ActEdge),LastNode,attrs);  /* Einfuegen der letzten Kante eines Pfades. */
                                     free(Target(ActEdge));
                                   }/*else*/  
                      else  if (Target(ActEdge) == RingSource(ActElem) || Target(ActEdge) == RingTarget(ActElem)) 
                                    if (LastNode == empty_snode)
                                           {
                                                  /* Einfuegen der ersten Kante des Pfades. */
                                                  
                                             InsertedNode = LastNode = make_node(TheGraph,attrs);
                                             node_set(create_graphed_node_from_snode(InsertedNode),
                                                      NODE_SIZE,GraphedNodeInfos(Source(ActEdge))->nodeheight,GraphedNodeInfos(Source(ActEdge))->nodeheight,
                                                      NODE_NEI,GraphedNodeInfos(Source(ActEdge))->nodenei,
                                                      NODE_NLP,GraphedNodeInfos(Source(ActEdge))->nodenlp,
                                                      NODE_FONT,GraphedNodeInfos(Source(ActEdge))->nodefont,
                                                      NODE_TYPE,GraphedNodeInfos(Source(ActEdge))->nodetype,
                                                      NODE_COLOR,GraphedNodeInfos(Source(ActEdge))->nodecolor,
                                                      NODE_LABEL_VISIBILITY,GraphedNodeInfos(Source(ActEdge))->nodelabelvisibility,0);
                                             free(Source(ActEdge)->attrs.data);
                                             CopyNodeEntries(Source(ActEdge),InsertedNode);
                                             InsertedEdge = make_edge(InsertedNode,Target(ActEdge),attrs);
                                           }/*then*/
                                       else{
                                             InsertedEdge = make_edge(LastNode,Target(ActEdge),attrs);  /* Einfuegen der letzten Kante eines Pfades. */
                                             free(Source(ActEdge));
                                           }  
                               else 
                                      /* Einfuegen der "inneren" Kanten eines Pfades. */
                                      
                                    if (Source(ActEdge)->nr == LastNode->nr)
                                           {
                                             InsertedNode = make_node(TheGraph,attrs);
                                             node_set(create_graphed_node_from_snode(InsertedNode),
                                                      NODE_SIZE,GraphedNodeInfos(Target(ActEdge))->nodewidth,GraphedNodeInfos(Target(ActEdge))->nodeheight,
                                                      NODE_NEI,GraphedNodeInfos(Target(ActEdge))->nodenei,
                                                      NODE_NLP,GraphedNodeInfos(Target(ActEdge))->nodenlp,
                                                      NODE_FONT,GraphedNodeInfos(Target(ActEdge))->nodefont,
                                                      NODE_TYPE,GraphedNodeInfos(Target(ActEdge))->nodetype,
                                                      NODE_COLOR,GraphedNodeInfos(Target(ActEdge))->nodecolor,
                                                      NODE_LABEL_VISIBILITY,GraphedNodeInfos(Target(ActEdge))->nodelabelvisibility,0);
                                             free(Target(ActEdge)->attrs.data);
                                             CopyNodeEntries(Target(ActEdge),InsertedNode);
                                             InsertedEdge = make_edge(LastNode,InsertedNode,attrs);
                                             LastNode = InsertedNode;
                                             free(Source(ActEdge));
                                           }
                                       else{
                                             InsertedNode = make_node(TheGraph,attrs);
                                             node_set(create_graphed_node_from_snode(InsertedNode),
                                                      NODE_SIZE,GraphedNodeInfos(Source(ActEdge))->nodewidth,GraphedNodeInfos(Source(ActEdge))->nodeheight,
                                                      NODE_NEI,GraphedNodeInfos(Source(ActEdge))->nodenei,
                                                      NODE_NLP,GraphedNodeInfos(Source(ActEdge))->nodenlp,
                                                      NODE_FONT,GraphedNodeInfos(Source(ActEdge))->nodefont,
                                                      NODE_TYPE,GraphedNodeInfos(Source(ActEdge))->nodetype,
                                                      NODE_COLOR,GraphedNodeInfos(Source(ActEdge))->nodecolor,
                                                      NODE_LABEL_VISIBILITY,GraphedNodeInfos(Source(ActEdge))->nodelabelvisibility,0);
                                             free(Source(ActEdge)->attrs.data);
                                             CopyNodeEntries(Source(ActEdge),InsertedNode);
                                             InsertedEdge = make_edge(InsertedNode,LastNode,attrs);
                                             LastNode = InsertedNode;
                                             free(Target(ActEdge));
                                           }
                    CopyEdgeEntries(LSavedEdge(ActEdge),InsertedEdge);
                    edge_set(create_graphed_edge_from_sedge(InsertedEdge),
                         EDGE_TYPE,GraphedEdgeInfos(LSavedEdge(ActEdge))->edgetype,
                         EDGE_ARROW_LENGTH,GraphedEdgeInfos(LSavedEdge(ActEdge))->edgearrowlength,
                         EDGE_ARROW_ANGLE,GraphedEdgeInfos(LSavedEdge(ActEdge))->edgearrowangle,
                         EDGE_FONT,GraphedEdgeInfos(LSavedEdge(ActEdge))->edgefont,
                         EDGE_LABEL_VISIBILITY,GraphedEdgeInfos(LSavedEdge(ActEdge))->edgelabelvisibility,
                         EDGE_COLOR,GraphedEdgeInfos(LSavedEdge(ActEdge))->edgecolor,0);
                   free(LSavedEdge(ActEdge)->attrs.data);
                   free(LSavedEdge(ActEdge));                       
                end_for_slist(SavedEdgesList(ActElem),ActEdge);
                free_slist(SavedEdgesList(ActElem));
              }/*else*/                                                      
    end_for_slist(DeletedEdges,ActElem);
    free_slist(DeletedEdges);
  }/* end of RecoverOldGraph */



/***************************************************/
/*                                                 */
/*                  FindVCycle                     */
/*                                                 */
/***************************************************/

static SedgeStack FindVCycle(V)
Snode  V;

/*******************************************************************************/
/* Die Funktion 'FindVCycle' generiert den "V-Cycle" im Graphen G2 und gibt    */
/* eine Liste der zugehoerigen Kante zurueck. Diese Liste (Stack) ist eine     */
/* einfache Liste von 'Sedges' , die noch nicht zum gezielten Durchlaufen      */
/* geeignet ist.                                                               */
/*******************************************************************************/

  {
    Snode       StartNode;
    Slist       FacialEdge;
    SedgeStack  TheCycle = EmptySedgeStack;
    
    StartNode = TargetNodeGet(OrderedEdgeList(V));    /* Gestartet wird am Zielknoten einer beliebigen Kante, die von V ausgeht. */
    FacialEdge = EdgeCopy(OrderedEdgeList(V))->suc;
    while(TargetNodeGet(FacialEdge) != StartNode)
       {
         AppendSedgeStack(&TheCycle,Edge(FacialEdge));
         if (TargetNodeGet(EdgeCopy(FacialEdge)->suc) != V)
                 FacialEdge = EdgeCopy(FacialEdge)->suc;    /* Mit V verbundene Kanten werden nicht beachtet. */
            else FacialEdge = EdgeCopy(FacialEdge)->suc->suc;
       }/*while*/
    AppendSedgeStack(&TheCycle,Edge(FacialEdge));
    return(TheCycle);
  }/* end of FindVCycle */                


 
/***************************************************/
/*                                                 */
/*    MakeExtendibleFacialCycleFromVCycle          */
/*                                                 */
/***************************************************/


static Slist MakeExtendibleFacialCycleFromVCycle(TheVCycle)
SedgeStack  TheVCycle;

/*******************************************************************************/
/* Die Funktion dient dazu, aus dem vorliegenden 'Stack' von Kanten,'TheCycle',*/
/* eine "brauchbare" 'Slist' fuer die Flaechenbegrenzung zu konstruieren.      */
/* Es wird dabei einfach im neu eingebetteten Graphen nach den Kanten gesucht  */
/* und diese werden dann wie ueblich in eine entsprechende 'Slist' abgelegt,   */
/* so dass ein gezieltes Durchlaufen der Flaechenbegrenzung moeglich wird.     */
/*******************************************************************************/

  {
    Slist TheExtCycle = empty_slist,ActEdge;
    Snode TNode;
    
    if ((TheVCycle->edge->snode == TheVCycle->suc->edge->snode) || (TheVCycle->edge->snode == TheVCycle->suc->edge->tnode))
           {
             TNode = TheVCycle->edge->snode;
             for_slist(OrderedEdgeList(TheVCycle->edge->tnode),ActEdge)
                if (TargetNodeGet(ActEdge) == TNode)
                       {
                         TheExtCycle = new_slist(make_attr(ATTR_DATA,(char *)ActEdge->attrs.data));
                         break; 
                       }    
             end_for_slist(OrderedEdgeList(TNode),ActEdge);
           }  
       else{
             TNode = TheVCycle->edge->tnode;
             for_slist(OrderedEdgeList(TheVCycle->edge->snode),ActEdge)
                if (TargetNodeGet(ActEdge) == TNode)
                       {
                         TheExtCycle = new_slist(make_attr(ATTR_DATA,(char *)ActEdge->attrs.data));
                         break; 
                       }    
             end_for_slist(OrderedEdgeList(TNode),ActEdge);
           }  
       
    TheVCycle = TheVCycle->suc;
       
    while(TheVCycle != EmptySedgeStack)
       {
         if (TNode == TheVCycle->edge->snode)
                {
                  for_slist(OrderedEdgeList(TNode),ActEdge)
                     if (TargetNodeGet(ActEdge) == TheVCycle->edge->tnode)
                            {
                              add_immediately_to_slist(TheExtCycle,make_attr(ATTR_DATA,(char *)ActEdge->attrs.data));
                              break;   
                            }
                  end_for_slist(OrderedEdgeList(TNode),ActEdge);
                  TNode = TheVCycle->edge->tnode;
                } 
            else{
                  for_slist(OrderedEdgeList(TNode),ActEdge)
                     if (TargetNodeGet(ActEdge) == TheVCycle->edge->snode)
                            {
                              add_immediately_to_slist(TheExtCycle,make_attr(ATTR_DATA,(char *)ActEdge->attrs.data));
                              break;   
                            }
                  end_for_slist(OrderedEdgeList(TNode),ActEdge);
                  TNode = TheVCycle->edge->snode;
                }                      
                    
         TheVCycle = TheVCycle->suc;
       }/*while*/
    return(TheExtCycle);
  }/*end of MakeExtendibleFacialCycleFromVCycle */     
                            
        
/***************************************************/
/*                                                 */
/*             RemoveSepPairList                   */
/*                                                 */
/***************************************************/

static void RemoveSepPairList(TheSepPairList)
Slist TheSepPairList;

/*******************************************************************************/
/* Die Prozedur gibt den fuer die Liste der Spaltungspaare benoetigten         */
/* Speicherplatz frei.                                                         */
/*******************************************************************************/

  {
    Slist  ActElem;
    
    for_slist(TheSepPairList,ActElem)
       RemoveRing(HasOneRing(ActElem));
       free_slist(VirtualEdgeList(ActElem));
       free(ActElem->attrs.data);
    end_for_slist(TheSepPairList,ActElem);
    free_slist(TheSepPairList);
  }/* end of RemoveSepPairList */  
  
     
/***************************************************/
/*                                                 */
/*           RemoveSplitCompList                   */
/*                                                 */
/***************************************************/

static void RemoveSplitCompList(TheSplitCompList)
Slist TheSplitCompList;

/*******************************************************************************/
/* Die Prozedur gibt den fuer die Liste der Splitkomponenten benoetigten       */
/* Speicherplatz frei.                                                         */
/*******************************************************************************/

  {
    Slist  ActElem;
    
    for_slist(TheSplitCompList,ActElem)
       free_slist(SplitCompEdges(ActElem));
       free(ActElem->attrs.data);
    end_for_slist(TheSplitCompList,ActElem);
    free_slist(TheSplitCompList);
  }/* end of RemoveSplitCompList */
  
  
  
/***************************************************/
/*                                                 */
/*             RemoveOrderedEdgeList               */
/*                                                 */
/***************************************************/

static void RemoveOrderedEdgeList(TheNode)
Snode  TheNode;

  {
    Slist  ActEdge;
    
    for_slist(OrderedEdgeList(TheNode),ActEdge)
       free(ActEdge->attrs.data);
    end_for_slist(OrderedEdgeList(TheNode),ActEdge);
    free_slist(OrderedEdgeList(TheNode));
  }/* end of RemoveOrderedEdgeList */
  
  
/***************************************************/
/*                                                 */
/*             RemoveOrderedNEAttrs                */
/*                                                 */
/***************************************************/


void RemoveOrderedNEAttrs(TheGraph)
Sgraph  TheGraph;

/*******************************************************************************/
/* Mit Hilfe der Prozedur 'RemoveOrderedEdgeList', wird hier,der durch die     */
/* Prozedur 'InitEmbededNodeEdgeAttrs' belegte Speicherplatz, freigegeben.     */
/*******************************************************************************/

  {
    Snode  ActNode;
    
    for_all_nodes(TheGraph,ActNode)
       RemoveOrderedEdgeList(ActNode);
       free(ActNode->attrs.data);
    end_for_all_nodes(TheGraph,ActNode);
  }/* end of RemoveOrderedNEAttrs */  
  
  
/***************************************************/
/*                                                 */
/*                ConvexityTest                    */
/*                                                 */
/***************************************************/


Slist  ConvexityTest(TheGraph)
Sgraph  TheGraph;

/*******************************************************************************/
/* 'ConvexityTest' entspricht dem Algorithmus "CONVEX-TEST" der in [DPG4]      */
/* beschrieben ist.                                                            */
/* Die Funktion arbeitet im wesentlichen in 2 Schritten:                       */
/* (1)  Als erstes werden, mit Hilfe der Funktionen 'FindSepPairs' und         */
/*	und 'DetermineSplitComponents', die Spaltungspaare des Graphen         */
/*	ermittelt. Diese Paare werden dann in drei Klassen aufgeteilt.         */
/*	PSP = 'prime' Spaltungspaare,CSP = kritische Spaltungspaare und        */
/*	FSP = verbotene Spaltungspaare. Diese Einteilung geschieht durch die   */
/*	Funktionen bzw. Prozeduren 'FindThreeConnectedComp' und 'CheckSepPairs'*/
/*	Wenn nun die Klasse FSP nicht leer ist, bricht das Programm mit der    */
/*	Meldung, dass der Graph nicht konvex zeichenbar ist, ab.               */
/*	Sind die Klassen CSP und FSP leer, so kann jede Flaechenbegrenzung     */
/*	im Graphen als Umkreis verwendet werden. Mit Hilfe der Funktion        */
/*	'FindFacialCycle' wird eine solche ermittelt und als Ergebnis          */
/*	zurueckgegeben.D.h. in diesem Fall kann der Graph konvex gezeichnet    */
/*	werden. Fuer den Fall das genau 1 kritisches Spaltungspaar vorhanden   */
/*	ist, tritt die Funktion 'FacialCycleOfSevenPossibleGraphs' in Aktion,  */
/*	auch in diesem Fall kann der Graph konvex gezeichnet werden.           */
/*									       */
/* (2)  Hier wird der Fall behandelt, dass der Graph mindestens 2 kritische    */
/*	Spaltungspaare besitzt.(Dies ist der problematische Fall!)             */
/*	Der urspruengliche Graph G wird nun wie folgt in einen Graphen G2      */
/*	umgewandelt:							       */
/*   (a)Es wird ein Graph G1 erzeugt indem man folgende Operationen fuer jedes */
/*	der kritischen Spaltungspaare {x,y} ausfuehrt:                         */
/*	  Falls die Kante (x,y) im Graphen ex. wird diese geloescht,           */
/*         ansonsten,falls genau eine {x,y}-Splitkomponente vom Typ Ring ex.,  */
/*	   wird der x-y-Pfad dieser Komponente geloescht.                      */
/*   (b)Aus dem Graphen G1 wird nun der Graph G2 wie folgt generiert. Es wird  */
/*	ein neuer Knoten V in den Graphen eingefuegt, der mit allen Knoten     */
/*	der kritischen Spaltungspaare verbunden wird.                          */
/*									       */
/*  Nun gilt:							               */
/*	S ist genau dann eine 'extendible' Flaechenbegrenzung, wenn (i) G2     */
/*	planar ist und (ii) S der 'V-Cycle' in G2 ist, d.h. betrachtet man     */
/*	die Kanten mit denen V verbunden ist als nicht existent, liegt V       */
/*	in der Flaeche, die durch S begrenzt ist.                              */
/*									       */
/* Ist G2 nicht planar, bricht das Programm mit der Meldung ab, dass kein      */
/* "V-Cycle" ex.; d.h. G ist nicht konvex zeichenbar. Andernfalls wird mit der */
/* Funktion 'FindVCycle' bzw 'MakeExtendibleFacialCycleFromVCycle' die         */
/* 'extendible' Flaechenbegrenzung in G berechnet.                             */
/*									       */
/*									       */
/* Zur Implementierung von (2) ist eine Einbettungsfunktion notwendig. Diese   */
/* Funktion 'embed 'wurde zur Verfuegung gestellt. Als Eingabe erwartet sie    */
/* einen "unbehandelten" 'Sgraph', in dessen Knotenattributen sie jeweils die  */
/* geordnete Kantenliste in form einer 'Slist' anlegt.                         */
/* Um diese Funktion auch verwenden zu koennen, muessen die Knoten bzw. Kanten */
/* "wirklich' geloescht und natuerlich gesichert werden. Dies geschieht in der */
/* Prozedur 'ComputeGraphG2' mit Hilfe der Datenstruktur 'DeletedListAttrs'    */
/* (siehe auch ConvexTest.h). Nach Testen des Graphen G2 muss nun der "alte"   */
/* Graph widerhergestellt werden und erneut die Einbettung des "alten"         */
/* Graphen berechnet werden. Das Widerherstellen des Graphen realisiert die    */
/* Prozedur 'RecoverOldGraph'. Hier muss darauf geachtet werden, dass die      */
/* eventuell gefundenen Flaechenbegrenzung auch als solche erhalten bleibt.    */
/*******************************************************************************/

  {
    Slist       ActElem,ForbiddenPair,SplitList = empty_slist,SepPairList = empty_slist,TheFacialCycle = empty_slist,DeletedEdges;
    int         NumberOfCriticalPairs = 0;
    SedgeStack  TheVCycle,Help;
    Snode       ActNode,NewNode,CenterNode;
    Attributes  attrs;
    
    FindSeperationPairs(TheGraph);
    
    DetermineSplitComponents(TheGraph,&SplitList,&SepPairList);   
    FindThreeConnectedComp(SplitList);

    ForbiddenPair = CheckSepPairs(SepPairList,SplitList,&NumberOfCriticalPairs);
    RemoveSplitAttrs(TheGraph);

        
    if (ForbiddenPair != empty_slist)
            {
                     /* Es ex. ein verbotenes Spaltungspaar; der Graph kann deshalb nicht konvex gezeichnet werden ! */
                     
              error  ("The graph cannot be drawn convex :\n");
              message(" {%s,%s} is a forbidden separaration pair.\n",
              	SepPairA(ForbiddenPair)->label,
              	SepPairB(ForbiddenPair)->label);
            }
        else if (embed(TheGraph) == SUCCESS)
                    {
                          /* Der Graph ist planar. */
                          
                      if (NumberOfCriticalPairs == 0)
                              { 
                                   /* Der Graph besitzt kein kritisches und kein verbotenes Spaltungspaar. */
                                     
                                message("All faces are extendible.\n");
                                InitEmbededNodeEdgeAttrs(TheGraph);
                                
                                TheFacialCycle = FindFacialCycle(TheGraph);   /* Eine Flaechenbegrenzung mit moeglichst vielen */
                                                                              /* Knoten wird gesucht und als Ergebnis          */
                                                                              /* zurueckgegeben.                               */
                              }     
                          else if (NumberOfCriticalPairs == 1)
                                       {
                                             /* Es ex. genau ein kritisches und kein verbotenes Spaltungspaar.*/
                                            
                                          message("There is exactly one critical separation pair.\n");
                                          InitEmbededNodeEdgeAttrs(TheGraph);
                                          TheFacialCycle = FacialCycleOfSevenPossibleGraphs(TheGraph,SepPairList);
                                       }  
                                  else {     /* Es ex. mindestens 2 kritische Spaltungspaare und kein verbotenes. */
                         
                                         message("There are at least two critical separation pairs.\n");
                                         
                                         /* Loeschen der hier nicht benoetigten geoerdneten Kantenliste, die durch 'embed' */
                                         /* angelegt wurde.                                                                */
                                         
                                         for_all_nodes(TheGraph,ActNode)
                                             free_slist(EmbededList(ActNode));
                                             free(ActNode->attrs.data);
                                         end_for_all_nodes(TheGraph,ActNode);
                                             
                                         ComputeGraphG2(TheGraph,SepPairList,&DeletedEdges,&NewNode);     /* Generieren des oben beschriebenen */
                                                                                                          /* Graphen G2.                       */
                                          
                                 
                                         if (embed(TheGraph) == SUCCESS) 
                                                {          
                                                       /* G2 ist planar. Der "V-Cycle" wird bestimmt. */
                                       
                                                   InitEmbededNodeEdgeAttrs(TheGraph);
                                          
                                                   TheVCycle = FindVCycle(NewNode);
                                                   Help = TheVCycle;
                                                                                   
                                                   RemoveOrderedNEAttrs(TheGraph);   
                                
                                                   RecoverOldGraph(TheGraph,DeletedEdges,NewNode);   /* Der "alte" Graph wird widerhergestellt. */
                                          
                                                   /* Mit Hilfe des neuen Knotens 'CenterNode', der mit allen */
                                                   /* Knoten der gefundenen Flaechenbegrenzung verbunden ist, */
                                                   /* wird gewaehrleistet,dass die Flaechenbegrenzung auch im */
                                                   /* "alten" Graphen wieder eine solche ist.                 */
                                          
                                                   CenterNode = make_node(TheGraph,attrs);
                                          
                                                   for_all_nodes(TheGraph,ActNode)                      /* Die 'Flag' dient dazu, dass  */
                                                      set_nodeattrs(ActNode,make_attr(ATTR_FLAGS,0));   /* Entstehen von MehrfachKanten */
                                                   end_for_all_nodes(TheGraph,ActNode);                 /* zu verhindern.               */
                                          
                                                   Help = TheVCycle;
                                                   while(Help != EmptySedgeStack)
                                                      {
                                                            /* Erzeugen der neuen Kanten. */
                                                   
                                                        if (attr_flags(Help->edge->snode) == 0)
                                                               {
                                                                 make_edge(CenterNode,Help->edge->snode,attrs);
                                                                 attr_flags(Help->edge->snode) = 1;
                                                               }
                                                        if (attr_flags(Help->edge->tnode) == 0)
                                                               {
                                                                 make_edge(CenterNode,Help->edge->tnode,attrs);
                                                                 attr_flags(Help->edge->tnode) = 1;
                                                               } 
                                                        Help = Help->suc;       
                                                      }/* while */ 
                                                                  
                                                   /* Einbettung */

                                                   if (embed(TheGraph) == SUCCESS)
                                                         {  
                                                                /* Einbettung des "alten" Graphen. */
                                                
                                                           InitEmbededNodeEdgeAttrs(TheGraph);
                                                  
                                                           /* Entfernen des Hilfsknotens 'CenterNode' und seiner Kanten. */
                                                  
                                                           for_slist(OrderedEdgeList(CenterNode),ActElem)
                                                              OrderedEdgeList(TargetNodeGet(ActElem)) = subtract_from_slist(OrderedEdgeList(TargetNodeGet(ActElem)),EdgeCopy(ActElem)->attrs);
                                                              Deg(TargetNodeGet(ActElem))--; 
                                                           end_for_slist(OrderedEdgeList(CenterNode),ActElem);
                                                  
                                                           RemoveOrderedEdgeList(CenterNode);
                                                           remove_node(CenterNode);

                                                           /* Ausgabe der Struktur des eingebeteten Graphen. */
                                                           /*for_all_nodes(TheGraph,ActNode)
                                                              printf("%s     ",ActNode->label);
                                                              for_slist(OrderedEdgeList(ActNode),ActElem)
                                                                 printf("%s  ",TargetNodeGet(ActElem)->label);
                                                              end_for_slist(OrderedEdgeList(ActNode),ActElem);
                                                              printf("\n");
                                                           end_for_all_nodes(TheGraph,ActNode);*/
                                                  
                                                           /* Erzeugen der "brauchbaren" Datenstruktur fuer die Flaechenbegrenzung. */
                                                  
                                                           TheFacialCycle = MakeExtendibleFacialCycleFromVCycle(TheVCycle);
                                                         }
                                                     else  /* Dieser Fall duerfte nach Aussagen der Saetze bzw. Lemmata in [DPG4]  */
                                                           /* nicht auftreten. Hier wird zwar eine brauchbare Flaechenbegrenzung   */
                                                           /* in G2 gefunden, der urspruengliche Graph enthaelt diese              */
                                                           /* Flaechenbegrenzung jedoch nicht.                                     */
                                                           warning("Die 'extendible' Flaechenbegrenzung ist \"verschwunden\" !!\n");      
                                                }
                                            else{
                                                  /* G2 ist nicht planar, dass heisst es ex. kein "V-Cycle" und */
                                                  /* der Graph kann nicht konvex gezeichnet werden.             */
                                         
                                                  RecoverOldGraph(TheGraph,DeletedEdges,NewNode);
                                                  error  ("The graph cannot be drawn convex :\n");
                                                  message("  there is no face containing all critical separation pairs.\n");
                                                }  
                                     }/*else*/
                    }                 
                else{
                      error ("The graph is not planar. no convex drawing.\n");
                    }                    
    /* Freigeben des Speicherplatzes fuer die Liste der Spaltungspaare,bzw. die Liste der Splitkomponenten. */                                                               
    RemoveSepPairList(SepPairList);
    RemoveSplitCompList(SplitList);
    
   
    return(TheFacialCycle); 
  }/* end of ConvexityTest */

/*****************************************************************************************/  
            


  
  
