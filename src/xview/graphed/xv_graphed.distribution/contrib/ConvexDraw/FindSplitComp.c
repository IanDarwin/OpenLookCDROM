/* (C) Universitaet Passau 1986-1991 */
/*1990-91 by Stefan Jockenhoevel and Gerd Nienhaus.*/

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE FindSplitComp.c                                            **/
/**                                                                                     **/
/** Das Modul implementiert die im folgenden Artikel beschriebenen Funktionen und       **/
/** Prozeduren:                                                                         **/
/**                J.E.Hopcroft und R.E. Tarjan:                                        **/
/**                                                                                     **/
/**                DIVIDING A GRAPH INTO TRICONNECTED COMPONENTS                        **/
/**                                                                                     **/
/**          (erschienen in : SIAM J. Comput. Vol. 2, Nr. 3 ,September 1973)            **/
/**                                                                                     **/
/** Dieses Modul bezieht sich auf den Abschnitt 4 des Artikels:                         **/
/**          'Finding split components.'                                                **/
/**                                                                                     **/
/**                                                                                     **/
/**   Exportiert wird die Prozedur :                                                    **/
/**     DetermineSplitComponents(Sgraph TheGraph,Slist SplitList,Slist SepPairList)     **/
/*****************************************************************************************/
/*****************************************************************************************/
/**  In der Prozedur FindSplitComp sind einige Verbesserungen gegenueber dem Original   **/
/**  PATHSEARCH notwendig gewesen, da das Original offensichtlich an einigen Stellen    **/
/**  unzureichend (oder zumindest ungenau beschrieben) ist.                             **/
/**  Die Aenderungen sind im Prozedur-Code gekennzeichnet und dokumentiert.             **/
/*****************************************************************************************/
/*****************************************************************************************/

#include "paths.h"
#include STDH
#include SGRAPHH
#include SLISTH
#include "NodeEdgeAttrs.h" 
#include "Utilities.h"
#include "FindSplitComp.h"
  

/***************************************************/
/*                                                 */
/*               FindSepPair                       */
/*                                                 */
/***************************************************/

 
static Slist FindSepPair(NodeA,NodeB,SepPairList)
Snode     NodeA,NodeB;
Slist     SepPairList;

/*******************************************************************************/
/*  Die Funktion sucht das Paar {NodeA,NodeB} in der Liste der Spaltungspaare  */
/*  'SepPairList', und gibt den Zeiger auf das entsprechende 'Slist'-Element   */
/*  zurueck. Die Reihenfolge der Knoten des Paares ist nicht relevant.         */
/*******************************************************************************/

  {
    Slist  ActSepPair;
  
    for_slist(SepPairList,ActSepPair)
       if ((NodeA == SepPairA(ActSepPair) && NodeB == SepPairB(ActSepPair))||
           (NodeA == SepPairB(ActSepPair) && NodeB == SepPairA(ActSepPair)))
               return(ActSepPair);
    end_for_slist(SepPairList,ActSepPair);
    return(empty_slist);    /* Das Paar {NodeA,NodeB} ist noch nicht in 'SepPairList' enthalten. */ 
  }/*end of FindSepPair*/

               
/***************************************************/
/*                                                 */
/*            AddToSepPairList                     */
/*                                                 */
/***************************************************/


static void AddToSepPairList(FirstNode,SecondNode,SepPairList)
Snode   FirstNode,SecondNode;
Slist   *SepPairList;

/*******************************************************************************/
/*  Die Prozedur legt in der Variablen 'SepPairList' eine Liste, der im        */
/*  Graphen vorhandenen SpaltungsPaare, an. Zu beachten ist das nicht wirklich */
/*  alle Spaltungspaare gespeichert werden, sondern nur die, die bei der       */
/*  Zerlegung des Graphen in seine Splitkomponenten gefunden werden. Diese     */
/*  ist offensichtlich nicht eindeutig. Die Spaltungspaare, die nicht gefunden */
/*  werden, sind offensichtlich nicht 'prime' und deshalb fuer den Algorithmus */
/*  nicht relevant.                                                            */
/*******************************************************************************/

  {
    SepPairListAttrs  TheAttrs;
    Slist             ActSepPair;
    bool              contains = FALSE;

    /*  In der folgenden Schleife wird untersucht, ob das Paar {FirstNode,SecondNode} eventuell */
    /*  schon in der Liste der Spaltungspaare eingetragen ist.                                  */
    
    for_slist((*SepPairList),ActSepPair)
       if ((FirstNode == SepPairA(ActSepPair) && SecondNode == SepPairB(ActSepPair))||
           (FirstNode == SepPairB(ActSepPair) && SecondNode == SepPairA(ActSepPair)))
               {
                 contains = TRUE;
                 BelongsToSepPair(FirstNode) = BelongsToSepPair(SecondNode) = ActSepPair;
                 break;
               }
    end_for_slist((*SepPairList),ActSepPair);
    
    if (!contains)
         {  
           /* Initialisierung der Listen-Attribute. */
                       
           TheAttrs = talloc(struct seppairlistattrs);
           TheAttrs->anode = FirstNode;
           TheAttrs->bnode = SecondNode;
           TheAttrs->virtualedgelist = empty_slist;
           TheAttrs->prime = FALSE;
           TheAttrs->forbidden = FALSE;
           TheAttrs->critical = FALSE;
           TheAttrs->exactlyonering = EmptySedgeStack;
           
           /* Einfuegen des neuen Spaltungspaares. */
           
           if ((*SepPairList) == empty_slist)
                   (*SepPairList) = new_slist(make_attr(ATTR_DATA,(char *)TheAttrs));
              else add_immediately_to_slist((*SepPairList),make_attr(ATTR_DATA,(char *)TheAttrs));
           BelongsToSepPair(FirstNode) = BelongsToSepPair(SecondNode) = (*SepPairList)->pre;   
         }         
  }/*end of AddToSepPairList*/
 
                 
/***************************************************/
/*                                                 */
/*            AddToVirtualEdgeList                 */
/*                                                 */
/***************************************************/


static void AddToVirtualEdgeList(TheEdge,SepPair)
Sedge   TheEdge;
Slist   SepPair;

/*******************************************************************************/
/*  Es wird die virtuelle Kante 'TheEdge' in die Liste der virtuellen Kanten   */
/*  von SepPair eingetragen. Diese Liste enthaelt alle virtuellen Kanten deren */
/*  Endpunkte die Knoten des Spaltungspaares 'SepPair' sind.                   */
/*******************************************************************************/

  {
    if (VirtualEdgeList(SepPair) == empty_slist)
             VirtualEdgeList(SepPair) = new_slist(make_attr(ATTR_DATA,(char *)TheEdge));
       else  add_immediately_to_slist(VirtualEdgeList(SepPair),make_attr(ATTR_DATA,(char *)TheEdge));
  }/*end of AddToVirtualEdgeList*/
 
                 
/***************************************************/
/*                                                 */
/*            LinkVirtualEdges                     */
/*                                                 */
/***************************************************/


static void LinkVirtualEdges(First,Second)
Sedge   First,Second;

/*******************************************************************************/
/*  Die Prozedur verbindet die beiden virtuellen Kanten First und Second,      */
/*  die durch einen Spaltungsvorgang entstanden sind.                          */
/*  Diese direkte Verzeigerung ermoeglicht das "Springen" von einer            */
/*  Splitkomponente in eine unmittelbar angrenzende Komponente.                */
/*******************************************************************************/

  {
    CorrespondingEdge(First) = Second;
    CorrespondingEdge(Second) = First;
  } 
 
   
/***************************************************/
/*                                                 */
/*            MakeVirtualEdge                      */
/*                                                 */
/***************************************************/


static Slist MakeVirtualEdge(SNode,TNode,Nr)
Snode  SNode,TNode;
int    Nr;

/*******************************************************************************/
/*  Die Funktion erzeugt eine virtuelle Kante von SNode nach TNode mit der     */
/*  Nummer Nr. Gleichzeitig werden alle notwendigen Kanten- bzw. Listen-       */
/*  Attribute initialisiert, bzw. laut Eingabe gesetzt.                        */
/*  Zu beachte: Es wird ein Elemt vom Typ Slist und NICHT vom Typ Sedge        */
/*  zurueckgegeben.                                                            */
/*******************************************************************************/

  {
    MyEdgeListAttrs TheEdgeListAttrs;
    MyEdgeAttrs     TheEdgeAttrs;
    Sedge           TheEdge;
    
    /* Setzen der Kantenattribute. */
    
    TheEdgeAttrs = talloc(struct myedgeattrs);
    TheEdgeAttrs->virtualedgenr = Nr;
    TheEdgeAttrs->correspondingedge = empty_sedge;
    TheEdgeAttrs->splitcomp = empty_slist;
    TheEdgeAttrs->activ = TRUE;
    
    /* Erzeugen der neuen Kante. */
       
    TheEdge = talloc(struct sedge);
    TheEdge->snode = SNode;
    TheEdge->tnode = TNode;
    TheEdge->attrs = make_attr(ATTR_DATA,(char *)TheEdgeAttrs);
    
    /* Setzen der Attribute fuer das erzeugte ListenElement. */
    
    TheEdgeListAttrs = talloc(struct myedgelistattrs);
    TheEdgeListAttrs->edgeofsourcelist = TRUE;
    TheEdgeListAttrs->reached = FALSE;    
    TheEdgeListAttrs->edge = TheEdge;
    
    return(new_slist(make_attr(ATTR_DATA,(char *)TheEdgeListAttrs)));
  }/*end of MakeVirtualEdge*/  


/***************************************************/
/*                                                 */
/*              AddToNewComp                       */
/*                                                 */
/***************************************************/


static void AddToNewComp(NewEdge,SplitComp)
Slist   NewEdge,*SplitComp;

/*******************************************************************************/
/*  Die Prozedur baut in der Variablen 'SplitComp' eine neue Splitkomponente   */
/*  auf, d.h. eine Liste von Kanten ('Slist'-Elemente !) die eine Split-       */
/*  komponente bilden.                                                         */
/*******************************************************************************/

  {
    if ((*SplitComp) == empty_slist)
             (*SplitComp) = new_slist(make_attr(ATTR_DATA,(char *)EdgePointer(NewEdge)));
       else  add_immediately_to_slist((*SplitComp),make_attr(ATTR_DATA,(char *)EdgePointer(NewEdge)));
  }/*end of AddToNewComp*/
 
            
/***************************************************/
/*                                                 */
/*        AddToSplitCompList                       */
/*                                                 */
/***************************************************/


static void AddToSplitCompList(SplitComp,CompType,SplitList)
Slist           SplitComp;
SplitCompType   CompType;
Slist           *SplitList;

/*******************************************************************************/
/*  Die Prozedur legt in der Variablen 'SplitList' eine Liste der Split-       */
/*  komponenten des Graphen an.                                                */
/*  Weiterhin wird in den Attributen jeder Kante, der eingefuegten Split-      */
/*  komponente, ein Zeiger auf das zugehoerige, neu erzeugte, Element der      */
/*  Liste der Splitkomponenten angelegt.                                       */
/*  Dadurch erhaelt man den direkten Zugriff von jeder Kante auf die Split-    */
/*  komponente, die die Kante enthaelt.                                        */
/*******************************************************************************/

 {
   SplitCompListAttrs  TheAttrs;
   Slist               ActEdge;
   
   /* Setzen der Listenattribute. */
   
   TheAttrs = talloc(struct splitcomplistattrs);
   TheAttrs->type = CompType;
   TheAttrs->edgelist = SplitComp;
   TheAttrs->reached = FALSE;
   
   /* Einfuegen der neuen Splitkomponennte. */
   
   if ((*SplitList) == empty_slist)
            (*SplitList) = new_slist(make_attr(ATTR_DATA,(char *)TheAttrs));
      else  add_immediately_to_slist((*SplitList),make_attr(ATTR_DATA,(char *)TheAttrs));
      
   /* Setzen des Zeigers auf das 'SplitList'-Element fuer jede Kante der Splitkomponente. */
   
   for_slist(SplitComp,ActEdge)
      BelongsToSplitComp(EdgeOfSplitComp(ActEdge)) = (*SplitList)->pre;
   end_for_slist(SplitComp,ActEdge);   
  }/*end of AddToSplitCompList*/
 
                
/***************************************************/
/*                                                 */
/*                PushTStack                       */
/*                                                 */
/***************************************************/


static void  PushTStack(TheTStack,HNode,ANode,BNode)
TripleStack  *TheTStack;
Snode  HNode,ANode,BNode;

/*******************************************************************************/
/*  Die Funktion baut in der Variablen 'TheTStack' den im Artikel genau        */
/*  beschriebenen TStack auf.                                                  */
/*  Der implementierte Stack ist allerdings kein Stack von Tripeln ganzer      */
/*  Zahlen, sonder von Tripeln vom Typ 'Snode'. (siehe auch FindSpliComp.h)    */
/*******************************************************************************/

  {
    TripleStack  HelpPointer;
    
    HelpPointer = talloc(struct triplestack);
    HighestNode(HelpPointer) =  HNode;
    ASepNode(HelpPointer) = ANode;
    BSepNode(HelpPointer) = BNode;
    HelpPointer->suc = (*TheTStack);
    (*TheTStack) = HelpPointer;
  }/* end of PushTStack*/

                        
/***************************************************/
/*                                                 */
/*                FindSplitComp                    */
/*                                                 */
/***************************************************/


static void FindSplitComp(TheGraph,CurrentNode,TStack,EStack,FLAG,j,SplitList,SepPairList)
Sgraph      TheGraph;
Snode       CurrentNode;
TripleStack *TStack;     /* Zur Definition von 'TripleStack' siehe FindSplitComp.h. */
EdgeStack   *EStack;     /* Kantenstack (Definition siehe Utilities.h) */
bool        *FLAG;
int         *j;
Slist       *SplitList;     /* Liste der Splitkomponenten */
Slist       *SepPairList;     /* Liste der Spaltungspaare */

/*******************************************************************************/
/*  Die Prozedur entspricht im wesentlichen der im Artikel beschriebenen       */
/*  Prozedure PATHSEARCH. Sie Zerlegt einen beliebigen 2fach-zusammenhaengen-  */
/*  den Graphen in seine Splitkomponenten. Die Komponenten werden in der       */
/*  Variablen 'SpliList' abgelegt. Die Variable 'SepPairList' enthaelt die     */
/*  "gefundenen" (siehe auch 'AddToSepPairList') Spaltungspaare des Graphen    */
/*  'TheGraph'.                                                                */
/*  Die verwendeten Funktion GetTargetNode,GetSourceNode sowie FindNode        */
/*  sind im Modul Utilities.c definiert.                                       */
/*  Bei Aufruf der Funktion sind dann die Parameter folgendermassen zu belegen:*/
/*     -- 'TheGraph' ist der durch 'FindSeperationPairs' vorbereitete Graph    */
/*     -- 'CurrentNode' ist die Wurzel des erzeugte 'palm tree', also          */
/*         der Knoten mit NewNum(CurrentNode) = 1                              */
/*     -- 'TripleStack' ist mit 'EmptyTStack' zu initialisieren                */
/*     -- 'ESTack' ist mit 'EmptyEdgeStack' zu initialisieren                  */
/*     -- 'FLAG' dient als Kennzeichen fuer gefundene Mehrfachkanten und       */
/*     --  mit 'FALSE' zu initialisieren                                       */
/*     -- 'j' ist ein Zaehler fuer virtuelle Kanten, er ist mit 'Null' zu      */
/*         initialisieren                                                      */
/*     -- 'SplitList' ist die Liste in der die Splitkomponenten abgelegt       */
/*         werden, sie ist mit 'empty_slist' zu initialisieren                 */
/*     -- 'SepPairList' ist die Liste der "gefundenen" Spaltungspaare, sie     */
/*         ist ebenfalls mit 'empty_slist' zu initialisieren                   */
/*******************************************************************************/


  {
    Slist        ActElem,SavedEdge,ActEdge,LastVirtualEdge,VirtEdge;
    TripleStack  LastElem,TestStack;
    Snode        x,y = EmptySnode,TargetNode,ActNode;
    bool         deleted = FALSE;
    EdgeStack    LastEdge;
    Slist        SplitComp = empty_slist;
    int          i;
        
    for_slist(AdjacencyList(CurrentNode),ActElem)
       TargetNode = GetTargetNode(ActElem);
       if (TypeOfEdge(EdgePointer(ActElem)) == ARC)
              {
                if (FirstEdgeOfPath(EdgePointer(ActElem)))
                        {
                          deleted = FALSE;
                          while(((*TStack) != EmptyTStack) &&(NewNum(ASepNode((*TStack))) > NewNum(LowPt1(TargetNode))))
                             {
                               y = MAX(y,HighestNode((*TStack)));
                               LastElem = (*TStack);
                               PopTStack((*TStack));
                               deleted = TRUE;
                             }/*while*/
                          if (!deleted)
                                   PushTStack(TStack,FindNode(TheGraph,NEWNUM,(NewNum(TargetNode)+Nd(TargetNode)-1)),LowPt1(TargetNode),CurrentNode);
                             else  PushTStack(TStack,MAX(y,FindNode(TheGraph,NEWNUM,NewNum(TargetNode)+Nd(TargetNode)-1)),LowPt1(TargetNode),BSepNode(LastElem));
                             
                          /*EOS Marke auf TStack*/
                          
                          PushTStack(TStack,EmptySnode,EmptySnode,EmptySnode);              
                        }/* then  FirstEdgeOfPath(EdgePointer(ActElem)) */
                FindSplitComp(TheGraph,TargetNode,TStack,EStack,FLAG,j,SplitList,SepPairList);
                AppendEdgeStack(EStack,ActElem);
                
                
                /*******************************************************************/
                /* Test auf Spaltungspaare vom Typ 2  (siehe Lemma13 des Artikels) */
                /*******************************************************************/
                
                /*TestStack = (*TStack);
                message("\n");
                message(" %s \n",CurrentNode->label);
                message("TSTACK:\n");
                while(TestStack != EmptyTStack)
                  {
                    message("%d %d %d\n",NewNum(HighestNode(TestStack)),NewNum(ASepNode(TestStack)),NewNum(BSepNode(TestStack)));
                    TestStack = TestStack->suc;
                  } */
                  
                /*for_all_nodes(TheGraph,ActNode)
                   message(" %s %d | ",ActNode->label,Degree(ActNode));
                end_for_all_nodes(TheGraph,ActNode);
                message("\n");*/                         
                
                while(((*TStack) != EmptyTStack) && ((NewNum(CurrentNode) != 1) && ((Degree(TargetNode) == 2) && 
                                                     (NewNum(FirstSon(TargetNode)) > NewNum(TargetNode)) || 
                                                     (NewNum(ASepNode((*TStack))) == NewNum(CurrentNode)))))
                   {
                     if (((*TStack) != EmptyTStack) && ((NewNum(ASepNode((*TStack))) == NewNum(CurrentNode)) && 
                                                        (NewNum(Father(BSepNode((*TStack)))) == NewNum(ASepNode((*TStack))))))
                               
                               /* kein Spaltungspaar vom Typ 2 vorhanden */
                               
                               PopTStack((*TStack));
                        else{
                               if ((Degree(TargetNode) == 2) && (NewNum(FirstSon(TargetNode)) > NewNum(TargetNode)))
                                      {
                                      
                                        
                                        /* {CurrentNode,FirstSon(TargetNode} sind ein Spaltungspaar.                      */
                                        /* D.h. die Kanten (CurrentNode,TargetNode),(TargetNode,FirstSon(TargetNode))     */
                                        /* sowie die neue virtuelle Kante (CurrentNode,FirstSon(TargetNode)) bilden eine  */
                                        /* neue Splitkomponente vom Typ Dreieck.                                          */
                                        
                                        AddToSepPairList(CurrentNode,FirstSon(TargetNode),SepPairList);
                                        
                                        (*j)++;
                                        AddToNewComp((*EStack)->edgelistelem,&SplitComp);
                                        PopEStack((*EStack));
                                        LastEdge = (*EStack);
                                        x = GetTargetNode(LastEdge->edgelistelem);
                                        AddToNewComp((*EStack)->edgelistelem,&SplitComp);
                                        LastVirtualEdge = MakeVirtualEdge(CurrentNode,x,(*j));
                                        AddToVirtualEdgeList(EdgePointer(LastVirtualEdge),BelongsToSepPair(CurrentNode));
                                        AddToNewComp(LastVirtualEdge,&SplitComp);
                                        AddToSplitCompList(SplitComp,TRIANGLE,SplitList);
                                        SplitComp = empty_slist;
                                        PopEStack((*EStack));
                                        
                                        /* Die beiden folgenden Anweisungen sind ebenfalls nicht im Original vorhanden. */
                                        /* Sie sind jedoch offensichtlich notwendig,da beim Hinzufuegen der virtuellen  */
                                        /* Kante (siehe [4] unten) (x,CurrentNode) ,der Grad der beiden Knoten erhoeht  */
                                        /* wird.                                                                        */
                                        
                                        Degree(x)--;
                                        Degree(CurrentNode)--;
                                        
                                        /* Ueberpruefen, ob eine Mehrfachkante durch das Hinzufuegen der virtuellen Kante */
                                        /* entstanden ist. [1]                                                            */
                                        
                                        if (((*EStack) != EmptyStack) && 
                                             ((NewNum(x) == NewNum(GetSourceNode((*EStack)->edgelistelem))) &&
                                            (NewNum(CurrentNode) == NewNum(GetTargetNode((*EStack)->edgelistelem)))))
                                               {
                                                 (*FLAG) = TRUE;
                                                 SavedEdge = (*EStack)->edgelistelem;
                                                 PopEStack((*EStack));
                                               }/*then*/
                                      }/* then  -{CurrentNode,FirstSon(TargetNode} sind ein Spaltungspaar- */ 
                                  else{
                                        if (((*TStack) != EmptyTStack) && ((NewNum(CurrentNode) == NewNum(ASepNode((*TStack)))) &&
                                                                     (NewNum(ASepNode((*TStack))) != NewNum(Father(BSepNode((*TStack)))))))
                                           {
                                           
                                             /* {ASepNode((*TStack)),BSepnode((*TStack))} sind ein Spaltungspaar. */
                                             
                                             AddToSepPairList(ASepNode((*TStack)),BSepNode((*TStack)),SepPairList);
                                              
                                             (*j)++;
                                             i = 0;
                                             while((((*EStack) != EmptyStack) && ((*TStack) != EmptyTStack)) &&
                                                    ((NewNum(GetSourceNode((*EStack)->edgelistelem)) >= NewNum(ASepNode((*TStack)))) &&
                                                    (NewNum(GetSourceNode((*EStack)->edgelistelem)) <= NewNum(HighestNode((*TStack)))) &&                                                     (NewNum(GetTargetNode((*EStack)->edgelistelem)) >= NewNum(ASepNode((*TStack)))) &&
                                                    (NewNum(GetTargetNode((*EStack)->edgelistelem)) <= NewNum(HighestNode((*TStack))))) )
                                                {
                                                  x = GetSourceNode((*EStack)->edgelistelem);
                                                  if ((((*TStack) != EmptyTStack) &&
                                                      ((NewNum(x) == NewNum(ASepNode((*TStack)))) &&
                                                      (NewNum(GetTargetNode((*EStack)->edgelistelem)) == NewNum(BSepNode((*TStack)))))) ||
                                                      (( (NewNum(x) == NewNum(BSepNode((*TStack)))) &&
                                                      (NewNum(GetTargetNode((*EStack)->edgelistelem)) == NewNum(ASepNode((*TStack)))))))
                                                      
                                                             /* In SPLIT(G) ist die Abfrage ohne das '||' und den darauf   */
                                                             /* folgenden Teil !!                                          */
                                                             
                                                         {
                                                           /* Mehrfachkante durch das Hinzufuegen der neuen virtuellen Kante [2]*/
                                                           
                                                           (*FLAG) = TRUE;
                                                           SavedEdge = (*EStack)->edgelistelem;
                                                           PopEStack((*EStack));
                                                         }/*then*/
                                                     else{  
                                                           /* Aufbau der neuen Splitkomponente. */
                                                           
                                                           i++;
                                                           AddToNewComp((*EStack)->edgelistelem,&SplitComp);
                                                           
                                                           
                                                           Degree(x)--;
                                                           Degree(GetTargetNode((*EStack)->edgelistelem))--; 
                                                           PopEStack((*EStack));
                                                         } /*else*/       
                                                }/*while*/

                                             /* Erzeugen der neuen virtuellen Kante (ASepNode((*TStack)),BSepNode((*TStack))) */
                                             
                                             LastVirtualEdge = MakeVirtualEdge(ASepNode((*TStack)),BSepNode((*TStack)),(*j));
                                             
                                             AddToVirtualEdgeList(EdgePointer(LastVirtualEdge),BelongsToSepPair(ASepNode((*TStack))));
                                             AddToNewComp(LastVirtualEdge,&SplitComp);
                                             
                                             /* Bestimmung des Typs der neuen Splitkomponente. */
                                             
                                             if (i == 2)
                                                     AddToSplitCompList(SplitComp,TRIANGLE,SplitList);
                                                else AddToSplitCompList(SplitComp,THREECONCOMP,SplitList); 
                                                
                                             SplitComp = empty_slist;     
                                             x = BSepNode((*TStack));
                                             PopTStack((*TStack));                                            
                                           }/* then  -{ASepNode((*TStack)),BSepnode((*TStack))} sind ein Spaltungspaar-*/     
                                      }/*else*/
                              if ((*FLAG))
                                     {
                                       /* Abtrennen einer Mehrfachkante, als 'bond, die durch das Hinzufuegen der neuen */
                                       /* virtuellen Kante entstanden ist.(siehe [1] bzw. [2])                          */
                                       
                                       (*FLAG) = FALSE;
                                       (*j)++;
                                       
                                       AddToNewComp(SavedEdge,&SplitComp);
                                       VirtEdge = MakeVirtualEdge(x,CurrentNode,(*j)-1);
                                       AddToVirtualEdgeList(EdgePointer(VirtEdge),BelongsToSepPair(x));
                                       LinkVirtualEdges(EdgePointer(VirtEdge),EdgePointer(LastVirtualEdge));
                                       AddToNewComp(VirtEdge,&SplitComp);
                                       LastVirtualEdge = MakeVirtualEdge(x,CurrentNode,(*j));
                                       AddToVirtualEdgeList(EdgePointer(LastVirtualEdge),BelongsToSepPair(x));
                                       AddToNewComp(LastVirtualEdge,&SplitComp);
                                       AddToSplitCompList(SplitComp,BOND,SplitList);
                                       SplitComp = empty_slist;
                                       Degree(x)--;
                                       Degree(CurrentNode)--;
                                     }
                              VirtEdge = MakeVirtualEdge(CurrentNode,x,(*j));
                              AddToVirtualEdgeList(EdgePointer(VirtEdge),BelongsToSepPair(CurrentNode));
                              LinkVirtualEdges(EdgePointer(VirtEdge),EdgePointer(LastVirtualEdge));
                              AppendEdgeStack(EStack,VirtEdge);
                              Degree(x)++;                         /* [4] */
                              Degree(CurrentNode)++;
                              Father(x) = CurrentNode;
                              if ((FirstSon(CurrentNode) != EmptySnode) &&
                                  ((NewNum(FirstSon(CurrentNode)) <= NewNum(x)) && 
                                   (NewNum(x) < (NewNum(FirstSon(CurrentNode)) + Nd(FirstSon(CurrentNode))))))
                                       FirstSon(CurrentNode) = x;
                                       
                              /*********/
                              /*  [3]  */ 
                              /*********/
                                      
                              TargetNode = x; 
                            }/*else*/
                   }/* while Test auf Spaltungspaare vom Typ 2 */ 
                   
                   
                   
                                                    
                /*******************************************************************/
                /* Test auf Spaltungspaare vom Typ 1  (siehe Lemma13 des Artikels) */
                /*******************************************************************/
                

                if ( ( NewNum(LowPt2(TargetNode)) >= NewNum(CurrentNode) )  && ( (NewNum(LowPt1(TargetNode)) != 1) || 
                      ( NewNum(Father(CurrentNode)) !=1) || (NewNum(GetTargetNode(ActElem)) > 3) ) )
                      
                      /* Im Original-Artikel lautet die Letzte Bedingung : NewNum(TargetNode)>3.                   */
                      /* Die Verwendung dieser Bedingung fuehrt jedoch zu einem falschen Ablauf des Algorithmus.   */
                      /* Da der Test auf Spaltungspaare vom Typ 2 zuerst durchlaufen wird, kann es moeglich sein,  */
                      /* das die aktuelle 'TargetNode' zwar eine groessere Nummer als 3 hat, die Kanten zwischen   */
                      /* der CurrentNode und der TargetNode jedoch schon abgespalten wurden.(Der kritische Befehl  */
                      /* hier : [3] TargetNode = x.) D.h. die Anzahl der noch nich abgespaltenen Kanten kann       */
                      /* kleiner sein als 2, auch wenn NewNum(TargetNode) > 3.                                     */
                      /* Daraus folgt jedoch, das moeglicherweise ein Spaltungspaar entdeckt wird, wobei eine      */
                      /* Splitkomponente aber nur aus einer Kante besteht, was nach Definition nicht zulaessig ist.*/ 
                      
                       {
                         AddToSepPairList(CurrentNode,LowPt1(TargetNode),SepPairList);
                         
                         (*j)++;
                         i = 0;
                         while(((*EStack) != EmptyStack) &&
                                (( ( NewNum(TargetNode) <= NewNum(GetSourceNode((*EStack)->edgelistelem)) ) && 
                               ( NewNum(GetSourceNode((*EStack)->edgelistelem)) < (NewNum(TargetNode)+Nd(TargetNode)) )) ||
                              ( ( NewNum(TargetNode) <= NewNum(GetTargetNode((*EStack)->edgelistelem)))  &&
                               ( NewNum(GetTargetNode((*EStack)->edgelistelem)) < (NewNum(TargetNode)+Nd(TargetNode)) ))))
                            {
                              /* Aufbau der neuen Splitkomponente. */
                              
                              i++;
                              AddToNewComp((*EStack)->edgelistelem,&SplitComp);
                              Degree(GetTargetNode((*EStack)->edgelistelem))--;
                              Degree(GetSourceNode((*EStack)->edgelistelem))--;
                              PopEStack((*EStack));
                            }/*while*/
                         
                         /* Erzeugen der neuen virtuellen Kante. */
                         
                         LastVirtualEdge = MakeVirtualEdge(CurrentNode,LowPt1(TargetNode),(*j));
                         AddToVirtualEdgeList(EdgePointer(LastVirtualEdge),BelongsToSepPair(CurrentNode));
                         AddToNewComp(LastVirtualEdge,&SplitComp);
                         
                         /* Bestimmung des Typs der neuen Splitkomponente. */
                         
                         if (i == 2)
                                 AddToSplitCompList(SplitComp,TRIANGLE,SplitList);
                            else AddToSplitCompList(SplitComp,THREECONCOMP,SplitList);
                            
                         SplitComp = empty_slist;        
                         
                         /* "Updaten" von FirstSon(CurrentNode) */
                         
                         if (FirstSon(CurrentNode) == TargetNode)
                                  FirstSon(CurrentNode) = LowPt1(TargetNode);
                         
                         /* Tests auf moeglicherweise, durch das Hinzufuegen der neuen virtuellen Kante, entstandene */
                         /* Mehrfachkanten.                                                                          */
                         
                         if (((*EStack) != EmptyStack) &&
                              ((NewNum(GetSourceNode((*EStack)->edgelistelem)) == NewNum(CurrentNode)) &&
                             (NewNum(GetTargetNode((*EStack)->edgelistelem)) == NewNum(LowPt1(TargetNode)))))
                                {
                                  /* Die erste Kante auf dem Kantenstack und die virtuelle Kante bilden */
                                  /* eine Mehrfachkante.                                                */
                                  
                                  (*j)++;
                                  
                                  /* Abspalten der Mehrfachkante als 'bond'. */
                                  
                                  AddToNewComp((*EStack)->edgelistelem,&SplitComp);
                                  VirtEdge = MakeVirtualEdge(CurrentNode,LowPt1(TargetNode),(*j)-1);
                                  AddToVirtualEdgeList(EdgePointer(VirtEdge),BelongsToSepPair(CurrentNode));
                                  LinkVirtualEdges(EdgePointer(VirtEdge),EdgePointer(LastVirtualEdge));
                                  AddToNewComp(VirtEdge,&SplitComp);
                                  LastVirtualEdge = MakeVirtualEdge(CurrentNode,LowPt1(TargetNode),(*j));
                                  AddToVirtualEdgeList(EdgePointer(LastVirtualEdge),BelongsToSepPair(CurrentNode));
                                  AddToNewComp(LastVirtualEdge,&SplitComp);
                                  AddToSplitCompList(SplitComp,BOND,SplitList);
                                  SplitComp = empty_slist;
                                  PopEStack((*EStack));
                                  Degree(CurrentNode)--;
                                  Degree(LowPt1(TargetNode))--;
                                }/* then */  
                         if (NewNum(LowPt1(TargetNode)) != NewNum(Father(CurrentNode)))
                                {
                                  /* Es ist keine weitere Mehrfachkante mehr entstanden. */
                                  
                                  VirtEdge = MakeVirtualEdge(CurrentNode,LowPt1(TargetNode),(*j));
                                  AddToVirtualEdgeList(EdgePointer(VirtEdge),BelongsToSepPair(CurrentNode));
                                  LinkVirtualEdges(EdgePointer(VirtEdge),EdgePointer(LastVirtualEdge));
                                  AppendEdgeStack(EStack,VirtEdge);
                                  Degree(CurrentNode)++;
                                  Degree(LowPt1(TargetNode))++;
                                }
                            else{
                                  /* Da die Kante (LowPt1(TargetNode),CurrentNode) schon im Graphen vorhanden war */
                                  /* ist durch das Hinzufuegen der virtuellen Kante eine Mehrfachkante entstanden.*/
                                  
                                  (*j)++;
                                  
                                  /* Abspalten der Mehrfachkante als 'bond'. */
                                  
                                  VirtEdge = MakeVirtualEdge(CurrentNode,LowPt1(TargetNode),(*j)-1);
                                  AddToVirtualEdgeList(EdgePointer(VirtEdge),BelongsToSepPair(CurrentNode));
                                  LinkVirtualEdges(EdgePointer(VirtEdge),EdgePointer(LastVirtualEdge));
                                  AddToNewComp(VirtEdge,&SplitComp);
                                  LastVirtualEdge = MakeVirtualEdge(CurrentNode,LowPt1(TargetNode),(*j));
                                  AddToVirtualEdgeList(EdgePointer(LastVirtualEdge),BelongsToSepPair(CurrentNode));
                                  AddToNewComp(LastVirtualEdge,&SplitComp);
                                  
                                  /* Achtung: Hier wird keine virtuelle Kante erzeugt, sonder eine "Kopie" der */
                                  /* im Graphen vorhandenen Kante (LowPt1(TargetNode),CurrentNode).            */
                                  
                                  AddToNewComp(MakeVirtualEdge(CurrentNode,LowPt1(TargetNode),0),&SplitComp);
                                  
                                  /* Die im Graphen schon vorhandene Kante wird als virtuelle Kante markiert. */
                                  
                                  for_slist(AdjacencyList(LowPt1(TargetNode)),ActEdge)
                                     if (NewNum(GetTargetNode(ActEdge)) == NewNum(CurrentNode))
                                           {
                                             LinkVirtualEdges(EdgePointer(ActEdge),EdgePointer(LastVirtualEdge));
                                             AddToVirtualEdgeList(EdgePointer(ActEdge),BelongsToSepPair(CurrentNode));
                                             VirtualEdgeNr(EdgePointer(ActEdge)) = (*j);
                                             break;
                                           }/*then*/
                                  end_for_slist(AdjacencyList(LowPt1(TargetNode)),ActEdge);           
                                 
                                  AddToSplitCompList(SplitComp,BOND,SplitList);
                                  SplitComp = empty_slist;
                                }/*else*/         
                       }/* then -Spaltungspaar vom Typ 1 vorhanden- */
                 
                /* "Updaten" des 'TStacks' wie im Artikel beschrieben. */
                       
                if (FirstEdgeOfPath(EdgePointer(ActElem)))
                       {
                         while(((*TStack) != EmptyTStack) &&
                                ((HighestNode((*TStack))  != EmptySnode) && (ASepNode((*TStack))  != EmptySnode) &&                                                       (BSepNode((*TStack))  != EmptySnode)))
                                  {
                                    PopTStack((*TStack));
                                  }  
                         PopTStack((*TStack));
                       }/*then*/
                 else while (((*TStack) != EmptyTStack) &&
                             ((HighestNode((*TStack))  != EmptySnode) && (ASepNode((*TStack))  != EmptySnode) &&                                                        (BSepNode((*TStack))  != EmptySnode)) && 
                              (NewNum(HighestNode((*TStack))) < HighPt(CurrentNode)))
                          PopTStack((*TStack));                      
              }/* then -TypeOfEdge(EdgePointer(ActElem)) == ARC- */
          else{
                if (FirstEdgeOfPath(EdgePointer(ActElem)))
                       {
                         y = EmptySnode;
                         deleted = FALSE;
                         while(((*TStack) != EmptyTStack) &&(NewNum(ASepNode((*TStack))) > NewNum(TargetNode)))
                            {
                              y = MAX(y,HighestNode((*TStack)));
                              LastElem = (*TStack);
                              PopTStack((*TStack));
                              deleted = TRUE;
                            }/*while*/
                         if (!deleted)
                                 PushTStack(TStack,CurrentNode,TargetNode,CurrentNode);
                            else PushTStack(TStack,y,TargetNode,BSepNode(LastElem));
                       }/*then  -FirstEdgeOfPath(EdgePointer(ActElem))- */
                if (NewNum(TargetNode) == NewNum(Father(CurrentNode)))
                        { 
                          /******************************************************************/
                          /* Dieser Fall kann eigentlich nur bei Graphen mit MehrfachKanten */
                          /* auftreten !!!                                                  */
                          /******************************************************************/
                          (*j)++;
                          
                          /* add (v,w),(v,w,j),treearc(w,v) to new component*/
                          AddToNewComp(ActElem,&SplitComp);
                          AddToNewComp(MakeVirtualEdge(GetSourceNode(ActElem),GetTargetNode(ActElem),(*j)),&SplitComp);
                          /*************************************************************************************/
                          /*ACHTUNG :hier wird eine neue Kante erzeugt,da die Kante nicht wirklich existiert !!*/
                          /*************************************************************************************/
                          AddToNewComp(MakeVirtualEdge(GetSourceNode(ActElem),GetTargetNode(ActElem),0),&SplitComp);
                          AddToSplitCompList(SplitComp,BOND,SplitList);
                          SplitComp = empty_slist;
                          Degree(CurrentNode)--;
                          Degree(TargetNode)--;
                        }
                    else{ 
                          /* Hier liegt wieder eine Aenderung des Originalalgorithmus vor !!                        */
                          /* Im Original steht hier NUR die Anweisung: 'AppendEdgeStack(EStack,ActElem);'.          */
                          /* diese ist jedoch offensichtlich ungenuegend, da es moeglich ist, das auf dem           */
                          /* Kantenstack schon eine virtuelle Kante liegt die, die mit dem Hinzugefuegte 'frond'    */
                          /* eine Mehrfachkante bildet. Diese Mehrfachkante kann nur an dieser Stelle des Programms */
                          /* entdeckt werden.                                                                       */
                          
                          if ( ((*EStack) != EmptyStack) &&
                               ( (GetSourceNode((*EStack)->edgelistelem) == GetSourceNode(ActElem)) &&
                                 (GetTargetNode((*EStack)->edgelistelem) == GetTargetNode(ActElem)) ) )
                                  {
                                    /* Der hinzugefuegte 'frond' und die erste Kante des Kantenstacks bilden eine Mehrfachkante. */
                                    
                                    (*j)++;
                                    AddToNewComp((*EStack)->edgelistelem,&SplitComp);
                                    AddToNewComp(ActElem,&SplitComp);
                                    LastVirtualEdge = MakeVirtualEdge(GetSourceNode(ActElem),GetTargetNode(ActElem),(*j));
                                    
                                    /* Hier muss die Funktion 'FindSepPair' verwendet werden, da der Zeiger 'BelongsToSepPair' */
                                    /* eventuell nicht mehr den korrekten Wert enthaelt ! (siehe auch NodeEdgeAttrs.h ->       */
                                    /* BelongsToSepPair(TheNode) )                                                             */          
                                    
                                    AddToVirtualEdgeList(EdgePointer(LastVirtualEdge),
                                                         FindSepPair(GetSourceNode(ActElem),GetTargetNode(ActElem),(*SepPairList)));
                                    AddToNewComp(LastVirtualEdge,&SplitComp);
                                    AddToSplitCompList(SplitComp,BOND,SplitList);
                                    SplitComp = empty_slist;   
                                    PopEStack((*EStack));
                                    VirtEdge = MakeVirtualEdge(GetSourceNode(ActElem),GetTargetNode(ActElem),(*j));
                                    AddToVirtualEdgeList(EdgePointer(VirtEdge),
                                                         FindSepPair(GetSourceNode(ActElem),GetTargetNode(ActElem),(*SepPairList)));
                                    LinkVirtualEdges(EdgePointer(VirtEdge),EdgePointer(LastVirtualEdge));
                                    AppendEdgeStack(EStack,VirtEdge);
                                  }/*then*/
                              else /* Es liegt keine Mehrfachkante vor. */
                                   AppendEdgeStack(EStack,ActElem);      
                         }/*else*/        
              }/*then -TypeOfEdge(EdgePointer(ActElem)) == FROND- */     
    end_for_slist(AdjacencyList(CurrentNode),ActElem);
  }/*end of FindSplitComp*/
  

/***************************************************/
/*                                                 */
/*        DetermineSplitComponents                 */
/*                                                 */
/***************************************************/
   

void DetermineSplitComponents(TheGraph,SplitList,SepPairList)
Sgraph   TheGraph;
Slist    *SplitList;
Slist    *SepPairList;

  {
    TripleStack TStack = EmptyTStack;
    EdgeStack   EStack = EmptyStack;
    int         j = 0;
    bool        FLAG = FALSE;
    Slist       SplitComp = empty_slist;
    int         i = 0;
    
    
    FindSplitComp(TheGraph,TheGraph->nodes,&TStack,&EStack,&FLAG,&j,SplitList,SepPairList);
    
    
    /* Konstruieren der letzten Splitkomponente, aus den auf dem 'EStack' verbliebenen Kanten. */
    
    while(EStack != EmptyStack)
      {
        i++;
        AddToNewComp(EStack->edgelistelem,&SplitComp);
        PopEStack(EStack);
      }
    if (i == 3)
            AddToSplitCompList(SplitComp,TRIANGLE,SplitList);
       else AddToSplitCompList(SplitComp,THREECONCOMP,SplitList);
  }/* end of DetermineSplitComponents */     
    
