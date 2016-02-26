/* (C) Universitaet Passau 1986-1991 */
/*1990-91 von Stefan Jockenhoevel und Gerd Nienhaus.*/

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE FindSplitComp.h                                            **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

#include "Utilities.h"

#ifndef FINDSPLITCOMP_HEADER
#define FINDSPLITCOMP_HEADER


/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                                 TripleStack                                         **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

typedef struct triplestack{

        Snode  highpt;
        
        Snode  nodea;
        
        Snode  nodeb;
        
        struct triplestack  *suc;
        
        } *TripleStack;
        
#define EmptyTStack ((TripleStack)NULL)
#define PopTStack(TStack) ((TStack != EmptyTStack) ? (TStack = TStack->suc) : (TStack = EmptyTStack))

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**             Makros zum Zugriff auf die Elemente des Stacks                          **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/
        
#define HighestNode(TStack)  (TStack->highpt)
#define ASepNode(TStack)  (TStack->nodea)
#define BSepNode(TStack)  (TStack->nodeb)



#define MAX(FirstNode,SecondNode) ((NewNum(FirstNode) >= NewNum(SecondNode)) ? FirstNode : SecondNode)



typedef enum{TRIANGLE,BOND,THREECONCOMP} SplitCompType;


/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                               SplitCompListAttrs                                    **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

typedef struct splitcomplistattrs{

        SplitCompType   type;
        
        bool            reached;
        
        Slist           edgelist;
        
        } *SplitCompListAttrs;
        
/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**         Makros zum Zugriff auf die Attribute der Liste der Splitkomponenten         **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/


/*******************************************************************************/
/*  TypeOfSplitComp(SlistElem) bezeichnet den Typ der ,in 'SlistElem'          */
/*       abgespeicherten, Splitkomponente.                                     */
/*       Wie bekannt gibt es drei Arten von Splitkomponenten:                  */
/*              * dreifach-Zusammenhangskomponenten  (THREECONCOMP)            */
/*              * Dreiecke  (TRIANGLE)                                         */
/*              * MehrfachKanten  (BOND)                                       */
/*******************************************************************************/
        
#define  TypeOfSplitComp(SlistElem) (attr_data_of_type(SlistElem,SplitCompListAttrs)->type)

/*******************************************************************************/
/*  SplitCompEdges(SlistElem) ist der Zeiger auf die 'Slist' der Kanten der    */
/*        in 'SlistElem' abgespeicherten Splitkomponente.                      */
/*******************************************************************************/

#define  SplitCompEdges(SlistElem) (attr_data_of_type(SlistElem,SplitCompListAttrs)->edgelist)

/*******************************************************************************/
/*  IsReached(SlistElem) ist ein Boolscher Wert der angibt, ob die Komponente, */
/*       waehrend des Durchsuchens der gesamten Splitkomponenten, schon einmal */
/*       gefunden und bearbeitet wurde. (siehe auch ConvexTest.c)              */
/*******************************************************************************/

#define  IsReached(SlistElem) (attr_data_of_type(SlistElem,SplitCompListAttrs)->reached)



#define  EdgeOfSplitComp(SlistElem)  (attr_data_of_type(SlistElem,Sedge))


 
/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                               SepPairListAttrs                                      **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/
 
typedef struct seppairlistattrs{

        Snode    anode;
        
        Snode    bnode;
        
        Slist    virtualedgelist;
        
        bool     prime;
        
        bool     forbidden;
        
        bool     critical;
        
        SedgeStack     exactlyonering;  /* Die Definition des Datentyps 'SedgeStack' ist in Utilities.h zu finden. */
        
        } *SepPairListAttrs;
        

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**         Makros zum Zugriff auf die Attribute der Liste der Spaltungspaare           **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

/*******************************************************************************/
/*  SepPairA(SlistElem) liefert den ersten Knoten des Spaltungspaares.         */
/*******************************************************************************/

#define SepPairA(SlistElem) (attr_data_of_type(SlistElem,SepPairListAttrs)->anode)

/*******************************************************************************/
/*  SepPairA(SlistElem) liefert den zweiten Knoten des Spaltungspaares.        */
/*******************************************************************************/
               
#define SepPairB(SlistElem) (attr_data_of_type(SlistElem,SepPairListAttrs)->bnode)

/*******************************************************************************/
/*  VirtualEdgeList(SlistElem) liefert den Zeiger auf die 'Slist',die alle     */
/*       virtuellen Kanten , deren Endknoten, Knoten des Spaltungspaares sind, */
/*       enthaelt.                                                             */
/*******************************************************************************/

#define VirtualEdgeList(SlistElem) (attr_data_of_type(SlistElem,SepPairListAttrs)->virtualedgelist)

/*******************************************************************************/
/*  IsPrime(SlistElem) ist ein Boolscher Wert der angibt, ob das in            */
/*         'SlistElem' abgespeicherte Spaltungspaar 'prime' ist.               */
/*******************************************************************************/

#define IsPrime(SlistElem) (attr_data_of_type(SlistElem,SepPairListAttrs)->prime)

/*******************************************************************************/
/*  IsForbidden(SlistElem) ist ein Boolscher Wert der angibt, ob das in        */
/*         'SlistElem' abgespeicherte Spaltungspaar 'forbidden' ist.           */
/*******************************************************************************/

#define IsForbidden(SlistElem) (attr_data_of_type(SlistElem,SepPairListAttrs)->forbidden)


/*******************************************************************************/
/*  IsCritical(SlistElem) ist ein Boolscher Wert der angibt, ob das in         */
/*         'SlistElem' abgespeicherte Spaltungspaar 'critical' ist.            */
/*******************************************************************************/

#define IsCritical(SlistElem) (attr_data_of_type(SlistElem,SepPairListAttrs)->critical)


/*******************************************************************************/
/*  HasOneRing(SlistElem) ist nur fuer kritische Spaltungspaare relevant.      */
/*       Der Wert dises Attributes ist nur dann ungleich 'nil', wenn in        */
/*       'SlistElem' ein kritisches Spaltungspaar abgespeichert ist und es     */
/*       genau eine {x,y}-Splitkomponente  existiert, die ein Ring ist.        */
/*       Alle Kanten des Rings, die nicht virtuell sind, sind dann im          */
/*       'SedgeStack' HasOneRing(SlistElem) abgelegt.                          */
/*******************************************************************************/
       
#define HasOneRing(SlistElem) (attr_data_of_type(SlistElem,SepPairListAttrs)->exactlyonering)


#define VirtualEdge(SlistElem) (attr_data_of_type(SlistElem,Sedge))


/*******************************************************************************/
/*  Die Funktion DetermineSpliComponents entspricht im wesentlichen der        */
/*  Funktion SPLIT(G) des Artikels.                                            */
/*  Sie zerlegt einen beliebigen zweifach-zusammenhaengenden Graphen in seine  */
/*  Splitkomponenten. Die Splitkomponenten sind als Kantenlisten in der 'Slist'*/
/*  'SplitList' abgelegt.                                                      */
/*  Die gefundenen Spaltungspaare werden in der 'Slist' SepPairList abgelegt.  */
/*  Es wird vorrausgesetzt, das der Graph durch die Funktion                   */
/*  'FindSeperationPairs' (siehe FindSepPairs.c) , bearbeitet worden ist.      */
/*******************************************************************************/

extern void DetermineSplitComponents(/* Sgraph TheGraph, Slist *SplitList, Slist *SepPairList */);
#endif
