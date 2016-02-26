/* (C) Universitaet Passau 1986-1991 */
/*1990-91 von Stefan Jockenhoevel und Gerd Nienhaus.*/

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE NodeEdgeAttrs.h                                            **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

/*****************************************************************************/
/*                                                                           */
/*  Diese Modul stellt die Datenstruktur fuer die benoetigten                */
/*  Knoten- bzw. Kantenattribute zur Verfuegung. Ausserdem werden Makros     */
/*  fuer den Zugriff auf diese Attribute bereitgestellt.                     */
/*                                                                           */
/*****************************************************************************/

#ifndef NODEEDGEATTRS_HEADER
#define NODEEDGEATTRS_HEADER
#include STDH
#include SLISTH
#include SGRAPHH

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   My Node Attributes                                                **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

typedef struct mynodeattrs{
        int       nr;
        
        bool      flag;
        
        Snode     lowpt1;
        
        Snode     lowpt2;
        
        int       nd ;
        
        Snode     father;
        
        int       newnum;
        
        int       degree;
        
        int       highpt;
        
        Slist     A; 
          
        Snode     firstson;
        
        Slist   seppair;
        
        } *MyNodeAttrs;
        
/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                 Makros zum Zugriff auf die Node-Attributes.                         **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/



/*******************************************************************************/
/*  Number(TheNode) ist die Nummer, die dem Knoten 'TheNode' waehrend der      */
/*        Prozedur DFS (siehe FindSepPairs.c) zugewiesen wird. D.h. 'Number'   */
/*        gibt die Reihenfolge an, in der die Knoten in der ersten             */
/*        Tiefensuche erreicht werden.                                         */
/*******************************************************************************/

#define  Number(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->nr)

/*******************************************************************************/
/*  Flag(TheNode) wird in der Prozedur DFS benoetigt, um zu verhindern, dass   */
/*      eine Kante mehrmalig untersucht wird.                                  */
/*******************************************************************************/
 
#define  Flag(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->flag)

/*******************************************************************************/
/*  LowPt1(TheNode) bezeichnet den Knoten mit der niedrigsten Nummer, der von  */
/*        'TheNode' aus, ueber null oder mehrere Aeste des 'palm tree' und     */
/*        hoechstens einem 'frond', erreichbar ist.                            */
/*                                                                             */
/*        LowPt1 wird als 'Snode' implementiert, um spaeteres Suchen nach      */
/*        Knoten mit bestimmten Nummern (Lowpt-Werte) zu vermeiden. Durch den  */
/*        Zeiger erhaelt man den direkten Zugriff auf den entsprechenden       */
/*        Knoten. Auch erspart man sich hierdurch ein erneutes Berechnen des   */
/*        Wertes, nach der neuen Nummerierung der Knoten in der Prozedur       */
/*        'PathFinder'.(siehe auch FindSepPairs.c)                             */
/*******************************************************************************/
 
#define  LowPt1(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->lowpt1)

/*******************************************************************************/
/*  LowPt2(TheNode) bezeichnet den Knoten mit der zweit-niedrigsten Nummer, der*/
/*        von 'TheNode' aus, ueber null oder mehrere Aeste des 'palm tree' und */
/*        hoechstens einem 'frond', erreichbar ist.                            */
/*                                                                             */
/*        LowPt2 wird als 'Snode' implementiert, um spaeteres Suchen nach      */
/*        Knoten mit bestimmten Nummern (Lowpt-Werte) zu vermeiden. Durch den  */
/*        Zeiger erhaelt man den direkten Zugriff auf den entsprechenden       */
/*        Knoten. Auch erspart man sich hierdurch ein erneutes Berechnen des   */
/*        Wertes, nach der neuen Nummerierung der Knoten in der Prozedur       */
/*        'PathFinder'.(siehe auch FindSepPairs.c)                             */
/*******************************************************************************/

#define  LowPt2(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->lowpt2)

/*******************************************************************************/
/*  Nd(TheNode) bezeichnet die Anzahl der Nachkommen von 'TheNode'.            */
/*      KONVENTION: Jeder Knoten ist sein eigener Nachkomme.                   */
/*******************************************************************************/

#define  Nd(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->nd)

/*******************************************************************************/
/*  Father(TheNode) bezeichnet den Vater von 'TheNode'.                        */
/*        Father wird als 'Snode' implementiert,damit,nach der erneuten        */
/*        Nummerierung der Knoten durch die Prozedure 'PathFinder', der Wert   */
/*        nicht erneut berechnet werden muss.                                  */
/*******************************************************************************/

#define  Father(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->father)

/*******************************************************************************/
/*  (Set)NewNum(TheNode) gibt die Nummer an, die 'TheNode' waehrend der        */
/*              Prozedur 'PathFinder' (siehe FindSepPairs.c) zugewiesen wird.  */
/*              D.h. NewNum gibt die Reihenfolge an, in der die Knoten in der  */
/*              Prozedur 'PatheFinder' erreicht wurden.                        */
/*              Da es notwendig ist, dass das Makro auch fuer 'TheNode = nil'  */
/*              den korrekten Wert liefert, ist ein besonderes Makro zum Setzen*/
/*              des Attributs notwendig. Diese Funktion uebernimmt SetNewNum.  */
/*******************************************************************************/

#define  SetNewNum(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->newnum)
#define  NewNum(TheNode) ((TheNode != EmptySnode) ? (attr_data_of_type(TheNode,MyNodeAttrs)->newnum) : 0 )

/*******************************************************************************/
/*  Degree(TheNode) gibt die Anzahl der Knoten, mit denen 'TheNode' verbunden  */
/*         ist, an.                                                            */
/*******************************************************************************/

#define  Degree(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->degree)

/*******************************************************************************/
/*  HighPt(TheNode) bezeichnet den Knoten mit der hoechsten "neuen" Nummer, von*/
/*        dem aus ein 'frond' nach 'TheNode' existiert.                        */
/*******************************************************************************/

#define  HighPt(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->highpt)

/*******************************************************************************/
/*  AdjacencyList(TheNode) ist eine 'Slist' aller Kanten, die mit 'TheNode'    */
/*         verbunden sind. Sie wird benoetigt, damit die original 'Sgraph'-    */
/*         Struktur waehrend des Programmablaufs erhalten bleibt. Beim Ueber-  */
/*         fuehren des Graphen in den 'palm tree' muessen Kanten geloescht     */
/*         werden, dies kann jedoch nicht in der Originalstruktur geschehen,da */
/*         diese spaeter wieder zum Zeichnen des Graphen benoetigt wird.       */
/*         Also arbeitet der gesamte Algorithmus, zum Testen des Graphen auf   */
/*         Konvexitaet, auf dieser KantenListe.                                */
/*******************************************************************************/

#define  AdjacencyList(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->A)

/*******************************************************************************/
/*  FirstSon(TheNode) bezeichnet den "ersten Sohn" des Knoten 'TheNode'.       */
/*       Dies ist der ZielKnoten der ersten Kante von 'AdjacencyList(TheNode)',*/
/*       falls die erste Kante vom Typ 'ARC' ist. Ansonsten existiert kein     */
/*       "erster Sohn".                                                        */
/*******************************************************************************/
       
#define  FirstSon(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->firstson)

/*******************************************************************************/
/*  BelongsToSepPair(TheNode) gibt einen Zeiger auf das LETZTE gefundene       */
/*       Spaltungspaar zurueck, zu dem 'TheNode' gehoert. D.h. der Wert        */
/*       kann sich u.U waehren des Prorammablaufs staendig aendern.            */
/*       Der Zeiger erspart das staendige Durchlaufen der Liste der Spaltungs- */
/*       paare.(siehe FindSplitComp.c)                                         */
/*******************************************************************************/

#define  BelongsToSepPair(TheNode) (attr_data_of_type(TheNode,MyNodeAttrs)->seppair)


        
/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   My Edge Attributes                                                **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/


typedef enum {ARC,FROND,DELETE} EdgeType; /* Der Typ 'DELETE' dient zur Zeit nur der Initialisierung. */

typedef struct myedgeattrs{
        EdgeType  edgetype;
        
        bool      firstedgeofpath;
        
        int       virtualedgenr;
        
        Slist     splitcomp;
        
        Sedge     correspondingedge;
        
        bool      activ;     

        } *MyEdgeAttrs;
        
        
/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                 Makros zum Zugriff auf die Edge-Attributes.                         **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/


/*******************************************************************************/
/*  TypeOfEdge(TheEdge) gibt den, in der Prozedur DFS bestimmten (siehe        */
/*        FindSepPairs.c) Typ der Kante 'TheEdge', im erzeugten 'palm tree',   */
/*        an.                                                                  */
/*******************************************************************************/

#define TypeOfEdge(TheEdge) (attr_data_of_type(TheEdge,MyEdgeAttrs)->edgetype)

/*******************************************************************************/
/*  FirstEdgeOfPath(TheEdge) gibt an, ob die Kante 'TheEdge' die erste Kante,  */
/*       eines in der Prozedur 'PathFinder' (siehe FindSepPairs.c) erzeugten   */
/*       Pfades, im 'palm tree', ist.                                          */
/*******************************************************************************/
   
#define FirstEdgeOfPath(TheEdge) (attr_data_of_type(TheEdge,MyEdgeAttrs)->firstedgeofpath)

/*******************************************************************************/
/*  VirtualEdgeNr(TheEdge) gibt an, ob die Kante 'TheEdge' eine virtuelle oder */
/*       eine reale Kante des Graphen ist. Ist der Wert ungleich Null, so      */
/*       liegt eine virtuelle Kante vor; d.h. 'TheEdge' ist durch einen        */
/*       Splitvorgang entstanden. Virtuelle Kanten, die durch einen Split-     */
/*       vorgang entstanden sind, erhalten den gleichen Wert. (siehe auch      */
/*       FindSplitComp.c)                                                      */
/*******************************************************************************/

#define VirtualEdgeNr(TheEdge) (attr_data_of_type(TheEdge,MyEdgeAttrs)->virtualedgenr)

/*******************************************************************************/
/*  BelongsToSplitComp(TheEdge) ist ein Zeiger auf die Splitkomponente zu der  */
/*       'TheEdge' gehoert.                                                    */
/*******************************************************************************/

#define BelongsToSplitComp(TheEdge) (attr_data_of_type(TheEdge,MyEdgeAttrs)->splitcomp)

/*******************************************************************************/
/*  CorrespondingEdge(TheEdge) ist ein Zeiger, der nur fuer virtuelle Kanten   */
/*       ungleich 'nil' ist. Er ermoeglicht den direkten Zugriff auf die       */
/*       virtuelle Kante, die im gleichen Splitvorgang wie 'TheEdge'           */
/*       entstanden ist.                                                       */
/*******************************************************************************/

#define CorrespondingEdge(TheEdge) (attr_data_of_type(TheEdge,MyEdgeAttrs)->correspondingedge)

/*******************************************************************************/
/*  IsActiv(TheEdge) ist ebenfalls nur fuer virtuelle Kanten von Bedeutung.    */
/*        Es dient dazu die Splitkomponenten "pseudomaessig" zu dreifach-      */
/*        Zusammenhangskomponenten oder {x,y}-Splitkomponenten zusammen zu     */
/*        setzen. Der Wert ist gleich TRUE, falls die virtuelle Kante wirklich */
/*        besteht, d.h der Graph wurde nicht wieder "zusammengesetzt".         */
/*        (siehe auch ConvexTest.c)                                            */
/*******************************************************************************/

#define IsActiv(TheEdge) (attr_data_of_type(TheEdge,MyEdgeAttrs)->activ)



/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**               My Edge List Attributes                                               **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

typedef struct myedgelistattrs{

        bool    edgeofsourcelist;
        
        bool    reached;
        
        Sedge   edge;
        
        } *MyEdgeListAttrs;

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**              Makros zum Zugriff auf die EdgeList-Attributes                         **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

/* 'TheSlist' ist im folgenden immer ein Element der 'AdjacencyList' eines Knotens.      */

/*******************************************************************************/
/*  EdgeOfSourceList(TheSlist) gibt an, ob die unter 'TheList' abgelegte Kante */
/*       aus der "Sourceliste" des betrachteten Knotens stammt oder nicht.     */
/*******************************************************************************/

#define EdgeOfSourceList(TheSlist)  (attr_data_of_type(TheSlist,MyEdgeListAttrs)->edgeofsourcelist)

/*******************************************************************************/
/*  EdgePointer(TheSlist) ist der Zeiger auf die, im Listenelement 'TheSlist'  */
/*       abgespeicherte Kante.                                                 */
/*******************************************************************************/

#define EdgePointer(TheSlist)  (attr_data_of_type(TheSlist,MyEdgeListAttrs)->edge)

/*******************************************************************************/
/*  Reached(TheSlist) ist ein Boolscher Wert der angibt, ob das betreffende    */
/*       Listenelement 'TheSlist', waehrend der Prozedur DFS (siehe            */
/*       FindSepPairs.c) , erreicht wurde oder nicht. Das Attribut wird in den */
/*       Attributen der Kantenliste angelegt und nicht in der Kante selbst,    */
/*       da zwar jede Kante zweimal in der original Sgraph-Struktur vorkommt,  */
/*       aber die Kante real nur einmal abgespeichert ist.                     */
/*******************************************************************************/

#define Reached(TheSlist)  (attr_data_of_type(TheSlist,MyEdgeListAttrs)->reached)

#endif
