/* (C) Universitaet Passau 1986-1991 */
/*1990-91 von Stefan Jockenhoevel und Gerd Nienhaus.*/

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE ConvexTest.h                                               **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

 
#ifndef CONVEXTEST_HEADER
#define CONVEXTEST_HEADER

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                               OrderedNodeAttrs                                      **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

typedef struct orderednodeattrs{
        
        int     degree;
        
        int     drawn;
        
        int     nodevisited;
        
        bool    removed;
        
        bool    apex;
        
        Slist   orderededgelist;
        
        } *OrderedNodeAttrs;


/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**              Makros zum Zugriff auf die 'OrderedNodeAttrs'                          **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/


/*******************************************************************************/
/*  Deg(TheNode) gibt die Anzahl der Knoten an, mit denen 'TheNode' verbunden  */
/*      ist.                                                                   */
/*******************************************************************************/
        
#define Deg(TheNode) (attr_data_of_type(TheNode,OrderedNodeAttrs)->degree)


/*******************************************************************************/
/*  Drawn(TheNode) ist ein Integer, der angibt zu welcher Flaechenbegrenzung   */
/*	der Knoten zuletzt gehoerte. Da jeder Knoten mehrmals in Flaechen-     */
/*	begrenzungen auftreten kann aendert sich 'Drawn' im Verlaufe der       */
/*	Prozedur 'ExtendFacialCycle' (siehe auch DrawConvex.c).                */
/*******************************************************************************/

#define Drawn(TheNode) (attr_data_of_type(TheNode,OrderedNodeAttrs)->drawn)


/*******************************************************************************/
/*  NodeVisited(SlistElem) wird in der Prozedur 'SearchNodesAndEdges'          */
/*	benoetigt.Der Integer dient dazu besuchte Knoten zu kennzeichnen.      */
/*	Naeheres dazu findet man in DrawConvex.c.                              */
/*******************************************************************************/

#define NodeVisited(TheNode) (attr_data_of_type(TheNode,OrderedNodeAttrs)->nodevisited)


/*******************************************************************************/
/*  Removed(TheNode) ist ein Boolscher Wert, der 'TRUE' liefert, wenn 'TheNode'*/
/*	im Verlaufe der Prozedur 'ExtendFacialCycle' "geloescht" wird.         */
/*	(siehe auch DrawConvex.c)                                              */
/*******************************************************************************/

#define Removed(TheNode) (attr_data_of_type(TheNode,OrderedNodeAttrs)->removed)


/*******************************************************************************/
/*  Apex(TheNode) wird in der Prozedur 'DrawEntryConvex' benoetigt, 'Apex'     */
/*	kennzeichnet einen Knoten als Eckpunkt des zu zeichnenden Polygons     */
/*      (siehe auch DrawConvex.c).                                             */
/*******************************************************************************/

#define Apex(TheNode) (attr_data_of_type(TheNode,OrderedNodeAttrs)->apex)


/*******************************************************************************/
/*  OrderedEdgeList(TheNode) ist eine 'Slist' der Kanten, die mit 'TheNode'    */
/*	verbunden sind. Die Kanten sind dabei im Uhrzeigersinn um 'TheNode'    */
/*	in der 'Slist' abgelegt. D.h. OrderedEdgeList(TheNode)->suc ist die    */
/*	im Uhrzeigersinn auf OrderedEdgeList(TheNode) folgende Kante. Diese    */
/*      so zur Verfuegung stehende Kantenliste wird dazu benoetigt um          */
/*	Flaechen im Graphen bestimmen zu koennen. Naeheres zu der genauen      */
/*	Struktur der 'Slist' findet man im folgenden unter                     */
/*	'OrderedEdgeListAttrs'.                                                */
/*******************************************************************************/

#define OrderedEdgeList(TheNode) (attr_data_of_type(TheNode,OrderedNodeAttrs)->orderededgelist)



#define EmbededList(TheNode) (attr_data_of_type(TheNode,Slist))

#define EmbededEdge(SlistElem) (attr_data_of_type(SlistElem,Sedge))


/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                            OrderedEdgeListAttrs                                     **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

/* Im folgenden ist 'SlistElem' ein Element der geordneten Kantenliste eines Knotens. */
        
typedef struct orderededgelistattrs{
        
        Sedge   edge;
        
        Slist   edgecopy;
        
        bool    edgeoftargetlist;
        
        bool    traversed;
        
        int     edgevisited;
        
        } *OrderedEdgeListAttrs;
 
/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**              Makros zum Zugriff auf die 'OrderedNodeAttrs'                          **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/
 
 
/*******************************************************************************/
/*  Edge(SlistElem) ist der Zeiger auf die im Listenelement abgespeicherte     */
/*	Kante (Sedge).                                                         */
/*******************************************************************************/
       
#define Edge(SlistElem) (attr_data_of_type(SlistElem,OrderedEdgeListAttrs)->edge)



/*******************************************************************************/
/*  EdgeCopy(SlistElem) ist ein Zeiger auf das 'SlistElem' einer anderen       */
/*	geordneten Kantenliste, in dem die Kopie von 'Edge(SlistElem)'         */
/*	abgespeichert ist. Mit Hilfe dieser Verbindung der beiden Kopien jeder */
/*	Kante im Graphen, ist es moeglich den Graphen gezielt zu Durchlaufen.  */
/*******************************************************************************/

#define EdgeCopy(SlistElem) (attr_data_of_type(SlistElem,OrderedEdgeListAttrs)->edgecopy) 



/*******************************************************************************/
/*  EdgeOfTargetList(SlistElem) ist ein Boolscher Wert der dazu dient          */
/*	der im Listenelemt abgespeicherten Kante eine bestimmte Richtung       */
/*	zu geben. (siehe auch Funktion TargetNodeGet(SlistElem) in Utilities.c)*/
/*******************************************************************************/
       
#define EdgeOfTargetList(SlistElem) (attr_data_of_type(SlistElem,OrderedEdgeListAttrs)->edgeoftargetlist)


/*******************************************************************************/
/*  Traversed(SlistElem) wird in der Prozedur 'ExtenFacialCycle' benoetigt.    */
/*	(siehe auch DrawConvex.c)                                              */
/*	Diser Boolsche Wert Kennzeichnet eine schon besuchte Kante.            */
/*******************************************************************************/

#define Traversed(SlistElem) (attr_data_of_type(SlistElem,OrderedEdgeListAttrs)->traversed)


/*******************************************************************************/
/*  EdgeVisited(SlistElem) wird in der Prozedur 'SearchNodesAndEdges'          */
/*	benoetigt.Der Integer dient dazu besuchte Kanten zu kennzeichnen.      */
/*	Naeheres dazu findet man in DrawConvex.c.                              */
/*******************************************************************************/

#define EdgeVisited(SlistElem) (attr_data_of_type(SlistElem,OrderedEdgeListAttrs)->edgevisited)

typedef struct orderededgeattrs{
        double             ang;
        Slist              edgeentry;
        } *OrderedEdgeAttrs;  
                
#define EdgeEntry(TheEdge) (attr_data_of_type(TheEdge,OrderedEdgeAttrs)->edgeentry)

#define Angle(TheEdge) (attr_data_of_type(TheEdge,OrderedEdgeAttrs)->ang)

typedef int   *intpointer ;

#define Deleted(TheEdge) (*attr_data_of_type(TheEdge,intpointer))


/*******************************************************************************/
/* Die Funktion ConvexTest testet einen Graphen, ob er konvex zeichenbar ist.  */
/* Vorausgesetzt wird, dass der Graph 2-fach-zusammenhaengend ist und keine    */
/* Mehrfachkanten oder Kanten von einem Knoten auf sich selbst enthaelt.       */
/* Fuer das Abfangen der letzten beiden Faelle steht die Prozedur              */
/* 'DrawConvexPossible' zur Verfuegung (siehe PossibleConvexDraw.h).           */
/* Als Ergebnis liefert die Funktion eine Flaechenbegrenzung im Graphen        */
/* 'TheGraph', in die der Graph konvex hineingezeichnet werden kann ('the      */
/* extendible facialcycle'). Diese wird in Form einer 'Slist' zurueckgegeben,  */
/* die mit Hilfe der Prozedure 'ExtenFacialCycle' weiterbearbeitet werden      */
/* kann (siehe DrawConvex.c/h).                                                */
/*******************************************************************************/
 
extern Slist ConvexityTest(/* Sgraph TheGraph */);



/*******************************************************************************/
/* Die Prozedur gibt den, durch die Prozedur 'InitEmbededNodeEdgeAttrs',       */
/* belegten Speicherplatz wieder frei.                                         */
/*******************************************************************************/

extern void RemoveOrderedNEAttrs(/* Sgraph TheGraph */);



typedef struct deletedlistattrs{
       Snode  ringsource,ringtarget;
       Sedge  savededge;
       Slist  savededgeslist;
       } *DeletedListAttrs;
       
#define RingTarget(SlistElem) (attr_data_of_type(SlistElem,DeletedListAttrs)->ringtarget) 
#define RingSource(SlistElem) (attr_data_of_type(SlistElem,DeletedListAttrs)->ringsource)
#define SavedEdge(SlistElem) (attr_data_of_type(SlistElem,DeletedListAttrs)->savededge)       
#define SavedEdgesList(SlistElem) (attr_data_of_type(SlistElem,DeletedListAttrs)->savededgeslist) 

typedef struct savededgeslistattrs{
        Snode  source,target;
        Sedge  lsavededge;
        } *SavedEdgesListAttrs;

       
        

#define Target(SlistElem) (attr_data_of_type(SlistElem,SavedEdgesListAttrs)->target)
#define Source(SlistElem) (attr_data_of_type(SlistElem,SavedEdgesListAttrs)->source)
#define LSavedEdge(SlistElem) (attr_data_of_type(SlistElem,SavedEdgesListAttrs)->lsavededge)        

 
typedef struct nodeinfos{
        int   nodewidth,nodeheight;
        int   nodenei;
        int   nodenlp;
        int   nodefont;
        int   nodetype;
        int   nodecolor;
        bool  nodelabelvisibility;
        } *NodeInfos;  
        
#define GraphedNodeInfos(TheNode) (attr_data_of_type(TheNode,NodeInfos))  

typedef struct edgeinfos{
        int  edgetype;
        int  edgearrowlength;
        float  edgearrowangle;
        int  edgefont;
        bool edgelabelvisibility;
        int  edgecolor;
        } *EdgeInfos;
        
#define GraphedEdgeInfos(TheEdge) (attr_data_of_type(TheEdge,EdgeInfos))  
                    
#endif


 
