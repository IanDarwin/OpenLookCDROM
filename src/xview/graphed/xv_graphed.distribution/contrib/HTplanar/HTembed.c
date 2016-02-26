/* (C) Universitaet Passau 1986-1991 */
/* Einbettung:
   Ordnung der Kanten um einen Knoten im Uhrzeigersinn.
   Letzte Aenderung: 21.03.91   */

/* Bei der Beschreibung des Algorithmus wird haeufiger verwiesen auf:
     John Hopcroft,Robert Tarjan: Efficient Planarity Testing
     JACM 1974 S. 549-568 (80/SA 3010 -21) [QUELLE]                   */

/* Speicherbedarf des Programms: 36*V+101*E Byte. */

#include "std.h"
#include "sgraph.h"
#include "slist.h"


#include "embed.h"

/* Konstanten und Macros fuer den 'lookup'-Algorithmus (s.u.) */

#define SIGN_ON   '1'        /* 'Bit' gesetzt */
#define SIGN_OFF  '0'        /* 'Bit' geloescht */

                     /* Endpunkt der Kante 'edge', die bei 'node' beginnt */
#define ENDPOINT(node,edge) ((node) == (edge)-> tnode ? (edge)-> snode : (edge)-> tnode)

/* Macros fuer den Zugriff auf die 'NODEATTR'-Elemente */

#define NUMBER(node) (attr_data_of_type((node),NODEATTR)->number)
#define LOWPT1(node) (attr_data_of_type((node),NODEATTR)->lowpt1)
#define LOWPT2(node) (attr_data_of_type((node),NODEATTR)->lowpt2)
#define EDGES(node)  (attr_data_of_type((node),NODEATTR)->edges)

                     /* Vorsicht: PATH und ROOT beziehen sich auf die     */
                     /* Nummer des Knotens, nicht auf den Knoten selbst ! */
#define PATH(i) (attr_data_of_type(NODE[i],NODEATTR)->path)
#define ROOT(i) (attr_data_of_type(NODE[i],NODEATTR)->root)

                     /* Durchgehen aller Knoten in for-Schleife */
#define for_all_edges(node,elem) for(elem = EDGES(node); elem != (EDGELIST)NULL; elem = elem->next)

/* Konstanten fuer die Markierung der Kanten in 'dfs' */
   
#define UNUSED 'u'      /* Kante wurde bisher nicht benutzt */
#define ARC    'a'      /* Kante ist als 'Pfeil' markiert */
#define FROND  'f'      /* Kante ist als 'Bogen' markiert */

/* Macros fuer den Zugriff auf einige globale Variablen
   die im 'pathfinder' Algorithmus benoetigt werden     */

#define NEXT(i)  (stack[i+1].next)
#define STACK(i) (stack[i+1].entry)

        /* Vorsicht: 'PATHF' speichert den Pfad ,der zu dem ein 'frond'      */
        /* gehoert, und nicht wie 'PATH' den Pfad auf dem ein Knoten liegt ! */
#define PATHF(i) (stack[i+1].path)

/* Konstanten fuer Einbettung */
                         /* Beziehungen zwischen Knoten ('fronds') im   */
#define SAME  's'        /* Abhaengigkeitsgraphen: Kanten sind entweder */
#define OTHER 'o'        /* mit SAME oder OTHER markiert. */

#define UNKNOWN 'u'      /* Konstanten fuer die Festlegung auf welcher  */
#define LEFT    'l'      /* Seite ein Pfad eingebettet werden muss. */
#define RIGHT   'r'

/* -------------------------------------------------------------------------- */

/* Elementare Typdefinitionen */

/* Typdefinitionen fuer zusaetzliche Datenstrukturen, die in
   das 'attrs'-Feld der 'Snode'-Struktur eingehaengt werden   */

typedef struct Edgelist  *EDGELIST;
       
  struct Edgelist {
                   EDGELIST next;      /* Zeiger zum naechsten Listenelement */
                   EDGELIST pre;       /* Zeiger fuer doppelt verkettete Liste */
                   EDGELIST suc;       /* ('suc' = Nachfolger im Uhrzeigersinn) */
                   EDGELIST link;      /* Verbindung zur selben Kante, die in */
                                       /* umgekehrter Richtung verlaeuft */
                   Sedge    edge;      /* Urspruengliche Kante */
                   Snode    node;      /* Endpunkt der Kante */
                   char     mark;      /* Markierung (siehe 'dfs' und */
                                       /* Definition der Konstanten)  */
                  };

typedef
  struct Nodeattr {
                   int      number;    /* Werte fuer Tiefensuche */
                   int      lowpt1;
                   int      lowpt2;
                   int      path;      /* Pfad auf dem der Knoten liegt, */
                                       /* (siehe 'pathfinder') */
                   EDGELIST edges;     /* Neue ungerichtete Kantenliste */
                   EDGELIST root;      /* Wurzel der neuen geordneten,  */
                  }                    /* doppelt verketteten Kantenlisten */
       *NODEATTR; 

/* Elementare globale Variablen */
                             
static int  V = 0;     /* Anzahl der Knoten */
static int  E = 0;     /* Anzahl der Kanten */

static Snode *NODE;     /* Das Feld 'NODE' dient uns dazu ueber die Nummer  */
                        /* eines Knotens auf das Element der Datenstruktur  */
                        /* zugreifen zu koennen.                            */
                        /* Es gilt:        NODE[NUMBER(v)] = v              */
                        /*            und  NUMBER(NODE[i]) = i.             */
                        /* In 'embed' wird 'NODE' genuegend Speicherplatz   */
                        /* zugewiesen.                                      */
               
/* ====================================================================== */

/* Prozedur zum Ueberpruefen des Graphen auf Schleifen und Mehrfachkanten.
   Werden solche erkannt, wird die Meldung SELF_LOOP bzw. MULTIPLE_EDGE
   zurueckgegeben.
   Gleichzeitig wird die Anzahl der Knoten V und die Anzahl der Kanten E 
   berechnet und die Planaritaetsbedingung E <= 3V-3 ueberprueft.
   Ist diese erfuellt wird SUCCESS zurueckgegeben, sonst NONPLANAR.
   Sollte nicht genuegend Speicherplatz zur Verfuegung stehen ist das
   Resultat NO_MEM.                                                       */

/* ACHTUNG: In diesem Abschnitt wird der Eintrag 'attrs' in der 
            'Snode'-Struktur als 'int' benutzt                            */

static char *set;     /* Zeichenkette dient uns als Menge von Knoten (BITSET) */

  /* Hilfsprozedur zum Testen der Zulaessigkeit der Kante (i,j).  */
  
    static RESULT test_edge(i,j)
    int i,j;
    {
      if (j == i)
        return SELF_LOOP;
      if (set[j] == SIGN_ON)
        return MULTIPLE_EDGE;
      if (j < i)                    /* Bedingung damit die Kanten  */
        if (++E > 3*V - 3)          /* nicht doppelt gezaehlt werden */
          return NONPLANAR;
      set[j] = SIGN_ON;             /* 'set' = 'set' + Element */
      return SUCCESS;
    }
  
  
static RESULT test_graph(graph)
Sgraph graph;

{ char *malloc();
  RESULT result;
  int i;
  Snode node;
  Sedge edge;
  
  V = E = 0;
  for_all_nodes(graph,node)                     /* Stelle Anzahl der Knoten fest */
    node-> attrs = make_attr(ATTR_FLAGS,V++);   /* und nummeriere Knoten durch.  */
  end_for_all_nodes(graph,node);
  if ((set = malloc((unsigned)(V+1))) == (char *) NULL)
    return NO_MEM;                     /* Speicher fuer Menge reservieren */
  for (i = 0; i < V; i++)              /* 'set' = leere Menge */
    set[i] = SIGN_OFF;
  for_all_nodes(graph,node)
    {
      i = attr_flags(node);
      for_sourcelist(node,edge)
        if ((result = test_edge(i,attr_flags(ENDPOINT(node,edge)))) != SUCCESS)
          { free(set); return result; }
      end_for_sourcelist(node,edge);
      if (graph-> directed)             /* Falls gerichteter Graph:  */
        for_targetlist(node,edge)       /*  auch 'target'-Liste durchgehen */
          if ((result = test_edge(i,attr_flags(ENDPOINT(node,edge)))) != SUCCESS)
            { free(set); return result; }
        end_for_targetlist(node,edge);
      for_sourcelist(node,edge)          /* Gesetzte Markierungen loeschen */
        set[attr_flags(ENDPOINT(node,edge))] = SIGN_OFF;
      end_for_sourcelist(node,edge);
      if (graph-> directed)
        for_targetlist(node,edge)
          set[attr_flags(ENDPOINT(node,edge))] = SIGN_OFF;
        end_for_targetlist(node,edge);
    }
  end_for_all_nodes(graph,node);
  free(set);
  return SUCCESS;
}
   
/* ====================================================================== */
 
/* In diesem Abschnitt folgen Prozeduren zum Erzeugen der zusaetzlichen 
   Datenstrukturen 'NODEATTR' und 'EDGELIST'.                             */
   
/* ACHTUNG: Ab jetzt wird das 'attrs'-Feld der 'Snode'-Struktur als
            Element vom Typ 'NODEATTR' aufgefasst                         */

static NODEATTR v_space;   /* 'v_space' stellt genuegend Platz fuer alle (!)     */
                           /* neuen Attribute vom Typ 'NODEATTR' zur Verfuegung. */
static EDGELIST e_space;   /* Analog stellt 'e_space' genuegend Speicherplatz    */
                           /* fuer die Kanten der neu zu erzeugenden ungerich-   */
                           /* teten Adjazenzlisten zur Verfuegung.               */
                           /* (siehe auch 'embed' !)                             */
 
/* Einfuegen eines neuen Elements 'elem' in die bei '*list' beginnende
   'EDGELIST'-Struktur; das Element wird an erster Stelle angefuegt 
   (dies ist insbesondere wichtig beim Sortieren der Kantenlisten, siehe 
   auch 'link_edges' und 'bucketsort') !                                 */

static void append_edgelist(list,elem)
EDGELIST *list,elem;

{
  elem-> next = *list;
  *list = elem;
}

/* Einhaengen der 'NODEATTR'-Strukturen und Erzeugen der neuen Kantenlisten */

static void init_additional_structures(graph)
Sgraph graph;

{ NODEATTR nodeattr = v_space;
  EDGELIST edgelist = e_space;
  Snode node;
  Sedge edge;
  
  for_all_nodes(graph,node)
    {
      nodeattr-> edges = nodeattr-> root = (EDGELIST)NULL;
        for_sourcelist(node,edge)
          {
            edgelist-> edge = edge;
            edgelist-> node = ENDPOINT(node,edge);
            edgelist-> mark = UNUSED;
            append_edgelist(&nodeattr->edges,edgelist++);
          }
        end_for_sourcelist(node,edge)
      if (graph-> directed)           /* Im gerichteten Fall muss die 'target'- */
        for_targetlist(node,edge)     /* Liste auch durchgegangen werden        */ 
          {
            edgelist-> edge = edge;
            edgelist-> node = ENDPOINT(node,edge);
            edgelist-> mark = UNUSED;
            append_edgelist(&nodeattr->edges,edgelist++);
          }
        end_for_targetlist(node,edge)
      node->attrs = make_attr(ATTR_DATA,(char *)nodeattr++); 
    }
  end_for_all_nodes(graph,node);
}

/* ======================================================================== */
 
static EDGELIST *bucket;     /* Globale Variable fuer Sortieralgorithmen;       */
                             /* wird verwendet in 'link_edges' und 'bucketsort' */
 
/* Die folgende Prozedur stellt eine Verbindung ('link') zwischen der
   Kante (i,j) in der Adjazenzliste von i und der Kante (j,i) in der
   Adjazenzliste von j her.
   Dazu werden die Adjazenzlisten aller Knoten bzgl. einer vorlaeufigen
   Knotennummerierung geordnet, was anschliessend ein einfaches Setzen
   der 'links' ermoeglicht.                                             */
 
static void link_edges(graph)
Sgraph graph;

{ int i,n;
  Snode node;
  EDGELIST elem,dual;

  n = 0;
  for_all_nodes(graph,node)                  /* Vorlaeufige Nummerierung */
    NUMBER(node) = ++n;
  end_for_all_nodes(graph,node);
  for(i = 1; i <= n; i++)                    /* Leere 'buckets' initialisieren */
    bucket[i] = (EDGELIST)NULL;
  for_all_nodes(graph,node)                         /* Kanten entsprechend der */
    while((elem = EDGES(node)) != (EDGELIST)NULL)   /* Nummer des Endknotens   */
      {                                             /* in den zugehoerigen     */
        EDGES(node) = elem-> next;                  /* 'bucket' einfuegen.     */
        append_edgelist(&bucket[NUMBER(elem-> node)],elem);
      }
  end_for_all_nodes(graph,node);                    /* Kanten mit aufsteigender */
  for(i = n; i >= 1; i--)                           /* Nummerierung des Endkno- */
    while((elem = bucket[i]) != (EDGELIST)NULL)     /* tens in die Adjazenz-    */
      {                                             /* listen zurueckkopieren.  */
        bucket[i] = elem-> next;
        append_edgelist(&EDGES(ENDPOINT(elem-> node,elem-> edge)),elem);
      }
  for_all_nodes(graph,node)                         /* 'buckets' auf Anfang der */
    bucket[NUMBER(node)] = EDGES(node);             /* Adjazenzlisten setzen.   */
  end_for_all_nodes(graph,node);
  for(i = 1; i <= n; i++)
    for(elem = bucket[i]; elem != (EDGELIST)NULL; elem = elem-> next)
      {                                             /* 'links' setzen:           */
        dual = bucket[NUMBER(elem->node)];          /* Durch das Sortieren der   */
        elem-> link = dual;                         /* Kantenlisten liegt die    */
        dual-> link = elem;                         /* zu (i,j) gehoerende Kante */
        bucket[NUMBER(elem->node)] = dual-> next;   /* (j,i) immer am Anfang der */
      }                                             /* Adjazenzliste von j.      */
}

/* ======================================================================== */

/* Erzeugen der 'Slist'-Struktur mit Eintraegen der Kanten,
   geordnet im Uhrzeigersinn.                                 */
   
static void create_slist_structure(graph)
Sgraph graph;

{ Slist slist;
  Snode node;
  EDGELIST elem,last;
  
  for_all_nodes(graph,node)
    {
      slist = empty_slist;
      if ((elem = last = attr_data_of_type(node,NODEATTR)->root) != (EDGELIST)NULL)
        do
          slist = add_immediately_to_slist(slist,make_attr(ATTR_DATA,(char *)elem->edge));
        while ((elem = elem->suc) != last);
      node->attrs = make_attr(ATTR_DATA,(char *)slist);
    }
  end_for_all_nodes(graph,node)
}

/* ======================================================================== */

/* Bemerkungen zur Einbettung:
   Leider liefert uns der Planaritaetstest aus QUELLE nicht die 
   Information auf welcher Seite (LEFT oder RIGHT) ein Pfad letzt-
   endlich einzubetten ist, sondern nur Beziehungen zwischen den
   Pfaden entweder von der From
        Pfad x und Pfad y liegen auf derselben Seite (SAME)
   oder Pfad x und Pfad y liegen auf verschiedenen Seiten (OTHER).
   Diese Informationen werden im 'Dependency' Graphen gespeichert
   und spaeter in der Prozedur 'fix_paths' dazu benutzt die Seiten
   auf der die Pfade eingebettet werden muessen festzulegen.       */
    
/* Typdefinitionen fuer die Einbettung */

static char *SIDE;      /* 'SIDE[p]' verweist auf die Seite auf der der Pfad p */
                        /* eingebettet werden muss. */

/* Typdefinitionen fuer den 'Dependency' Graphen */

typedef struct Heap *HEAP;

    struct Heap {
                  int  entry;  /* Endknoten der mit 'kind' markierten Kante */
                  char kind;
                  HEAP next;   /* Naechster Knoten */
                };

static HEAP heap;       /* 'heap' stellt Speicherplatz fuer die Kanten des    */
                        /* Abhaengigkeitsgraphen zur Verfuegung; dies erspart */
                        /* den haeufigen Aufruf von 'malloc' und damit viel   */
                        /* Rechenzeit. */

static int  TOP;        /* Naechstes freie Element im 'heap' */

static HEAP *DEPEND;    /* 'DEPEND[i]' verweist auf die Kantenliste (s.o.) des */
                        /* Knotens bzw des Pfades 'i' */

static int   p;         /* Nummer des aktuellen Pfades      */
static int   s;         /* Startknoten des aktuellen Pfades */
static int  *f;         /* f[p] = Endknoten des Pfades p    */
                        /* Fuer Details siehe 'pathfinder'. */
 
/* ------------------------------------------------------------------------ */
 
/* Einfuegen einer (ungerichteten) Kante in den 'Dependency' Graphen */

static void add_to_dependency_graph(p1,p2,kind)
int p1,p2;
char kind;

{
  heap[TOP].entry = p2;           /* Einfuegen von p2 in die Adjazenz-   */
  heap[TOP].kind  = kind;         /* liste von p1. */
  heap[TOP].next  = DEPEND[p1];   /* Neues Element wird an erster Stelle */
  DEPEND[p1] = &heap[TOP++];      /* eingefuegt. */
  heap[TOP].entry = p1;           /* Analog wird p1 in die Adjazenzliste */
  heap[TOP].kind  = kind;         /* von p2 eingefuegt */
  heap[TOP].next  = DEPEND[p2];
  DEPEND[p2] = &heap[TOP++];
}

/* ------------------------------------------------------------------------ */

/* Mit Hilfe der Informationen des 'Dependency' Graphen legen die
   folgenden Prozeduren die Seiten fest auf der die einzelnen Pfade
   eingebettet werden muessen.                                      */

  /* Rekursive Hilfsprozedur zum Festlegen der Seiten; geht die
     Kanten des 'Dependency' Graphen mit Tiefensuche durch.     */

    static void fp(i,side)
    int  i;
    char side;

    { HEAP elem;

      if (SIDE[i] == UNKNOWN)
        {
          SIDE[i] = side;
          for(elem = DEPEND[i]; elem != (HEAP)NULL; elem = elem-> next)
            fp(elem-> entry,(elem-> kind == SAME ? side : (side == LEFT ? RIGHT : LEFT)));
        }
    } 
 
/* Durchgehen aller noch nicht festgelegten Seiten */

static void fix_paths(p)
int p;             /* Nummer des letzten Pfades */

{ int i;

  for (i = 1; i <= p; i++)
    if (SIDE[i] == UNKNOWN)
      fp(i,LEFT);
}

/* ------------------------------------------------------------------------ */

/* Anmerkungen zum 'embedder':
   Normalerweise erreicht ein Pfad p den zugehoerigen 'initial cycle' c
   (= PATH(s))  [-> siehe QUELLE] auf derselben Seite, auf der er ihn 
   verlassen hat.
   Bei sogenannten 'special paths' (f[p] == f[c]) kann es jedoch vorkom-
   men, dass diese auf verschiedenen Seiten liegen (SIDE[p] != SIDE[c]).
   Dieser Spezialfall wird gesondert behandelt.                          */

/* Bedeutung der Variablen:
    'root' : Kante zum folgenden Knoten im 'palm tree' [-> siehe QUELLE]
             Variable ueber Macro 'ROOT' fuer 'NODEATTR'-Struktur global
             zugaenglich.
    'down' : Kante zum vorhergehenden Knoten im 'palm tree'.
             'root' und 'down' bilden das Skelett, fuer den zugehoerigen
             Knoten, an das die eingehenden 'fronds' links bzw rechts von 
             'root' und die ausgehenden Kanten links bzw rechts von 'down'
             eingehaengt werden.
    'low'  : Unterster (d.h i.d.R erster) bzgl. des betrachteten Knotens
             eingebetteter 'frond' (wichtig fuer obigen Spezialfall !).

/* Einfuegen von 'elem' in doppelt verkettete Liste vor dem Element 'suc'  
   (bzgl. der Reihenfolge im Uhrzeigersinn) ; 'elem' wird zurueckgegeben. */

static EDGELIST embed_below(elem,suc)
EDGELIST elem,suc;
  
{
  if (suc == (EDGELIST)NULL)           /* 'elem' erstes Element ? */
    elem-> suc = elem-> pre = elem;
  else
    {
      elem-> suc = suc;
      elem-> pre = suc-> pre;
      suc-> pre-> suc = elem;
      suc-> pre = elem;
    }
  return elem;
}

/* Der 'embedder' hat dieselbe Struktur wie der Algorithmus 'pathfinder'
   und nutzt die Informationen der globalen Variablen 'PATH' und 'f' aus
   'pathfinder'.                                                         */

static EDGELIST embedder(v,u_v)
int v;
EDGELIST u_v;

{ EDGELIST v_w,save,down,low;
  int w,w0;

  low = (EDGELIST)NULL;
  down = embed_below(u_v-> link,ROOT(v));     /* 'down' initialisieren und evtl. */
  for_all_edges(NODE[v],v_w)                  /* mit Kanten vorhergehender Kom-  */
    {                                         /* ponenten verbinden.             */
      w = NUMBER(v_w-> node);
      if (v_w-> mark == ARC)
        {
          if (s == 0)
            {
              s = v;
              p = p + 1;
            }
          if (SIDE[p] == LEFT)                /* 'root' initialisieren */
            ROOT(v) = embed_below(v_w,down-> suc);
          else
            ROOT(v) = embed_below(v_w,down);
          if (s != v)
            low = embedder(w,v_w);
          else                                /* Spezialfall: siehe oben */
            if (PATH(s) != 1 && SIDE[p] != SIDE[PATH(s)] && f[p] == f[PATH(s)])
              {
                save = ROOT(w0 = f[p]);
                ROOT(w0) = low;
                SIDE[p] = SIDE[PATH(s)];
                low = embedder(w,v_w);
                ROOT(w0) = save;
              }
            else
              embedder(w,v_w);
       }
      else
        if (v_w-> mark == FROND)
          {
            if (s == 0)
              {
                s = v;
                p = p + 1;
              }
            if (SIDE[p] == LEFT)
              {
                embed_below(v_w,down-> suc);
                embed_below(v_w-> link,ROOT(w));
              }
            else
              {
                embed_below(v_w,down);
                embed_below(v_w-> link,ROOT(w)-> suc);
              }
            if (low == (EDGELIST)NULL)
              low = v_w-> link;
            s = 0;
          }
    }
  ROOT(v) = down;   /* 'ROOT' auf Element der Liste setzen */
  return low;
}
  
/* ======================================================================== */

/* Typdefinitionen fuer den 'pathfinder'-Algorithmus */

typedef
  struct Block {
                int x;
                int y;
               }
        *BLOCK;

static BLOCK B;         /* In 'B' werden die auf verschiedenen Seiten ein-  */
                        /* zubettenden Segmente als Stack von Bloecken ver- */
                        /* waltet. Fuer Details siehe 'pathfinder'.         */
   
typedef
  struct Fronds {
                 int entry;    /* Endknoten des 'fronds' */
                 int path;     /* Pfad zu dem der 'frond' gehoert */
                 int next;
                }
        *FRONDS;
 
static FRONDS stack;    /* 'stack' ist ein Feld in dem die von Knoten aus- */
                        /* gehenden Rueckwaertsboegen verwaltet werden.    */
                        /* Fuer Datails siehe 'pathfinder'.                */

static int   FREE;      /* Erste freie Position in 'stack'  */

/* ------------------------------------------------------------------------ */

/* Stackoperationen fuer 'B' */

static int top_B;       /* Zeiger auf oberstes Element */

  /* Erzeugen eines leeren Stacks */
  
    static void empty_B()
    {
      top_B = -1;
    }
    
  /* Hinzufuegen eines Elements auf 'B' */
  
    static void add_to_B(x,y)
    int x,y;
    {
      top_B++;
      B[top_B].x = x;
      B[top_B].y = y;
    }
    
  /* Loeschen des obersten Elements */
  /* Der Wert des geloeschten Elements wird zurueckgegeben */
  
    static void delete_from_B(x,y)
    int *x,*y;
    {
      *x = B[top_B].x;
      *y = B[top_B].y;
      top_B--;
    }
  
  /* Ersetzen des obersten Elements durch (x,y) */
  
    static void replace_on_B(x,y)
    int x,y;
    {
      B[top_B].x = x;
      B[top_B].y = y;
    }
    
  /* Rueckgabe des Eintrags des obersten Elements */
  /* Das Resultat ist FALSE, falls kein Eintrag existiert, sonst TRUE */
  
    static bool top_on_B(x,y)
    int *x,*y;
    {
      if (top_B < 0) return FALSE;
      *x = B[top_B].x;
      *y = B[top_B].y;
      return TRUE;
    } 
    
/* ------------------------------------------------------------------------ */
 
/* Implementierung des Algorithmus 'pathfinder' aus QUELLE S.562f  */

/* Man beachte, dass 'pathfinder' wie auch die aufrufende Prozedur
   'biconnected_component' nicht mit den Knoten sondern mit deren
   Nummern arbeitet.                                               */

/* Im Unterschied zum Algorithmus in QUELLE startet dieser erst im
   zweiten Knoten w der zweizusammenhaengenden Komponente, sodass
   die Adjazenzliste des ersten Knotens v nicht durchlaufen wird. 
   Dies ist allerdings keine Einschraenkung, da die Adjazenzliste
   von v bzgl. der betrachteten Komponente ohnehin nur einen 'ARC'
   nach w enthaelt. Da die Adjazenzliste von v aber bereits markierte
   Knoten einer anderen Komponente enthalten kann, wird auf diese
   Weise ein Durcheinander verhindert.                             */

/* Das Resultat wird NONPLANAR zurueckgegeben, falls der Graph 
   nicht planar ist, ansonsten SUCCESS.                            */   

  static RESULT pathfinder(v)
  int v;

  { int l,r,            /* Indizes fuer Eintraege von 'stack' */
        x,y,            /* Variablen fuer Blockeintraege */
        w,              /* Knoten in Adjazenzliste von v */
        save;           /* Hilfsvariable */
    EDGELIST v_w;       /* Kante von v nach w */
    
    for_all_edges(NODE[v],v_w)
      {
        w = NUMBER(v_w-> node);
        if (v_w-> mark == ARC)
          {
            if (s == 0)    /* s = 0 markiert den Anfang eines neuen Pfades */
              {
                s = v;
                p = p + 1;
                DEPEND[p] = (HEAP)NULL;
                SIDE[p] = UNKNOWN;
              }
            PATH(w) = p;
            if (pathfinder(w) == NONPLANAR) return NONPLANAR;
                    /* Die folgenden Anweisungen loeschen 'fronds' auf  */
                    /* den Stacks L,R und B, die oberhalb des aktuellen */
                    /* Knotens v liegen; diese brauchen nicht mehr be-  */
                    /* trachtet zu werden. */
            while (top_on_B(&x,&y) && (STACK(x) >= v || x == 0)
                                   && (STACK(y) >= v || y == 0))
              delete_from_B(&x,&y);
            if (top_on_B(&x,&y) && STACK(x) >= v)
              replace_on_B(0,y);
            if (top_on_B(&x,&y) && STACK(y) >= v)
              replace_on_B(x,0);
            while (NEXT(-1) != 0 && STACK(NEXT(-1)) >= v)
              NEXT(-1) = NEXT(NEXT(-1));
            while (NEXT(0) != 0 && STACK(NEXT(0)) >= v)
              NEXT(0) = NEXT(NEXT(0));
                    /* Falls bei v -> w ein neues Segment beginnt, muessen  */
                    /* alle 'fronds', die von diesem Segment ausgehen, ein- */
                    /* gesammelt und auf einer(!) Seite eingebettet werden. */
            if (PATH(w) != PATH(v))
              {
                l = 0;
                while (top_on_B(&x,&y) && (STACK(x) > f[PATH(w)] ||
                  (STACK(NEXT(-1)) != 0 && STACK(y) > f[PATH(w)])))
                  {
                    if (STACK(x) > f[PATH(w)])
                      {
                        if (STACK(y) > f[PATH(w)]) return NONPLANAR;
                        l = x;
                      }
                    else   /* STACK(y) > f[PATH(w)] */
                      {
                        save = NEXT(l);
                        NEXT(l) = NEXT(-1);
                        NEXT(-1) = NEXT(y);
                        NEXT(y) = save;
                        l = y;
                      }
                    add_to_dependency_graph(PATHF(l),PATH(w),SAME);
                    delete_from_B(&x,&y);
                  }
                delete_from_B(&x,&y);
                if (x != 0)
                  add_to_B(x,y);
                else
                  if (l != 0 || y != 0)
                    add_to_B(l,y);
                NEXT(-1) = NEXT(NEXT(-1));
              }
          }
        else
          if (v_w-> mark == FROND)
            {
              if (s == 0)
                {
                  s = v;
                  p = p + 1;
                  DEPEND[p] = (HEAP)NULL;
                  SIDE[p] = UNKNOWN;
                }
              f[p] = w;
                      /* Es muessen evtl. Bloecke zwischen den Stacks L  */
                      /* und R verschoben werden, um den neuen 'frond' w */
                      /* ueberschneidungsfrei mit zuvor eingebetteten    */
                      /* 'fronds' aus L einfuegen zu koennen.            */
              l =  0;
              r = -1;
              while ((NEXT(l) != 0 && STACK(NEXT(l)) > w) ||
                     (NEXT(r) != 0 && STACK(NEXT(r)) > w))
                {
                  if (top_on_B(&x,&y) && x != 0 && y != 0)
                    if (STACK(NEXT(l)) > w)
                      {
                        add_to_dependency_graph(p,PATHF(NEXT(l)),OTHER);
                        if (STACK(NEXT(r)) > w) return NONPLANAR;
                        save = NEXT(r);
                        NEXT(r) = NEXT(l);
                        NEXT(l) = save;
                        save = NEXT(x);
                        NEXT(x) = NEXT(y);
                        NEXT(y) = save;
                        l = y;
                        r = x;
                      }
                    else
                      {
                        add_to_dependency_graph(p,PATHF(NEXT(r)),OTHER);
                        l = x;
                        r = y;
                      }
                  else
                    if (top_on_B(&x,&y) && x != 0)
                      {
                        add_to_dependency_graph(p,PATHF(NEXT(l)),OTHER);
                        save = NEXT(x);
                        NEXT(x) = NEXT(r);
                        NEXT(r) = NEXT(l);
                        NEXT(l) = save;
                        r = x;
                      }
                    else
                      if (top_on_B(&x,&y) && y != 0)
                        {
                          add_to_dependency_graph(p,PATHF(NEXT(r)),OTHER);
                          r = y;
                        }
                  delete_from_B(&x,&y);
                }
              if (f[PATH(s)] < w || s == w)   /* Aenderung gegenueber dem */
                {                             /* Originalalgorithmus:     */
                  if (l == 0) l = FREE;       /* Verhindert Fehler durch  */
                  STACK(FREE) = w;            /* leeren Stack !           */
                  PATHF(FREE) = p;
                  NEXT(FREE) = NEXT(0);
                  NEXT(0) = FREE;
                  FREE = FREE + 1;
                }
              if (r == -1) r = 0;
              if (l != 0 || r != 0 || v != s)
                add_to_B(l,r);
              if (v != s)            /* Falls der Pfad ein neues Segment ist,  */
                {                    /* wird eine Endemarkierung auf R erzeugt */
                  STACK(FREE) = 0;
                  NEXT(FREE) = NEXT(-1);
                  NEXT(-1) = FREE;
                  FREE = FREE + 1;
                }
              s = 0;
            }  /* if */
      }  /* for_all_edges */
    return SUCCESS;
  }

/* ------------------------------------------------------------------------ */

/* Funktion nach der die Kanten in Bucketsort sortiert werden */

    static int q(v,v_w)
    Snode v;
    EDGELIST v_w;
    
    { Snode w = v_w->node;
    
      if (v_w->mark == FROND)
        return 2*NUMBER(w);
      else
        if (v_w->mark == ARC)
          if (LOWPT2(w) >= NUMBER(v))
            return 2*LOWPT1(w);
          else
            return 2*LOWPT1(w)+1;
        else
          return 2*NUMBER(v); /* Unmarkierte Kanten muessen */
    }                         /* nicht sortiert werden      */
   
/* Prozedur zum Sortieren der Kantenliste der Knoten mit Nummer
   zwischen w und t. Der Knoten v enthaelt in seiner Kantenliste
   nur einen zur Komponente gehoerenden 'ARC' und muss nicht 
   sortiert werden (Im Uebrigen DARF diese Liste nicht sortiert
   werden, da 'dfs' noch auf ihr arbeitet).
   Da die Werte nach denen sortiert wird (siehe q) zwischen
   2*v und 2*t+1 liegen, werden nur die 'buckets' zwischen
   diesen Werten betrachtet.                                     */

/* Fuer Details des Algorithmus siehe QUELLE S.556 */

  static void bucketsort(v,w,t)
  int v,w,t;

  { int i;
    EDGELIST elem;
  
    for(i = 2*v; i<= 2*t+1; i++)
      bucket[i] = (EDGELIST)NULL;
    for(i = w; i <= t; i++)
      while((elem = EDGES(NODE[i])) != (EDGELIST)NULL)
        {
          EDGES(NODE[i]) = elem-> next;
          append_edgelist(&bucket[q(NODE[i],elem)],elem);
        }
    for(i = 2*t+1; i >= 2*v; i--)
      while((elem = bucket[i]) != (EDGELIST)NULL)
        {
          bucket[i] = elem-> next;
          append_edgelist(&EDGES(ENDPOINT(elem->node,elem->edge)),elem);
        }
  }


/* ------------------------------------------------------------------------- */

/* Analyse einer zweizusammenhaengenden Komponente mit erstem 
   Knoten v, zweitem Knoten w und letztem Knoten t. Die Angabe
   von w ist notwendig, da w nicht notwendigerweise gleich v+1 
   ist; erst ab Knoten w ist sichergestellt, dass die Knoten in 
   der betrachteten Komponente aufeinanderfolgende Nummern haben.   */

/* Als Ergebnis wird das Resultat von 'pathfinder' zurueckgegeben.  */
   
static RESULT biconnected_component(v,v_w,w,t)
int v,w,t;
EDGELIST v_w;

{ EDGELIST low;

  bucketsort(v,w,t);
          /* Initialisierung der Werte */
  NEXT(-1) = NEXT(0) = 0;
  FREE = 1;
  STACK(0) = 0;
  empty_B();
  s = v;
  PATH(v) = PATH(w) = p = 1;
  DEPEND[p] = (HEAP)NULL;
  SIDE[p] = UNKNOWN;
  TOP = 0;
          /* Aufruf mit zweitem Knoten */
  if (pathfinder(w) == NONPLANAR)
    return NONPLANAR;
          /* Festlegen der Seiten */
  fix_paths(p);          /* p ist die letzte vergebene Pfadnummer */
  s = v;
  p = 1;
  ROOT(v) = embed_below(v_w,ROOT(v));              /* Neue Komponente evtl. mit */
  if ((low = embedder(w,v_w)) != (EDGELIST)NULL)   /* vorhergehender, beginnend */
    ROOT(v) = low;                                 /* bei v, verbinden.         */
  return SUCCESS;
}     
        
/* ======================================================================== */ 

static int   N = 0;     /* Groesste vergebene 'dfs'-Nummer an einen Knoten, */
                        /* markiert das Ende einer 'biconnected'-Komponente */

/* Implementierung des Algorithmus 'dfs' wie in QUELLE S. 553        */

/* Ergaenzend zu obigem Algorithmus wird neben der Bestimmung der
   NUMBER,LOWPT1 und LOWPT2-Werte auch die Unterteilung in zwei-
   zusammenhaengende Komponenten vorgenommen:
   Wie man leicht nachprueft zerfaellt der Graph mit der Kante 
   v -> w in 2 zweizusammenhaengende Komponenten, falls
         LOWPT1(w) = NUMBER(w) ( > NUMBER(v) )
   oder  LOWPT1(w) = NUMBER(v),
   also  LOWPT1(w) >= NUMBER(v).                                      */  
   
  static RESULT dfs(v,u)
  Snode v,u;
    
  { Snode w;
    EDGELIST v_w;    /* Kante von v nach w */
      
    NUMBER(v) = LOWPT1(v) = LOWPT2(v) = ++N;
    NODE[N] = v;                         /* Zeiger auf 'Snode'-Struktur setzen */
    for_all_edges(v,v_w)
      {
        w = v_w->node;
        if (NUMBER(w) == 0)
          {
            v_w->mark = ARC;
            if (dfs(w,v) == NONPLANAR)
              return NONPLANAR;
            if (LOWPT1(w) >= NUMBER(v))  /* Bedingung fuer den Beginn einer */
              {                          /* neuen 'biconnected'-Komponente  */
                if (biconnected_component(NUMBER(v),v_w,NUMBER(w),N) == NONPLANAR)
                  return NONPLANAR;
                N = NUMBER(w)-1;         /* Nummerierung beginnt wieder bei w */
                v_w->mark = UNUSED;      /* Verbindung zur bereits bearbeiteten */
              }                          /* Komponente unterbrechen */
            else
              if (LOWPT1(w) < LOWPT1(v))
                {
                  LOWPT2(v) = minimum(LOWPT1(v),LOWPT2(w));
                  LOWPT1(v) = LOWPT1(w);
                }
              else
                if (LOWPT1(w) == LOWPT1(v))
                  LOWPT2(v) = minimum(LOWPT2(v),LOWPT2(w));
                else
                  LOWPT2(v) = minimum(LOWPT2(v),LOWPT1(w));
          }
        else
          if (NUMBER(w) < NUMBER(v) && w != u)
            {
              v_w->mark = FROND;
              if (NUMBER(w) < LOWPT1(v))
                {
                  LOWPT2(v) = LOWPT1(v);
                  LOWPT1(v) = NUMBER(w);
                }
              else
                if (NUMBER(w) > LOWPT1(v))
                  LOWPT2(v) = minimum(LOWPT2(v),NUMBER(w));
            }
      }
    return SUCCESS;
  }

/* Initialisierungsprozedur fuer 'dfs'; geht alle noch nicht 
   nummerierten Knoten durch.                                */
 
static RESULT depth_first_search(graph)
Sgraph graph;

{ Snode node;
  
  for_all_nodes(graph,node)
    NUMBER(node) = 0;
  end_for_all_nodes(graph,node);
  N = 0;
  for_all_nodes(graph,node)
    if (NUMBER(node) == 0)
      if (dfs(node,(Snode)NULL) == NONPLANAR)
        return NONPLANAR;
  end_for_all_nodes(graph,node);
  return SUCCESS;
}

/* ======================================================================== */

/* Die folgende Prozedur initialisiert die dynamischen Strukturen    */

/* HINWEIS : In dieser Prozedur werden 'gotos' verwendet, um den Ab-
             lauf des Programms im Fehlerfall deutlicher zu machen.  */
 
RESULT embed(graph)
Sgraph graph;

{ RESULT result;

  if ((result = test_graph(graph)) != SUCCESS)
    return result;
  if ((v_space = (NODEATTR)malloc((unsigned)V*sizeof(struct Nodeattr))) == (NODEATTR)NULL)
    { result = NO_MEM; goto em_1; }
  if ((e_space = (EDGELIST)malloc((unsigned)(2*E)*sizeof(struct Edgelist))) == (EDGELIST)NULL)
    { result = NO_MEM; goto em_2; }
  init_additional_structures(graph);
  if ((NODE = (Snode *)malloc((unsigned)(V+1)*sizeof(Snode))) == (Snode *)NULL)
    { result = NO_MEM; goto em_3; }
  if ((bucket = (EDGELIST *)malloc((unsigned)(2*V+2)*sizeof(EDGELIST))) == (EDGELIST *)NULL)
    { result = NO_MEM; goto em_4; }
  if ((stack = (FRONDS)malloc((unsigned)(E+2)*sizeof(struct Fronds))) == (FRONDS)NULL)
    { result = NO_MEM; goto em_5; }
  if ((B = (BLOCK)malloc((unsigned)E*sizeof(struct Block))) == (BLOCK)NULL)
    { result = NO_MEM; goto em_6; }
  if ((f = (int *)malloc((unsigned)E*sizeof(int))) == (int *)NULL)
    { result = NO_MEM; goto em_7; }
  if ((heap = (HEAP)malloc((unsigned)(2*E)*sizeof(struct Heap))) == (HEAP)NULL)
    { result = NO_MEM; goto em_8; }
  if ((DEPEND = (HEAP *)malloc((unsigned)E*sizeof(HEAP))) == (HEAP *)NULL)
    { result = NO_MEM; goto em_9; }
  if ((SIDE = malloc((unsigned)E)) == NULL)
    { result = NO_MEM; goto em_10; }
  link_edges(graph);
  if ((result = depth_first_search(graph)) == SUCCESS)
    create_slist_structure(graph);
         free((char *) SIDE);
  em_10: free((char *) DEPEND);
  em_9:  free((char *) heap);
  em_8:  free((char *) f);
  em_7:  free((char *) B);
  em_6:  free((char *) stack);
  em_5:  free((char *) bucket);
  em_4:  free((char *) NODE);
  em_3:  free((char *) e_space);
  em_2:  free((char *) v_space);
  em_1:  return result;
}
