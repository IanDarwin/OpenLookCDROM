/* (C) Universitaet Passau 1986-1991 */
/* Planaritaetstest:
   Letzte Aenderung 19.03.91   */

/* Bei der Beschreibung des Algorithmus wird haeufiger verwiesen auf:
     John Hopcroft,Robert Tarjan: Efficient Planarity Testing
     JACM 1974 S. 549-568 (80/SA 3010 -21) [QUELLE]                         */

/* Speicherbedarf des Programms: 32*V+48*E Byte. */

#include "std.h"
#include "sgraph.h"
#include "slist.h"


#include "planar.h"

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

                     /* Vorsicht: PATH bezieht sich auf die Nummer */
                     /* des Knotens, nicht auf den Knoten selbst ! */
#define PATH(i) (attr_data_of_type(NODE[i],NODEATTR)->path)

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

/* -------------------------------------------------------------------------- */

/* Elementare Typdefinitionen */

/* Typdefinitionen fuer zusaetzliche Datenstrukturen, die in
   das 'attrs'-Feld der 'Snode'-Struktur eingehaengt werden   */

typedef struct Edgelist  *EDGELIST;
       
  struct Edgelist {
                   EDGELIST next;      /* Zeiger zum naechsten Listenelement */
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
                  }
        *NODEATTR; 

/* Elementare globale Variablen */
                             
static int  V = 0;     /* Anzahl der Knoten */
static int  E = 0;     /* Anzahl der Kanten */

static Snode *NODE;     /* Das Feld 'NODE' dient uns dazu ueber die Nummer  */
                        /* eines Knotens auf das Element der Datenstruktur  */
                        /* zugreifen zu koennen.                            */
                        /* Es gilt:        NODE[NUMBER(v)] = v              */
                        /*            und  NUMBER(NODE[i]) = i.             */
                        /* In 'planarity' wird 'NODE' genuegend Speicher-   */
                        /* platz zugewiesen.                                */
                
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
  if ((set = malloc(V+1)) == (char *) NULL)
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
                           /* (siehe auch 'planarity' !)                         */
 
/* Einfuegen eines neuen Elements 'elem' in die bei '*list' beginnende
   'EDGELIST'-Struktur; das Element wird an erster Stelle angefuegt 
   (dies ist insbesondere wichtig beim Sortieren der Kantenlisten, siehe 
   auch 'bucketsort') !                                                   */

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
      nodeattr-> edges = (EDGELIST)NULL;
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
                 int entry;
                 int next;
                }
        *FRONDS;
 
static FRONDS stack;    /* 'stack' ist ein Feld in dem die von Knoten aus- */
                        /* gehenden Rueckwaertsboegen verwaltet werden.    */
                        /* Fuer Datails siehe 'pathfinder'.                */

static int   FREE;      /* Erste freie Position in 'stack'  */

static int   p;         /* Nummer des aktuellen Pfades      */
static int   s;         /* Startknoten des aktuellen Pfades */
static int  *f;         /* f[p] = Endknoten des Pfades p    */
                        /* Fuer Details siehe 'pathfinder'. */
 
/* ------------------------------------------------------------------------ */

/* Stackoperationen fuer 'B' */

static int top;       /* Zeiger auf oberstes Element */

  /* Erzeugen eines leeren Stacks */
  
    static void empty_B()
    {
      top = -1;
    }
    
  /* Hinzufuegen eines Elements auf 'B' */
  
    static void add_to_B(x,y)
    int x,y;
    {
      top++;
      B[top].x = x;
      B[top].y = y;
    }

  /* Loeschen des obersten Elements */
  /* Der Wert des geloeschten Elements wird zurueckgegeben */
  
    static void delete_from_B(x,y)
    int *x,*y;
    {
      *x = B[top].x;
      *y = B[top].y;
      top--;
    }
  
  /* Ersetzen des obersten Elements durch (x,y) */
  
    static void replace_on_B(x,y)
    int x,y;
    {
      B[top].x = x;
      B[top].y = y;
    }
    
  /* Rueckgabe des Eintrags des obersten Elements */
  /* Das Resultat ist FALSE, falls kein Eintrag existiert, sonst TRUE */
  
    static bool top_on_B(x,y)
    int *x,*y;
    {
      if (top < 0) return FALSE;
      *x = B[top].x;
      *y = B[top].y;
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
                        l = x;
                        r = y;
                      }
                  else
                    if (top_on_B(&x,&y) && x != 0)
                      {
                        save = NEXT(x);
                        NEXT(x) = NEXT(r);
                        NEXT(r) = NEXT(l);
                        NEXT(l) = save;
                        r = x;
                      }
                    else
                      if (top_on_B(&x,&y) && y != 0)
                        r = y;
                  delete_from_B(&x,&y);
                }
              if (f[PATH(s)] < w || s == w)   /* Aenderung gegenueber dem */
                {                             /* Originalalgorithmus:     */
                  if (l == 0) l = FREE;       /* Verhindert Fehler durch  */
                  STACK(FREE) = w;            /* leeren Stack !           */
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
 
  static EDGELIST *bucket;  /* Globale Variable fuer 'bucketsort' */
  
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
   
static RESULT biconnected_component(v,w,t)
int v,w,t;

{ 
  bucketsort(v,w,t);
          /* Initialisierung der Werte */
  NEXT(-1) = NEXT(0) = 0;
  FREE = 1;
  STACK(0) = 0;
  empty_B();
  s = v;
  PATH(v) = PATH(w) = p = 1;
          /* Aufruf mit zweitem Knoten */
  return pathfinder(w);
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
                if (biconnected_component(NUMBER(v),NUMBER(w),N) == NONPLANAR)
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

/* Die folgende Prozedur initialisiert die dynamischen Strukturen   */

/* HINWEIS : Es werden 'gotos' benutzt, um den Ablauf im Fehlerfall
             deutlicher erkennbar zu machen.                        */
 
RESULT planarity(graph)
Sgraph graph;

{ char *malloc();
  RESULT result;

  if ((result = test_graph(graph)) != SUCCESS)
    return result;
  if ((v_space = (NODEATTR)malloc(V*sizeof(struct Nodeattr))) == (NODEATTR)NULL)
    { result = NO_MEM; goto pl_1; }
  if ((e_space = (EDGELIST)malloc(2*E*sizeof(struct Edgelist))) == (EDGELIST)NULL)
    { result = NO_MEM; goto pl_2; }
  init_additional_structures(graph);
  if ((NODE = (Snode *)malloc((V+1)*sizeof(Snode))) == (Snode *)NULL)
    { result = NO_MEM; goto pl_3; }
  if ((bucket = (EDGELIST *)malloc((2*V+2)*sizeof(EDGELIST))) == (EDGELIST *)NULL)
    { result = NO_MEM; goto pl_4; }
  if ((stack = (FRONDS)malloc((E+2)*sizeof(struct Fronds))) == (FRONDS)NULL)
    { result = NO_MEM; goto pl_5; }
  if ((B = (BLOCK)malloc(E*sizeof(struct Block))) == (BLOCK)NULL)
    { result = NO_MEM; goto pl_6; }
  if ((f = (int *)malloc(E*sizeof(int))) == (int *)NULL)
    { result = NO_MEM; goto pl_7; }
  result = depth_first_search(graph);
        free((char *) f);
  pl_7: free((char *) B);
  pl_6: free((char *) stack);
  pl_5: free((char *) bucket);
  pl_4: free((char *) NODE);
  pl_3: free((char *) e_space);
  pl_2: free((char *) v_space);
  pl_1: return result;
}  
