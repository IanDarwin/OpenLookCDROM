/* (C) Universitaet Passau 1986-1991 */
/*####################################################################################################
  #                                                                                                  #
  #  Der Algorithmus arbeitet in zwei Schritten :                                                    #
  #  Zuerst wird der bisherige Graph rekursiv (dfs) durchgegangen und dabei alle Flaechen zusammen-  #
  #  gesucht. Hierzu benoetigt er die orientierte Kantenliste von 'embed'.                           #
  #  'dualgr' ist dabei die Hauptprozedur. Sie wird von 'startdual' aus aufgerufen. Am Ende dieses   #
  #  Schrittes ist zu jeder Kante bekannt, welche Flaeche links bzw. rechts davon liegt.             #
  #  (s.a. Datenstrukturen, Flaeche)                                                                 #
  #  Im naechsten Schritt wird wieder der urspruengliche Graph rekursiv durchsucht. Hierbei werden   #
  #  die Flaechen des Graphen zu Knoten des Dualgraphen. Benachbarte Flaechen werden ueber Kanten    #
  #  verbunden.                                                                                      #
  #  Hier ist 'make_nodes_and_edges_of_dualgraph' die Hauptprozedur, sie wird von 'make_dualgraph'   #
  #  aufgerufen.                                                                                     #
  #  Beide Hauptprozeduren sind rekursiv und arbeiten mit Seiteneffekten!                            #
  #                                                                                                  #
  #  Als Ergebnis wird der Dualgraph zurueckgegeben. Das Attributfeld der Knoten enthaelt eine Slist #
  #  von (Zeigern auf) Kanten des urspruenglichen Graphen, die die Flaeche begrenzten aus der der    #
  #  Dualgraphknoten hervorging. Das Attributfeld der Kanten enthaelt einen Zeiger auf die Kante des #
  #  urspruenglichen Graphen, ueber die die Dualgraphkante laeuft.                                   #
  #                                                                                                  #
  ####################################################################################################*/

#include "std.h"
#include "sgraph.h"
#include "slist.h"
#include "graphed.h"

/*####################################################################################################
  #                                                                                                  #
  #  Typdefinitionen, die waehrend dual gebraucht werden :                                           #
  #                                                                                                  #
  #  Plane : Eine Flaeche ist in der Hauptsache eine Liste der sie umgebenden Kanten.                #
  #            (planeedges -> 'dualgr')                                                              #
  #          Daneben existiert noch eine Markierung (visited -> 'make_nodes_and_edges_of_dualgraph)  #
  #          und ein Verweis auf den entsprechenden Knoten im Dualgraph (dualnode -> 'make_nodes_    #
  #          and_edges_of_dualgraph').                                                               #
  #  NodeMarks : Eine Knotenmarkierung besteht aus einer Markierung, ob der Knoten bereits besucht   #
  #              wurde (visited -> 'dual','startdual','dualgr') sowie der im Uhrzeigersinn geordneten#
  #              Kantenliste (edges -> 'startdual','dualgr').                                        #
  #              Diese wird von 'embed' uebernommen.                                                 #
  #  EdgeMarks : Eine Kantenmarkierung besteht aus einer Markierung, ob die Kante bereits passiert   #
  #              wurde (visited -> 'startdual','dualgr'), sowie der linken und rechten Flaeche bzgl. #
  #              dieser Kante.                                                                       #
  #  Side : Da die Begriffe "links" und "rechts" immer vom Standpunkt abhaengen wird hier so vorge-  #
  #         gangen, dass beim Aufruf einer Prozedur immer die Seite so angegeben wird, wie der Be-   #
  #         trachter sie sieht. Innerhalb der Prozeduren wird sie dann in eine "absolute" Seite      #
  #         umgerechnet. Bei der "absoluten" Seite wird davon ausgegangen, dass die Kante vom        #
  #         kleineren zum groesseren Knoten durchlaufen wird. Der kleinere Knoten ist hierbei der    #
  #         Knoten, dessen Zeiger auf eine niedrigere Adresse zeigt.                                 #
  # Fuer die Nodemarks und Edgemarks wird in 'dual' Speicher reserviert, die Variablen initialisiert #
  # und der Speicher zum Schluss wieder freigegeben.                                                 #
  # Die Flaechen werden in 'startdual' und 'dualgr' aufgebaut.                                       #
  # Hierbei werden folgende Funktionen verwendet :                                                   #
  #    'set_plane'               'set_plane_and_append'                                              #
  #    'get_plane'               'make_plane'                                                        #
  #    'add_planes'                                                                                  #
  # In 'make_nodes_and_edges_of_dualgraph' wird die Slist planeedges rekursiv abgearbeitet und       #
  # geloescht und danach der Speicher fuer Plane selbst freigegeben.                                 #
  #                                                                                                  #
  ####################################################################################################*/

typedef struct plane
  { int visited;
    Slist planeedges;
    Snode dualnode
  } *Plane;

typedef struct nodemarks
  { int visited;
    Slist edges
  } *NodeMarks;

typedef struct edgemarks
  { int passed;
    Plane left, right
  } *EdgeMarks;

typedef enum side
  {left, right} Side;

#define VISITED(node) (((NodeMarks)attr_data(node))->visited)
#define EDGELIS(node) (((NodeMarks)attr_data(node))->edges)

#define EDGE(slist) ((Sedge)attr_data(slist))

#define PASSED(edge) (((EdgeMarks)attr_data(edge))->passed)
#define LEFT(edge) (((EdgeMarks)attr_data(edge))->left)
#define RIGHT(edge) (((EdgeMarks)attr_data(edge))->right)

/*######################################################################
  ######################################################################

                         Testproceduren

  ######################################################################
  ######################################################################*/


void message_edge (edge)
  Sedge edge;

  {
   if (edge->snode < edge->tnode)
     message("%s-%s ",edge->snode->label,edge->tnode->label);
   else
     message("%s-%s ",edge->tnode->label,edge->snode->label);
  }

void message_dualnode(list)
  Slist list;

  {
   Slist l;
   for_slist(list,l)
     {
      message_edge(EDGE(l));
     }
   end_for_slist(list,l);
   message("\n");
  }

void message_graph(DualGraph)
  Sgraph DualGraph;

  {
   Snode node;
   Sedge edge;
   for_all_nodes(DualGraph,node)
     {
      message("Dualgraphknoten : ");
      message_dualnode((Slist)attr_data(node));
      message("   laeuft ueber `alte Kanten` :");
      for_sourcelist(node,edge)
        message_edge((Sedge)attr_data(edge));
      end_for_sourcelist(node,edge);
      message("\n");
     }
   end_for_all_nodes(DualGraph,node);
  }

/*####################################################################
  ####################################################################*/


/*####################################################################################################
  #                                                                                                  #
  #                                  Edge_in_slist                                                   #
  #                                                                                                  #
  # Edge_in_slist sucht in der geordneten Kantenliste von node2 (!) nach der Kante (node1,node2).    #
  #                                                                                                  #
  ####################################################################################################*/

static Slist Edge_in_slist(node1,node2)    /* Pointer auf Slist von node2, die auf */
  Snode node1,node2;                       /* Kante(node1,node2) zeigt.            */

  {
   Slist l;
   for_slist(EDGELIS(node2),l)
     {
      if ((EDGE(l)->snode == node1) ||
          (EDGE(l)->tnode == node1))
        return l;
      else ;
     }
   end_for_slist(EDGELIS(node2),l);
  }

/*####################################################################################################
  #                                                                                                  #
  #                                         Edge                                                     #
  #                                                                                                  #
  # Gibt die Kante (node1,node2) zurueck. Benutzt dazu 'Edge_in_slist'.                              #
  #                                                                                                  #
  ####################################################################################################*/

static Sedge Edge(node1,node2)
  Snode node1,node2;

  {
   return EDGE(Edge_in_slist(node1,node2));
  }

/*####################################################################################################
  #                                                                                                  #
  #                                     set_plane                                                    #
  #                                                                                                  #
  # Setzt 'plane' auf die Seite 'lr' der Kante (node1,node2).                                        #
  #                                                                                                  #
  ####################################################################################################*/

static void set_plane(node1,node2,edge12,plane,lr)
  Snode node1,node2;
  Sedge edge12;
  Plane plane;
  Side lr;

  {
   if (node1 < node2) /* node1 = sourcenode */
     {
      if (lr == left)
        LEFT(edge12) = plane;
      else
        RIGHT(edge12) = plane;
     }
   else /* node2 = sourcenode */
     {
      if (lr == left)
        RIGHT(edge12) = plane;
      else
        LEFT(edge12) = plane;
     }
  }

/*####################################################################################################
  #                                                                                                  #
  #                                 set_plane_and_append                                             #
  #                                                                                                  #
  # Setzt 'plane' auf die Seite 'lr' der Kante (node1,node2) und haengt (node1,node2) an die         #
  # 'planeedges' von 'plane' and.                                                                    #
  #                                                                                                  #
  ####################################################################################################*/

static void set_plane_and_append(node1,node2,edge12,plane,lr)
  Snode node1,node2;
  Sedge edge12;
  Plane plane;
  Side lr;

  {
   set_plane(node1,node2,edge12,plane,lr);
   add_to_slist(plane->planeedges,make_attr(ATTR_DATA,(char *)edge12));
  }

/*####################################################################################################
  #                                                                                                  #
  #                                   get_plane                                                      #
  #                                                                                                  #
  # get_plane gibt als Ergebnis die Flaeche zurueck, die auf der Seite lr von (node1,node2) liegt.   #
  #                                                                                                  #
  ####################################################################################################*/

static Plane get_plane(node1,node2,lr)
  Snode node1,node2;
  Side lr;

  {
   if (node1 < node2) /* node1 = sourcenode */
     {
      if (lr == left)
        return LEFT(Edge(node1,node2));
      else
        return RIGHT(Edge(node1,node2));
     }
   else /* node2 = sourcenode */
     {
      if (lr == left)
        return RIGHT(Edge(node1,node2));
      else
        return LEFT(Edge(node1,node2));
     }
  }

/*####################################################################################################
  #                                                                                                  #
  #                                    make_plane                                                    #
  #                                                                                                  #
  # make_plane reserviert den Speicher fuer eine neue Flaeche, bildet die Slist 'planeedges' und     #
  # haengt gleich den ersten Eintrag ein.                                                            #
  #                                                                                                  #
  ####################################################################################################*/

static Plane make_plane(node1,node2)
  Snode node1,node2;

  {
   char *malloc();
   Plane plane = (Plane)malloc(sizeof(struct plane));
   plane->planeedges = new_slist(make_attr(ATTR_DATA,(char *)Edge(node1,node2)));
   plane->visited = 0;
   return plane;
  }

/*####################################################################################################
  #                                                                                                  #
  #                                      add_planes                                                  #
  #                                                                                                  #
  # Die Flaechen die auf der Seite lr1 von (node1,node2) und die auf der Seite lr2 von (node3,node4) #
  # liegen werden verknuepft und bilden danach eine Flaeche.                                         #
  # Falls die Flaechen vorher schon gleich waren passiert nichts.                                    #
  # Andernfalls werden die 'planeedges' der zweiten Flaeche an die 'planeedges' der ersten Flaeche   #
  # angehaengt. Daraufhin werden alle Zeiger, die auf die zweite Flaeche zeigen, auf die erste       #
  # Flaeche gesetzt und der Speicher fuer die zweite Flaeche wieder freigegeben.                     #
  # Folgende Prozeduren werden in 'add_planes' benutzt :                                             #
  #       'get_plane'                                                                                #
  #                                                                                                  #
  ####################################################################################################*/

static void add_planes(node1,node2,lr1,node3,node4,lr2)
  Snode node1, node2, node3, node4;
  Side lr1, lr2;

/* addiert die Flaechen, setzt die Pointer um und loescht die zweite Flaeche */

  {
   Plane oldplane, newplane;
   Slist l;
   newplane = get_plane(node1,node2,lr1);
   oldplane = get_plane(node3,node4,lr2);
   if (oldplane == newplane)
     ;
   else
     {
      newplane->planeedges = add_slists(newplane->planeedges,oldplane->planeedges);
      for_slist(oldplane->planeedges,l)
        {
         if (LEFT(EDGE(l)) == oldplane)
           LEFT(EDGE(l)) = newplane;
         else
           RIGHT(EDGE(l)) = newplane;
        }
      end_for_slist(oldplane->planeedges,l);
      free_slist(oldplane->planeedges);
      free(oldplane);
     }
  }

/*####################################################################################################
  #                                                                                                  #
  #                                     dualgr                                                       #
  #                                                                                                  #
  # 'dualgr' bildet das Kernstueck des ersten Teils des Algorithmus.                                 #
  # 'dualgr' wird von 'startdual' aufgerufen.                                                        #
  # Folgende Prozeduren und Macros werden in 'dualgr' benutzt :                                      #
  #        'PASSED'                 'VISITED'                                                        #
  #        'Edge'                   'Edge_in_slist'                                                  #
  #        'add_planes'             'set_plane_and_append'                                           #
  #        'set_plane'              'get_plane'                                                      #
  #        'make_plane'                                                                              #
  # Vorgehensweise :                                                                                 #
  #    Der Aufruf von 'dualgr' stellt das Durchlaufen einer Kante von node1 nach node2 dar.          #
  #    Es werden nur solche Kanten durchlaufen, die noch nicht besucht wurden.                       #
  #    a.) node2 wurde noch nicht besucht                                                            #
  #        Nun wird von der Kante (node1,node2) ausgehend die linke Nachbarkante gesucht (leftedge). #
  #          i.) leftedge = (node1,node2)                                                            #
  #              => linke und rechte Flaeche von leftedge stimmen ueberein.                          #
  #                 Ende der Rekursion.                                                              #
  #         ii.) leftedge wurde bereits durchlaufen                                                  #
  #              => die linken Flaechen von (node1,node2) und leftedge stimmen ueberein.             #
  #        iii.) leftedge muss als naechste durchlaufen werden.                                      #
  #              linke Flaeche von leftedge = linke Flaeche von (node1,node2)                        #
  #              rechte Flaeche von leftedge = falls die rechte Nachbarkante (rightedge)             #
  #                                            von leftedge bereits durchlaufen wurde :              #
  #                                              linke Flaeche von rightedge + leftedge              #
  #                                            sonst :                                               #
  #                                              neue Flaeche, die nur aus leftedge besteht.         #
  #              Rekursiver Aufruf von dualgr(node2,leftnode).                                       #
  #        Auf diese Weise werden im Uhrzeigersinn alle Kanten um node2 rekursiv abgearbeitet.       #
  #    b.) node2 wurde bereits besucht                                                               #
  #        Suche bzgl. (node1,node2) die linke (leftedge) und die rechte (rightedge) Nachbarkante.   #
  #        Falls left/rightedge bereits durchlaufen wurde gilt :                                     #
  #          linke/rechte Flaeche von (node1,node2) = linke/rechte Flaeche von left/rightedge        #
  #        Ende der Rekursion.                                                                       #
  #                                                                                                  #
  ####################################################################################################*/

static void dualgr (node1,node2)
  Snode node1, node2;

  {
   Snode leftnode, lastnode, rightnode;
   Slist leftlist, rightlist, l;
   Sedge leftedge, rightedge;
   PASSED(Edge(node1,node2)) = 1;      /* Kantenmarkierung */
   if (VISITED(node2) == 0)
     {
      VISITED(node2) = 1;

      leftlist = Edge_in_slist(node1,node2)->suc;
      leftedge = EDGE(leftlist);
      if ((leftnode = leftedge->snode) == node2)
        leftnode = leftedge->tnode;
      else ;
         /* suche linkeste Kante bzgl. (node1,node2) = leftnode */

      if (leftnode == node1)
        {
         add_planes(node1,node2,left,node1,node2,right);
        }
      else
        {
         if (PASSED(leftedge) == 1)
           {
            add_planes(node2,leftnode,left,node1,node2,left);
           }
         else
           {
            set_plane_and_append(node2,leftnode,leftedge,
                                 get_plane(node1,node2,left),
                                 left);

            rightlist = leftlist->suc;
            rightedge = EDGE(rightlist);
            if ((rightnode = rightedge->snode) == node2)
              rightnode = rightedge->tnode;
            else ;
               /* suche rechten Nachbarn von (node2,leftnode) */

            if (PASSED(rightedge) == 0)
              set_plane(node2,leftnode,leftedge,make_plane(node2,leftnode),right);
            else
              {
               set_plane_and_append(node2,leftnode,leftedge,
                                    get_plane(node2,rightnode,left),
                                    right);
              }
            dualgr(node2,leftnode);

            if (PASSED(rightedge) == 1)
              add_planes(node2,leftnode,right,node2,rightnode,left);
            else ;
               /* Uebernehmen der neuen Informationen beim Backtracken */

            while(1 == 1)
             {
              lastnode = leftnode;
              leftlist = leftlist->suc;
              leftedge = EDGE(leftlist);
              if ((leftnode = leftedge->snode) == node2)
                leftnode = leftedge->tnode;
              else ;
                 /* suche linkeste Kante bzgl. (node2,lastnode) = leftnode */

              if (leftnode == node1)
                {
                 break;
                }
              else
                {
                 if (PASSED(leftedge) == 1)
                   {
                    add_planes(node2,leftnode,left,node2,lastnode,right);
                   }
                 else
                   {
                    set_plane_and_append(node2,leftnode,leftedge,
                                         get_plane(node2,lastnode,right),
                                         left);

                    rightlist = leftlist->suc;
                    rightedge = EDGE(rightlist);
                    if ((rightnode = rightedge->snode) == node2)
                      rightnode = rightedge->tnode;
                    else ;
                       /* suche rechten Nachbarn von (node2,leftnode) */

                    if (PASSED(rightedge) == 0)
                      set_plane(node2,leftnode,leftedge,make_plane(node2,leftnode),right);
                    else
                      set_plane_and_append(node2,leftnode,leftedge,get_plane(node2,rightnode,left),right);
                    dualgr(node2,leftnode);

                    if (PASSED(rightedge) == 1)
                      add_planes(node2,leftnode,right,node2,rightnode,left);
                    else ;
                       /* Uebernehmen der neuen Informationen beim Backtracken */
                   }
                }
              }
           }
        }
     }
   else  /* node2 bereits besucht */
     {
      leftlist = Edge_in_slist(node1,node2)->suc;
      leftedge = EDGE(leftlist);
      if (PASSED(leftedge) == 1)
        {
         if ((leftnode = leftedge->snode) == node2)
           leftnode = leftedge->tnode;
         else ;
         add_planes(node1,node2,left,node2,leftnode,left);
        }
      else ;
      rightlist = leftlist->pre->pre;
      rightedge = EDGE(rightlist);
      if (PASSED(rightedge) == 1)
        {
         if ((rightnode = rightedge->snode) == node2)
           rightnode = rightedge->tnode;
         else ;
         add_planes(node1,node2,right,node2,rightnode,right);
        }
      else ;
     }
  }

/*####################################################################################################
  #                                                                                                  #
  #                                       nextNode                                                   #
  #                                                                                                  #
  # nextNode sucht in der geordneten Kantenliste einen beliebigen Knoten aus, so dass gilt :         #
  #         PASSED((node1,node2)) = 0                                                                #
  # Falls kein solcher Knoten existiert wird 'NULL' zurueckgegeben.                                  #
  # Folgende Macros werden in nextnode benutzt :                                                     #
  #    'EDGELIS'            'PASSED'                                                                 #
  #                                                                                                  #
  ####################################################################################################*/

static Snode nextNode (node)
  Snode node;

  {
   Snode next = NULL;
   Slist l;
   for_slist(EDGELIS(node),l)
     {
      if (PASSED(((Sedge)attr_data(l))) == 0) 
        {
         if ((next = EDGE(l)->snode) == node)
           next = EDGE(l)->tnode;
         else ;
         break;
        }
     }
   end_for_slist(EDGELIS(node),l);
   return (next);
  }

/*####################################################################################################
  #                                                                                                  #
  #                              startdual                                                           #
  #                                                                                                  #
  # Startet die Hauptprozedur -> 'dualgr' an.                                                        #
  # Hierzu wird ein beliebiger Nachbarknoten ausgewaehlt (-> 'nextnode').                            #
  # Folgende Prozeduren und Macros werden in 'startdual' benutzt :                                   #
  #    'VISITED'                    'nextnode'                                                       #
  #    'Edge_in_slist'              'set_plane_and_append'                                           #
  #    'get_plane'                  'set_plane'                                                      #
  #    'make_plane'                 'dualgr'                                                         #
  #                                                                                                  #
  ####################################################################################################*/
  
static void startdual (node)
  Snode node;

  {
   Snode nextnode, leftnode, rightnode;
   Slist leftlist, rightlist;
   Sedge nextedge, leftedge, rightedge;

   VISITED(node) = 1;
   while ((nextnode = nextNode(node)) != NULL)
     {
      nextedge = Edge(node,nextnode);
      leftlist = Edge_in_slist(nextnode,node)->pre;
      leftedge = EDGE(leftlist);
      if (PASSED(leftedge) == 1)
        {
         if ((leftnode = leftedge->snode) == nextnode)
           leftnode = leftedge->tnode;
         else ;
         set_plane_and_append(node,nextnode,nextedge,get_plane(node,leftnode,right),left);
        }
      else
        set_plane(node,nextnode,nextedge,make_plane(node,nextnode),left);
      rightlist = leftlist->suc->suc;
      rightedge = EDGE(rightlist);
      if (PASSED(rightedge) == 1)
        {
         if ((rightnode = rightedge->snode) == nextnode)
           rightnode = rightedge->tnode;
         else ;
         set_plane_and_append(node,nextnode,nextedge,get_plane(node,rightnode,left),right);
        }
      else 
        set_plane(node,nextnode,nextedge,make_plane(node,nextnode),right);
      dualgr(node,nextnode);
     }
  }

/*####################################################################################################
  #                                                                                                  #
  #                                     exist_edge                                                   #
  #                                                                                                  #
  #   Ueberprueft, ob es bereits eine Kante von 'node1' nach 'node2' gibt.                           #
  #                                                                                                  #
  ####################################################################################################*/

static int exist_edge(node1,node2)
  Snode node1, node2;

  {
   int flag = 0;
   Sedge edge;
   for_sourcelist(node1,edge)
     {
      if ((edge->snode == node2) || (edge->tnode == node2))
        {
         flag = 1;
         break;
        }
      else ;
     }
   end_for_sourcelist(node1,edge);
   return flag;
  }

/*####################################################################################################
  #                                                                                                  #
  #                         make_nodes_and_edges_of_dualgraph                                        #
  #                                                                                                  #
  # 'make_nodes_and_edges_of_dualgraph' ist das Kernstueck des zweiten Teils des Algorithmus.        #
  # Sie wird von 'start_make_nodes_and_edges_of_dualgraph' aus aufgerufen.                           #
  # Folgende Prozeduren werden in 'make_nodes_and_edges_of_dualgraph' benutzt :                      #
  #      'exist_edge'          'make_nodes_and_edges_of_dualgraph'                                   #
  # Vorgehensweise :                                                                                 #
  #   Die aktuelle Flaeche wird als besucht markiert,danach werden rekursiv alle begrenzenden Kanten #
  #   (-> planeedges) entfernt und dabei jeweils die Flaeche auf der anderen Seite ('nextplane')     #
  #   untersucht.                                                                                    #
  #   a.) plane = nextplane :                                                                        #
  #       => naechste Kante untersuchen                                                              #
  #   b.) plane != nextplane :                                                                       #
  #       Die begrenzende Kante wird auch bei 'nextplane' entfernt.                                  #
  #        i.) nextplane wurde bereits besucht :                                                     #
  #            Falls noch keine Kante zwischen den beiden Dualgraphknoten besteht wird eine gemacht. #
  #            naechste Kante von plane untersuchen                                                  #
  #       ii.) nextplane wurde noch nicht besucht :                                                  #
  #            Ein neuer Knoten des Dualgraphen wird gemacht, danach werden die beiden Dualgraph-    #
  #            knoten ueber eine Kante verbunden.                                                    #
  #            rekursiver Aufruf von make_nodes_and_edges_of_dualgraph(nextplane)                    #
  #            nach dessen Abarbeitung werden die restlichen Begrenzungskanten von plane entfernt    #
  #                                                                                                  #
  ####################################################################################################*/

static make_nodes_and_edges_of_dualgraph(DualGraph,plane)
  Sgraph DualGraph;
  Plane plane;

  {
   Plane nextplane;
   Snode dualnode, nextdualnode;
   Sedge edge;
   Slist edgelist;
   edgelist = plane->planeedges;
   if (edgelist == empty_slist)
     ;
   else
     {
      edge = EDGE(edgelist);
      plane->planeedges = subtract_from_slist(plane->planeedges,make_attr(ATTR_DATA,(char *)edge));
      plane->visited = 1;
      dualnode = plane->dualnode;
      if ((nextplane = LEFT(edge)) == plane)
        nextplane = RIGHT(edge);
      else ;
      set_nodeattrs(dualnode,
                    make_attr(ATTR_DATA,
                              add_to_slist((Slist)attr_data(dualnode),
                                           make_attr(ATTR_DATA,(char *)edge))));
      if (nextplane != plane)
        {
         nextplane->planeedges = subtract_from_slist(nextplane->planeedges,make_attr(ATTR_DATA,(char *)edge));
         if (nextplane->visited == 1)
           {
            nextdualnode = nextplane->dualnode;
            if (exist_edge(dualnode,nextdualnode) == 0)
              make_edge(dualnode,nextdualnode,make_attr(ATTR_DATA,(char *)edge));
            else ;
            set_nodeattrs(nextdualnode,
                          make_attr(ATTR_DATA,
                                    add_to_slist((Slist)attr_data(nextdualnode),
                                                 make_attr(ATTR_DATA,(char *)edge))));
            make_nodes_and_edges_of_dualgraph(DualGraph,plane);
           }
         else
           {
            nextdualnode = make_node(DualGraph,
                                     make_attr(ATTR_DATA,(char *)new_slist(make_attr(ATTR_DATA,(char *)edge))));
            nextplane->dualnode = nextdualnode;
            make_edge(dualnode,nextdualnode,make_attr(ATTR_DATA,(char *)edge));
            make_nodes_and_edges_of_dualgraph(DualGraph,nextplane);
            make_nodes_and_edges_of_dualgraph(DualGraph,plane);
           }
        }
      else
        make_nodes_and_edges_of_dualgraph(DualGraph,plane);
     }
  }

/*####################################################################################################
  #                                                                                                  #
  #                        start_make_nodes_and_edges_of_dualgraph                                   #
  #                                                                                                  #
  #   Initialisierungsprozedur fuer 'make_nodes_and_edges_of_dualgraph'.                             #
  #                                                                                                  #
  ####################################################################################################*/

static start_make_nodes_and_edges_of_dualgraph(DualGraph,firstplane)
  Sgraph DualGraph;
  Plane firstplane;

  {
   Snode firstdualnode = make_node(DualGraph,make_attr(ATTR_DATA,NULL));
   set_nodeattrs(firstdualnode,
                 make_attr(ATTR_DATA,
                           (char *)new_slist(make_attr(ATTR_DATA,
                                                       (char *)((Sedge)attr_data(firstplane->planeedges))))));
   firstplane->dualnode = firstdualnode;
   while (firstplane->planeedges != empty_slist)
     make_nodes_and_edges_of_dualgraph(DualGraph,firstplane);
  }

/*####################################################################################################
  #                                                                                                  #
  #                                    make_dualgraph                                                #
  #                                                                                                  #
  #    Aufrufende Prozedur von 'start_make_nodes_and_edges_of_dualgraph'.                            #
  #                                                                                                  #
  ####################################################################################################*/

static Sgraph make_dualgraph(DualGraph,oldgraph)
  Sgraph DualGraph, oldgraph;

  {
   Snode node;
   Sedge edge;
   Slist l;
   Plane plane;
   for_all_nodes(oldgraph,node)
     {
      for_slist(EDGELIS(node),l)
        {
         edge = EDGE(l);
         plane = LEFT(edge);
         if (edge->snode < edge->tnode)
           {
            if (plane->visited == 0)
              {
               start_make_nodes_and_edges_of_dualgraph(DualGraph,plane);
              }
            else ;
            plane = RIGHT(edge);
            if (plane->visited == 0)
              {
               start_make_nodes_and_edges_of_dualgraph(DualGraph,plane);
              }
            else ;
           }
         else
           {
            if (plane->visited == 0)
              {
               start_make_nodes_and_edges_of_dualgraph(DualGraph,plane);
              }
            else ;
            plane = RIGHT(edge);
            if (plane->visited == 0)
              {
               start_make_nodes_and_edges_of_dualgraph(DualGraph,plane);
              }
            else ;
           }
        }
      end_for_slist(EDGELIS(node),l);
     }
   end_for_all_nodes(oldgraph,node);
   return DualGraph;
  }

/*####################################################################################################
  #                                                                                                  #
  #                                     delete_planes                                                #
  #                                                                                                  #
  #  'delete_planes' loescht zum Schluss die Flaechen (die 'planeedges' gibt es zu diesem Zeitpunkt  #
  #  schon nicht mehr, sie werden in 'make_nodes_and_edges_of_dualgraph' abgebaut.                   #
  #                                                                                                  #
  ####################################################################################################*/

static void delete_planes (graph)
  Sgraph graph;

  {
   Snode node;
   Sedge edge;
   for_all_nodes(graph,node)
     {
      for_sourcelist(node,edge)
        {
         if (attr_data(edge) != NULL)
           {
            free(attr_data(edge));
            set_edgeattrs(edge,make_attr(ATTR_DATA,NULL));
           }
         else ;
        }
      end_for_sourcelist(node,edge);
     }
   end_for_all_nodes(graph,node);
  }

/*####################################################################################################
  #                                                                                                  #
  #                                       dual                                                       #
  #                                                                                                  #
  #                 Hauptprozedur, wird von aussen angestartet.                                      #
  #                                                                                                  #
  # Falls kein Graph existiert wird eine Meldung zurueckgegeben. Wird ein nicht-zusammenhaengender   #
  # Graph eingegeben, so gibt es zu jeder Komponente eine Aussenflaeche.                             #
  #                                                                                                  #
  # Zuerst werden eigene Knoten- und Kantenmarkierungen eingefuegt, danach Teil 1 , dann Teil 2 des  #
  # Algorithmus angestartet. Zum Schluss wird der reservierte Speicher wieder freigegeben.           #
  #                                                                                                  #
  ####################################################################################################*/

Sgraph dual (graph)
  Sgraph graph;

  {
   char *malloc();
   Sgraph DualGraph = make_graph(make_attr(ATTR_DATA,NULL));
   int V = 0, E = 0;
   Snode node;
   Slist temp,l;
   Sedge edge;
   NodeMarks nodemark;
   EdgeMarks edgemark;
   char *nodebeginning, *edgebeginning;

   for_all_nodes(graph,node)                     /* Alle messages in dieser Schleife dienen nur dazu die */
     V++;                                        /* Einbettung zur Kontrolle auszugeben.                 */
     message("Knoten %s\n",node->label);
     message("   ");
     for_slist((Slist)attr_data(node),l)
       {
        E++;
        edge = EDGE(l);
        if (edge->snode < edge->tnode)
          message("%s-%s ",edge->snode->label,edge->tnode->label);
        else
          message("%s-%s ",edge->tnode->label,edge->snode->label);
        set_edgeattrs(edge,make_attr(ATTR_DATA,NULL));
       }
     end_for_slist((Slist)attr_data(node),l);
     message("\n");
   end_for_all_nodes(graph,node);
   E = E/2;
   if (V == 0)
     message("Kein Graph vorhanden !\n");
   else
     {
      message("%d Knoten, %d Kanten\n",V,E);                 /* kann geloescht werden */

      nodebeginning = malloc((V+1)*sizeof(struct nodemarks));
      nodemark = (NodeMarks)nodebeginning;
      edgebeginning = malloc((E+1)*sizeof(struct edgemarks));
      edgemark = (EdgeMarks)edgebeginning;

      set_graphlabel(DualGraph,"Dualgraph");
      DualGraph->nodes = NULL;
      DualGraph->directed = FALSE;

      for_all_nodes(graph,node)
        temp = (Slist)attr_data(node); /* Einhaengen der NodeMarks */
        set_nodeattrs(node,make_attr(ATTR_DATA,(char *)nodemark));
        VISITED(node) = 0;
        EDGELIS(node) = temp;
        nodemark++;
        for_slist (EDGELIS(node),l)
          {
           edge = EDGE(l);
           if (attr_data(edge) == NULL)
             {     /* Anhaengen der Edgemarks */
              set_edgeattrs(edge,make_attr(ATTR_DATA,(char *)edgemark));
              PASSED(edge) = 0;
              LEFT(edge) = NULL;
              RIGHT(edge) = NULL;
              edgemark++;
             }
           else ;
          }
        end_for_slist (EDGELIS(node),l)
      end_for_all_nodes(graph,node);

      for_all_nodes(graph,node)
        if (VISITED(node) == 0) startdual(node);
      end_for_all_nodes(graph,node);

      DualGraph = make_dualgraph(DualGraph,graph);
      delete_planes(graph);
      if (DualGraph == empty_sgraph)
        node = make_node(DualGraph,make_attr(ATTR_DATA,NULL));
      else
        message_graph(DualGraph);        /* Ausgabe der Kontrollinformation des Dualgraphen   */
                                         /* (Attribute der Knoten und Kanten des Dualgraphen) */
      message("Dualgraph erzeugt\n");

      free(nodebeginning);
      free(edgebeginning);
     }
  }

/*
void init_user_menu()

  {
   add_to_user_menu("Planare Einbettung",Einbettungs_aufruf);
   add_to_user_menu("Dualgraph",Dual_aufruf);
  }
*/
