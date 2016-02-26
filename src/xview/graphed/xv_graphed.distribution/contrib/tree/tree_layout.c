/* (C) Universitaet Passau 1986-1991 */
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/rect.h>
#include "std.h"
#include "slist.h"
#include "sgraph.h"
#include "graphed.h"
#include "tree.h"


typedef struct NNode {
  struct NNode *father;		/* Zeiger auf Vaterknoten 								*/
  struct NNode *lson;		/* auf linkestes Sohn								        */
  struct NNode *rlink;		/* auf rechten Bruder 									*/
  struct NNode *llink;		/* auf linken Bruder 									*/ 
  struct NNode *leftneighbor;	/* auf linken Nachbarn 									*/
  float preliminary;		
  float modifier;
  Snode knoten;			/* Knoten im Sgraph 									*/
} *Node;


typedef struct LLevelptr {		/* Liste der linken Knoten durch alle Level 					*/
  struct NNode *prevnode;
  struct LLevelptr *nextlevel;
} *Levelptr;

#define new_node() ((Node) malloc(sizeof(struct NNode)));

#define new_levelptr() ((Levelptr) malloc(sizeof(struct LLevelptr)));



static	bool fehler;
static	int nr_of_rt_error;
static	bool down;  			/* kennzeichnet die Richtung des Baumes 			*/
static	Levelptr levelzeroptr;		/* Listenanfang	 der Liste der 	linken Nachbarn			*/
static	float xtopadjustment;		/* absolute x-Koordinate der Wurzel 				*/
static	float ytopadjustment;		/* absolute y-Koordinate der Wurzel 				*/
static	float levelseparation;		/* Abstand zwischen Vaterknoten und seinen Soehnen 		*/
static	int maxdepth;			/* max. zu bearbeitende Tiefe des Baumes			*/
static	float siblingseparation = 32.0;	/* Abstand der Knoten auf der selben Stufe (level) im Baum 	*/
static	float subtreeseparation = 48.0;	/* Abstand zweier Knoten die verschiedenen Baeumen angehoeren 	*/
static	int count;			/* Anzahl der Knoten im Graph					*/
static	int *knotenhoehe;		/* Array ueber die max. Hoehe der Knoten pro Stufe		*/
static	int i;				/* Zaehlvariable fuer Schleife					*/
static	Sgraph_proc_info eingabe;

void tree_layout_walker();


/*****************************************************************/

Node Parent(node)		/* liefert Vaterknote 		*/
Node node;
{return(node->father);}

Node Firstchild(node)		/* liefert linken Sohn 		*/
Node node;
{return(node->lson);}

Node Leftsibling(node)		/* liefert linken Bruder 	*/
Node node;
{return(node->llink);}

Node Rightsibling(node)		/* liefert rechten Bruder 	*/
Node node;
{return(node->rlink);}

float Xcoord(node)		/* liefert x-Koordinate vom Sgraph */
Node node;
{return ((float)(int)node_get(graphed_node(node->knoten),NODE_X,0));}

float Ycoord(node)		/* liefert y-Koordinate vom Sgraph */
Node node;
{return ((float)(int)node_get(graphed_node(node->knoten),NODE_Y,0));}

float Prelim(node)		/* liefert vorlaefige x-Koordinate */
Node node;
{return(node->preliminary);}

float Modifier(node)		/* liefert Modifier-Wert 	*/
Node node;
{return(node->modifier);}

Node Leftneighbor(node)		/* liefert linken Nachbarn 	*/
Node node;
{return(node->leftneighbor);}

bool Isleaf(node)		/* prueft, ob Knoten Blatt ist 	*/
Node node;
{return(node->lson == 0L);}

bool Haschild(node)		/* prueft, ob Knoten Soehne hat */
Node node;
{return(node->lson != 0L);}	

bool Hasrightsibling(node)	/* prueft, ob Knoten rechten Bruder hat */
Node node;
{return(node->rlink != 0L);}

bool Hasleftsibling(node)	/* prueft, ob Knoten linken Bruder hat */
Node node;
{return(node->llink != 0L);}



/************************************************************************************************************************/
/*
In dieser Procedure wird der Knoten node in die Liste ,auf deren Anfang levelzeroptr zeigt, an die 
Stelle level eingeschrieben. Ist diese noch leer wird ein neues Listenelement erzeugt.
*/
 
void Setprevnodeatlevel(level,node)
int level;
Node node;
{ Levelptr tempptr;  
  Levelptr newnode;
  int i;
  tempptr = levelzeroptr;
  i = 0;
  while (tempptr != 0L)	/* level i bzw. Listenendesuchen								*/
  { if (i == level)
    { tempptr->prevnode = node;
      return;
    }
    else
    { if (tempptr->nextlevel == 0L)
      { newnode = new_levelptr();	/* neues Listenelement erzeugen							*/
        newnode->prevnode = 0L;
        newnode->nextlevel = 0L;
        tempptr->nextlevel = newnode;
      }
    }
    tempptr = tempptr->nextlevel;
    ++i;
  }
  levelzeroptr = new_levelptr();
  levelzeroptr->prevnode = node;
  levelzeroptr->nextlevel = 0L;
}

/***********************************************************************************************************************/

void Initprevnodelist()
{ Levelptr tempptr;
  tempptr = levelzeroptr;
  while (tempptr != 0L)
  { tempptr->prevnode = 0L;
    tempptr = tempptr->nextlevel;
  }
}

/***********************************************************************************************************************/
/*
Die mit Setprevnodeatlevel eingetragene Knoten werden hier wieder ausgelesen.
*/

Node Getprevnodeatlevel(level)
int level;
{ Levelptr tempptr;
  int i;
  tempptr = levelzeroptr;
  i = 0;
  while (tempptr != 0L)
  { if (i == level)
    {return(tempptr->prevnode);
    }
    tempptr = tempptr->nextlevel;
    ++i;
  }
  return(0L);
}

/***********************************************************************************************************************/
/*
Meannodesize addiert die Haelfte der Breite des rechten Knotens und die Haelfte der Breite des linken
Knotens. Somit ist es moeglich mit Knoten unterschiedlicher Breite zu arbeiten.
*/

float Meannodesize(leftnode, rightnode)
Node leftnode;
Node rightnode;
{ float nodesize;
  nodesize = 0.0;
  if (leftnode != 0L)
  { nodesize = (float)(int)node_get(graphed_node(leftnode->knoten),NODE_WIDTH)/2.0;
  }
  if (rightnode != 0L)
  { nodesize = nodesize + (float)(int)node_get(graphed_node(rightnode->knoten),NODE_WIDTH)/2.0;
  }
  return (nodesize);
}

/************************************************************************************************************************/
/*
Getleftmost berechnet den linkesten Knoten im Baum mit Wurzel node in der Tiefe depth.	
depth entspricht der Tiefe des Baumes node und nicht der des Orginalbaumes.
*/

Node Getleftmost(node, level, depth)
Node node;
int level;
int depth;
{ Node rightmost;
  Node leftmost;
  if (level >= depth)	/* gewuenschte Tiefe erreicht		 							*/
  { return (node);
  }
  else
  { if (Isleaf(node))	/*  Wenn node Blatt ist, so ist der Baum nicht tief genug. 					*/
    { return(0L);
    }
    else
		/* Rekursives Suchen nach dem linkesten Knoten zuerst beim linken Sohn und dann				*/
		/* bei dessen Bruedern von links nach rechts.								*/
    { rightmost = Firstchild(node);
      leftmost  = Getleftmost(rightmost,level+1,depth);
      while ((leftmost == 0L) && (Hasrightsibling(rightmost)))
      { rightmost = Rightsibling(rightmost);
        leftmost = Getleftmost(rightmost,level+1,depth);
      }
      return(leftmost);
    }
  }
}

/***********************************************************************************************************************/
/*
Diese Procedure berechnet Ueberlappungen zweier Baeume und schiebt diese soweit auseinander, dass
keine Ueberschneidungen mehr vorhanden sind. Werden also solche Veraenderungen vorgenommen, so
entstehen zwischen den Baeumen Hohlraeme, in welchen sich kleine Teilbaeume befinden koennen. Deren
Koordinaten werden dann so verschoben, dass die Soehne alle den gleichen Abstand haben.
*/

void Apportion(node, level)
Node node;
int level;
{ Node leftmost;	/* linkester Knoten im betrachteten Teilbaum							*/
  Node neighbor;	/* linker Nachbar 										*/
  int comparedepth;	/* Anzahl wieviel Baumtiefen betrachtet werden							*/
  int depthtostop;	/* groesste Baumtiefe, bis zu der der Baum durchsucht wird					*/
  float leftmodsum;	/* Summe der Modifierwerte im linken Baum							*/
  float rightmodsum;	/* Summe der Modifierwerte im rechten Baum							*/
  Node ancestorleftmost;	/* Schleifenvariable fuer leftmost							*/		
  Node ancestorneighbor;	/* Schleifenvariable fuer rightmost							*/
  float movedistance;		/* Wert um wieviel zwei Baeume ausainadergeschoben werden muessen			*/
  Node tempptr;			/* Hilfszeiger										*/
  int leftsiblings; 		/* Anzahl der linken Brueder eines Knotens						*/
  float portion;		/* Wert um wieviel ein Knoten von seinem Bruder noch auseinandergeschoben 		*/
				/* werden muss.			 							*/
  int i;

  leftmost = Firstchild(node);	
  neighbor = Leftneighbor(leftmost);	/* = linker Nachbar(im linken Baum!) des ersten Sohns				*/
  comparedepth = 1; 
  depthtostop = maxdepth - level;
  while ((leftmost != 0L) && (neighbor != 0L) && (comparedepth<=depthtostop))
		/* solange in einer Tiefe des Baumes noch zwei Teilbaeume vorhanden sind, die sich noch			*/
		/* behindern koennen.											*/
  { leftmodsum = 0.0;
    rightmodsum = 0.0;
    ancestorleftmost = leftmost;
    ancestorneighbor = neighbor;
		/* Schleife sucht ausgehend von ancestorleftmost und ancesterneighbor gemeinsame			*/
		/* Brueder. Dabei wird immer nur bis comparedepth nach oben gesucht.					*/
    for (i = 1 ; i <= comparedepth; ++i)
    { ancestorleftmost = Parent(ancestorleftmost);
      ancestorneighbor = Parent(ancestorneighbor);
      rightmodsum = rightmodsum + Modifier(ancestorleftmost);	/* Summieren der Modifierwerte 				*/
      leftmodsum = leftmodsum + Modifier(ancestorneighbor);
    };
    movedistance = Prelim(neighbor) + leftmodsum + subtreeseparation + Meannodesize(leftmost,neighbor)
		   - (Prelim(leftmost) + rightmodsum);
		/* movedistance enthaelt den Wert um den die Baeume verschoben werden muessen				*/
		/* Ist movedistance negativ so gibt es keine Kollisionen						*/
    if (movedistance > 0.0)
    { tempptr = node;
      leftsiblings = 0;
		/* In folgender Schleife wird die Zahl der Brueder berechnet, die zwischen dem Knoten 			*/
		/* node und dem Knoten ancesterneighbor liegen. Dabei bezeichnet ancesterneighbor den 			*/
		/* Knoten dessen darunterhaengender Baum sich mit dem Baum von node ueberschneidet.			*/
      while((tempptr != 0L) && ( tempptr != ancestorneighbor))
      { ++leftsiblings;
        tempptr = Leftsibling(tempptr);
      }
      if (tempptr != 0L)
      { portion = movedistance/leftsiblings;
        tempptr = node;
		/* Verschieben der oben gezaehlten Knoten um den entsprechenden Teil des Verschiebungs-			*/
		/* wertes movedistance. 										*/
        while (tempptr != ancestorneighbor) /* im Orginal == 								*/
        { tempptr->preliminary = Prelim(tempptr) + movedistance;
          tempptr->modifier = Modifier(tempptr) + movedistance;
          movedistance = movedistance - portion;
          tempptr = Leftsibling(tempptr);
        }
      } /* if 														*/
      else
      { return;
      }
    } /* if (movedistance .. 												*/
		/* Hier wird der naechste Schleifendurchlauf vorbereitet. Comparedepth wird um eins 			*/
		/* erhoeht. Neue leftmost und neighbor Werte werden errechnet. leftmost ist nun der			*/
		/* linkeste Knoten im Baum in der neuen Tiefe comparedepth und neighbor sein linker			*/
		/* Nachbar im Baum daneben (falls vorhanden).								*/
    ++comparedepth;
    if (Isleaf(leftmost))
    { leftmost = Getleftmost(node,0,comparedepth);
    }
    else
    { leftmost = Firstchild(leftmost);
    };
    if (leftmost == 0L)
    { neighbor = 0L;
    }
    else
    { neighbor = Leftneighbor(leftmost); /* nicht im Orginal Zeile eingefuegt 						*/
    }
  } /*while 														*/
}

/************************************************************************************************************************/
/*
In der Procedure Secondwalk werden die endgueltigen x und y Koordinaten berechnet, indem seine
vorlaeufige (preliminary) x-Koordinate und die Summe der Verschiebungswerte (modifier) aller
Nachkommen addiert werden. 
*/

bool Secondwalk(node, level, modsum)
Node node;
int level;
float modsum;
{ float xtemp;
  float ytemp;
  bool result;
  result = true;
  if (level <= maxdepth)
  { xtemp = xtopadjustment + Prelim(node) + modsum;	/* Koordinaten berechnen und					*/
    ytemp = ytopadjustment + knotenhoehe[level];	/* vertikaler Abstand der Knoten ist abhaengig von der Knoten-	*/
							/* groesse.							*/
    node->knoten->x = xtemp;				/* in Graph aintragen						*/
    node->knoten->y = ytemp;
    if (Haschild(node))	/* Rekursiver Aufruf von Secondwalk mit erstem Sohn; im Baum geht man 				*/
			/* eine Stufe tiefer (level+1); Aufsummieren der Modifier-Werte					*/
    { result = Secondwalk(Firstchild(node), level + 1, modsum + Modifier(node));
    }
    if (result && Hasrightsibling(node))
			/* rekursiver Aufruf mit den Bruedern, Baumtiefe bleibt gleich					*/
    { result = Secondwalk(Rightsibling(node), level, modsum);  /* im Orginal level +1 					*/
    }
  }
  else
  {result = true;
  }
  return (result);
}

/************************************************************************************************************************/
/* 
In der Procedure Firstwalk wird eine vorlaefige x-Koordianate berechnet, die unter preliminary
abgespeichert wird und Variable modifier die bei inneren Knoten benuetzt wird um ein Verschieben 
des unter dem Knoten haengenden Baumes zu ermoeglichen.
*/

void Firstwalk(node,level)
Node node;
int level;
{ Node leftmost;	/* linkester Sohn 										*/
  Node rightmost;	/* rechtester Sohn										*/
  float midpoint;	/* Halber Abstand zwischen linkesten und rechtesten Sohn also relative				*/
			/* Koordinate des Vaters ueber den Soehnen.							*/
  node->leftneighbor = Getprevnodeatlevel(level);	/* linken Nachbarn holen					*/
  Setprevnodeatlevel(level,node);
  node->modifier = 0L;
  
  /* Modified MH 5/10/91 */
/*if (knotenhoehe[level] < 2*(int)node_get(graphed_node(node->knoten),NODE_HEIGHT,0))	/* in knotenhoehe[level] wird jeweils	*/
/*										/* die Hoehe des hoechsten Knotens der	*/
/*									/* jeweiligen Stufe des Graphs eingetragen.	*/
/*{ knotenhoehe[level] = 2*(int)node_get(graphed_node(node->knoten),NODE_HEIGHT,0); */
/*}; */

  /* Modified MH 5/10/91 */
  if (knotenhoehe[level] < (int)node_get(graphed_node(node->knoten),NODE_HEIGHT,0))	/* in knotenhoehe[level] wird jeweils	*/
										/* die Hoehe des hoechsten Knotens der	*/
									/* jeweiligen Stufe des Graphs eingetragen.	*/
  { knotenhoehe[level] = (int)node_get(graphed_node(node->knoten),NODE_HEIGHT,0);
  };
  if (Isleaf(node) || (level == maxdepth))	/* ganz unten am Baum angekommen					*/
  { if (Hasleftsibling(node))
    { node->preliminary = Prelim(Leftsibling(node)) + siblingseparation + Meannodesize(Leftsibling(node),node);
		/* Die vorlaeufige x-Koordinate wird aus der vorlaeufigen x-Koordinate des linkesten			*/
		/* Sohnes, dem geforderten Abstand der Soehne und dem Durchmesser der Soehe berechnet.			*/
    }
    else
    { node->preliminary = 0.0;
    }
  }
  else		/* Knoten ist kein Blatt d. h. rekursiven Aufruf der Procedure fuer jeden der Soehne. 			*/
  { leftmost = rightmost = Firstchild(node);
    Firstwalk(leftmost, level +1);	/* fuer den ersten Sohn								*/
    while (Hasrightsibling(rightmost))	/* fuer jeden Bruder								*/
    { rightmost = Rightsibling(rightmost);
      Firstwalk(rightmost, level +1);
    }
    midpoint = (Prelim(leftmost) + Prelim(rightmost))/2;	/* Mitte ausrechnen					*/
    if (Hasleftsibling(node))
    { node ->preliminary = Prelim(Leftsibling(node)) + siblingseparation + Meannodesize(Leftsibling(node),node);
		/* vorlaeufige x-Koordinate wie oben. 									*/
      node->modifier = Prelim(node) - midpoint;
    Apportion(node,level);	/* Ueberlappungen von Teilbaeumen ueberpruefen. 					*/
    }
    else
    { node->preliminary = midpoint;
    }
  } /* else 														*/
}

/************************************************************************************************************************/
/*
Die Funktion Positiontree steuert die Berechnung der neuen Koordinaten. Es wird die Liste der 
linken Nachbarn initialisiert, anschliessend in Firstwalk die relativen Koordinaten berechnet,
dann die x und y Koordinaten der Wurzel unter xtopadjustment und ytopadjustment abgelegt, sowie
am Ende die Funktion Secondwalk aufgerufen, die dann die endgueltigen Koordinaten berechnet. 
*/

bool Positiontree(node)
Node node;
{ int i;
  int merk1;
  int merk2;
  if (node != 0L)
  { Initprevnodelist();
    Firstwalk(node,0);
    xtopadjustment = Xcoord(node) - Prelim(node);
    ytopadjustment = Ycoord(node);
    merk1 = knotenhoehe[0];
    knotenhoehe[0] = 0;
    i = 1;		/* In folgender while - Schleife werden die y-Koordinaten berechnet. Da Knoten unterschiedlich	*/
			/* gross sein koennen, wird zwischen zwei Stufen des Baums der doppelte Abstand der Summe die	*/
			/* sich aus der Haelfte der Hoehe der groessten Knoten zweier aufeinanderfolgender Stufen ergibt*/
			/* freigelassen. 										*/
    while (knotenhoehe[i] != 0)
    { merk2 = knotenhoehe[i];
/* Orig. Code
      knotenhoehe[i] = 0.5 * merk1 + 0.5 * merk2 + knotenhoehe[i-1];
*/
      knotenhoehe[i] = 0.5 * (merk1 + merk2 + levelseparation) + knotenhoehe[i-1];
      merk1 = merk2;
      ++i;
    };
    return(Secondwalk(node,0,0));
  }
  else
  {return(1);
  }
}

/*************************************************************************************************************************/
/*************************************************************************************************************************/

int size_slist(list)
Sedge list;
{ int i;
  Sedge first;
  if (list == 0L)
  { return(0);
  }
  else
  { first = list;
    i = 1;
    list = list->ssuc;
    while (list != first)
    { ++i;
      list = list->ssuc;
    };
    return(i);
  }
}
  
/*************************************************************************************************************************/

int size_tlist(list)
Sedge list;
{ int i;
  Sedge first;
  if (list == 0L)
  { return(0);
  }
  else
  { first = list;
    i = 1;
    list = list->tsuc;
    while (list != first)
    { ++i;
      list = list->tsuc;
    };
    return(i);
  }
}
 
/*************************************************************************************************************************/


Snode nno_input_edges (g,single)
Sgraph g;
bool *single;
/************************************************************************************************************************/
/* liefert einen Knoten aus 'g', der keine Eingangskanten besitzt und einen Wert 'single',    				*/
/* der angibt, ob dieser Knoten als Einziger im Baum diese Eigenschaft hat.                   				*/
/* Die globale Variable 'down' wird hier verbindlich belegt.                                  				*/
/************************************************************************************************************************/
{
  Snode n,no_in;
  Sedge e;
  int count;

  count = 0;
  no_in = empty_node;
  for_all_nodes (g,n)
    {
      if (n->tlist == empty_edge)
        {
          no_in = n;
          count = count + 1;
        }
    }
  end_for_all_nodes (g,n);
  
  *single = (count == 1);
  if (*single)
    down = true;

  return (no_in);
}




Snode nno_output_edges (g,single)
Sgraph g;
bool *single;
/************************************************************************************************************************/
/* Analog zu 'nno_input_edges ()'                                                              				*/
/************************************************************************************************************************/
{
  Snode n,no_out;
  Sedge e;
  int count;

  count = 0;
  no_out = empty_node;
  for_all_nodes (g,n)
    {
      if (n->slist == empty_edge)
        {
          no_out = n;
          count = count + 1;
        }
    }
  end_for_all_nodes (g,n);
  
  *single = (count == 1);
  if (*single)
    down = false;

  return (no_out);
}




Snode rroot_of_graph (g)
Sgraph g;
/************************************************************************************************************************/
/* Liefert die Wurzel eines Graphen 'g'. Eine Wurzel sei dabei definiert als der einzige Kno- 				*/
/* im Graph, der entweder keine Eingangskanten besitzt oder keine Ausgangskanten besitzt.     				*/
/* Existiert kein solcher Knoten folgt eine Belegung der Fehlervariablen. Es wird ein leerer  				*/
/* Knoten zurueckgegeben.                                                                     				*/
/************************************************************************************************************************/
{ 
  Snode help;
  bool root,single;
  if (g != 0L)
  { if (g->directed)
    {
      help = nno_input_edges (g,&single);
      root = ((help != empty_node) && single);
      if (root)
        return (help);
      else
        {
          help = nno_output_edges (g,&single);
          root = ((help != empty_node) && single);
          if (root)
            return (help);
          else
            { if (!(g->nodes == 0L))
              { fehler = true;  /* mehrere Wurzeln									*/
                nr_of_rt_error = 1;
                return (empty_node);
	      }
	      else /* leerer Baum											*/
	      { fehler = true;
                nr_of_rt_error = 999;
                return (empty_node);
              }
            }
        }
    }
    else
    {
      fehler = true;  /* ungerichtet											*/
      nr_of_rt_error = 3;
      return (empty_node);
    }
  }
  else  /* kein Graph  */
  { fehler = true;
    nr_of_rt_error = 999;
    return(empty_node);
  }
}




bool iis_leaf (t)
Node t;
/************************************************************************************************************************/
/* Testet, ob ein Knoten t ein Blatt ist. Dies ist der Fall, wenn die Richtung des Baumes von 				*/
/* der Wurzel weggeht und  t keine Ausgangskanten besitzt, oder die Richtung des Baumes auf   				*/
/* die Wurzel zulaeuft und t keine Eingangskanten besitzt.                                    				*/
/************************************************************************************************************************/
{
  if (t != 0L)
    {
      if (down)
        {
          return (t->knoten->slist == empty_edge);
        }
      else
        {
          return (t->knoten->tlist == empty_edge);
        }
    }
  else
    {
      return (false);
    }
}




void sortsons (t)
Node t;
/************************************************************************************************************************/
/* Nimmt einen Vaterknoten und haengt an ihn die Liste mit seinen Soehnen, die nach der       				*/
/* x-Koordinate vom graph_ed aufsteigend geordnet sind.					      				*/
/************************************************************************************************************************/
{Node son;              /* Sohn, der gerade bearbeitet wird			      					*/
 Node sonlist;	      /* aktuelle Position in der Liste der Soehne		      					*/
 Node start_sonlist;    /* zeigt immer auf den Anfang der Liste der geordneten Soehne   				*/
 Snode      node;	      /* Sohn im Sgraph 									*/
 Sedge      edge;	      /* Kante im Sgraph 									*/
 bool	    fertig;	   /* ist true, wenn akt. Sohn in dei Liste der Soehne einsortiert ist. 			*/

	/* Es ist nur ein Sohn vorhanden . Dieser wird der Variable node zugewiesen. 					*/
 if (((size_slist(t->knoten->slist) == 1)  && down) || ((size_tlist(t->knoten->tlist) == 1) && !down)) 
  { if (down)
    { node = t->knoten->slist->tnode;
    }
    else
    { node = t->knoten->tlist->snode;
    }
    t->lson = new_node();  /* Knoten initialisieren.									*/
    t->lson->father = t;
    t->lson->knoten = node;
    t->lson->lson = t->lson->rlink = t->lson->llink = t->lson->leftneighbor = 0L;
    t->lson->preliminary = t->lson->modifier = 0.0;
  }
  else		/* Es sind mindestens zwei Sohne vorhanden. 								*/
  { start_sonlist = 0L; /* start_sonlist markiert immer den Anfang der Liste der bereits				*/
			/* geordneten Sohne von t.									*/
    if ((down) && (size_slist(t->knoten->slist) > 1)) 	/* Liste wird auf >1 abgefragt,				*/
								/* da sie noch 0 sein kann.				*/
    { for_sourcelist (t->knoten,edge);  /* Kanten, die vom aktuellen Knoten weggehen 					*/
      { node = edge->tnode;   /* neuer Knoten 										*/
	son = new_node()
        son->knoten = node;	/* Initialisierung 									*/
        son->lson = son->rlink = son->llink = son->leftneighbor = 0L;
	son->preliminary = son->modifier = 0.0;
	son->father = t;
	/* aktuellen Knoten in die Liste der bereits vorhandenen Soehne einsortieren 					*/
	if (start_sonlist == 0L) /* Beim ersten Durchlauf ist start_sonlist noch leer, somit 				*/
				 /* entspricht aktueller Knoten der geordneten Liste der Soehne.			*/
	{ start_sonlist = son;
	}
	else			/* Es wurden bereits Soehne geordntet. Somit muss der aktuelle				*/
				/* Sohn in die Liste der georneten Soehne einsortiert werden.				*/
	{ if (start_sonlist->knoten->x >=son->knoten->x)
  	  { son->rlink = start_sonlist;		/* X-Koordinate von erstem Listenelement ist groesser			*/
						/* als die x-Koordinate des aktuellen Sohns. Dieser			*/
						/* wird deshalb an den Anfang der Liste gehaengt. 			*/
	    son->rlink->llink = son;
	    start_sonlist = son;
	  }
	  else	/* Aktueller Sohn wird in die Liste einsortiert. Diese wird Element fuer Element			*/
		/* durchlaufen. Das gerade betrachtete Element (eigentlich erstes Element in der ver-			*/
		/* bleibenden Liste) wird unter sunlist abgespeichert.		*/
	  { sonlist = start_sonlist;
	    fertig = false;	/* fertig wird auf true gesetzt, wenn Sohn einsortiert wurde, um 			*/
				/* Schleife vorzeitig abbrechen zu koennen.	*/
	    while ((sonlist != 0L) && !fertig)
	    { if (sonlist->rlink == 0L)	/* Listenende wurde erreicht. Sohn wird an Listenende gehaengt.			*/
	      { sonlist->rlink = son;
	        son->llink = sonlist;
		fertig = true;
	      }
	      else
	      { if (sonlist->rlink->knoten->x >= son->knoten->x)
	        { son->rlink = sonlist->rlink; /* Hier wird Sohn in die Liste eingebaut.			       */
	          son->rlink->llink = son;
	          sonlist->rlink = son;
	          sonlist->rlink->llink = sonlist;
		  fertig = true;
	        }
	        else 	    
	        { sonlist = sonlist->rlink;	/* Naechstes Listenelement wird betrachtet. 				*/
	        }
	      } /* if 													*/
	    } /* while 													*/
	  } /* else 													*/
        } /* else 													*/
      } /* for_slist 													*/
      end_for_sourcelist(t->knoten,edge);
      t->lson = start_sonlist;
    } /*if (down) 													*/
    else  /* Das Ganze nochmal fuer den Fall, dass der Graph andersherum gerichtet ist. 				*/
    { if (size_tlist (t->knoten->tlist) > 0)
      { for_targetlist (t->knoten,edge);  /* Kanten, die vom aktuellen Knoten weggehen 					*/
        { node = edge->snode;   /* neuer Knoten 									*/
	  son = new_node();
          son->knoten = node;
          son->lson = son->rlink = son->llink = son->leftneighbor = 0L;
  	  son->preliminary = son->modifier = 0.0;
	  son->father = t;
	  /* aktuellen Knoten in die Liste der bereits vorhandenen Soehne einsortieren 					*/
	  if (start_sonlist == 0L)  
	  { start_sonlist = son;
	  }
	  else
	  { if (start_sonlist->knoten->x >=son->knoten->x)
	    { son->rlink = start_sonlist;
	      son->rlink->llink = son;
	      start_sonlist = son;
	    }  
	    else
 	    { sonlist = start_sonlist;
	      fertig = false;
	      while ((sonlist != 0L) && !fertig)
	      { if (sonlist->rlink == 0L)
	        { sonlist->rlink = son;
	          son->llink = sonlist;
		  fertig = true;
	        }
	        else
	        { if (sonlist->rlink->knoten->x >= son->knoten->x)
	          { son->rlink = sonlist->rlink;
	            son->rlink->llink = son;
	            sonlist->rlink = son;
	            sonlist->rlink->llink = sonlist;
		    fertig = true;
		  }
	          else 	    
	          { sonlist = sonlist->rlink;
	          }
                } /* if 												*/
              } /* while 												*/
	    } /* else 													*/
          } /* else 													*/
        }; /* for_slist 												*/
        end_for_targetlist(t->knoten,edge);
	t->lson = start_sonlist;	
      } /*if (size_of) 													*/
    } /*if (down) 													*/
  } /* else mehr als ein Sohn 												*/
}
 	    	







void calc_soehne (t)
Node t;
/************************************************************************************************************************/
/* Berechnet einen knoten t und dann rekursiv seine Soehne.                                   				*/
/************************************************************************************************************************/
{ Node helptree;
  if (t!= 0L)
  { if (iis_leaf(t)) 	/* Wenn t Blatt ist, dann hat t keine Soehne							*/
    { t->lson = 0L;
    }
    else
    { sortsons(t);	/* t ist kein Blatt. Die Soehne werden nach ihren x-Koordinaten geordnet.	         	*/
      helptree = t->lson;	/* helptree wird erster Sohn zugewiesen. 						*/
      while (helptree != 0L)	/* Rekursives Ordnen der Soehne von den Soehnen						*/
      { calc_soehne(helptree);
        helptree = helptree->rlink;
      }
    }
  }
}

 

Node berechne_baum (g)
Sgraph g;
/************************************************************************************************************************/
/* Liefert einen Baum aus dem Graphen 'g'.                                                    				*/
/************************************************************************************************************************/
{ Node t;
  t = new_node();
  t->knoten = rroot_of_graph(g);
  if (!fehler)
  { t->father = t->lson = t->rlink = t->llink = t->leftneighbor = 0L;
    t->preliminary = t->modifier = 0.0;
    calc_soehne(t);
  }
  else 
  { t = 0L;
  }
  return(t);
}

/************************************************************************************************************************/
void markieren(baum)
Sgraph baum;
{ Snode knoten;
  count = 0;
  for_all_nodes(baum,knoten)
  { set_nodeattrs(knoten,make_attr(ATTR_FLAGS,0));
    ++count; 		/* Anzahl der Knoten zaehlen									*/

 
  }
  end_for_all_nodes(baum,knoten);
}

/************************************************************************************************************************/

bool zyklus(baum)
Snode baum;

{ Sedge edge;
  bool fehler1;
  if (attr_flags(baum) == 1)
  { nr_of_rt_error = 2;
    return(true);
  }
  else
  { if ((down && (baum->slist == empty_edge)) || (!down && (baum->tlist == empty_edge)))  /* baum ist Blatt */
    { set_nodeattrs(baum,make_attr(ATTR_FLAGS,1));
      return(false);
    }
    else
    { set_nodeattrs(baum,make_attr(ATTR_FLAGS,1));
      fehler1 = false;
      if (down)
      { for_sourcelist(baum,edge)
        { if (attr_flags(edge->tnode) == 1)
	  { nr_of_rt_error = 2;
            return(true);
    	  }
	  else
   	  { fehler1 = fehler1 || zyklus(edge->tnode);
	  }
        }
        end_for_sourcelist(baum,edge);
        return(fehler1);
      }
      else
      { for_targetlist(baum,edge)
        { if (attr_flags(edge->snode) == 1)
       	  { nr_of_rt_error = 2;
            return(true);
	  }
	  else
	  { fehler1 = fehler1 || zyklus(edge->snode);
	  }
        }
        end_for_targetlist(baum,edge);
        return(fehler1);
      }
    }
  }
}

/* Orig. Code
int last_x(edge)
Edgeline edge;
{ int merk;
  Edgeline el;
  for_edgeline(edge,el)
  { merk = edgeline_x(el);
  }
  end_for_edgeline(edge,el);
  return(merk);
}

int last_y(edge)
Edgeline edge;
{ int merk;
  Edgeline el;
  for_edgeline(edge,el)
  { merk = edgeline_y(el);
  }
  end_for_edgeline(edge,el);
  return(merk);
}
*/


int	last_x(edge)
Sedge	edge;
{
	int merk;
	Edgeline line, el;
	
	line = (Edgeline) edge_get (graphed_edge(edge), EDGE_LINE);
	for_edgeline(line,el) {
		merk = edgeline_x(el);
	} end_for_edgeline(line,el);
	return(merk);
}

int	last_y(edge)
Sedge	edge;
{
	int merk;
	Edgeline line, el;

	line = (Edgeline) edge_get (graphed_edge(edge), EDGE_LINE);
	for_edgeline(line,el) {
		merk = edgeline_y(el);
	} end_for_edgeline(line,el);
	return(merk);
}

/***********************************************************************************************************************/
/*
In ausloeschen wird der Baum t rekursiv abgebaut.
*/

void ausloeschen(t)
Node t;
{ Sedge edge;
  if (!Isleaf(t))
  { ausloeschen(Firstchild(t)); 		/* Linken Sohn loeschen							*/
  };
  if (Hasrightsibling(t))
  { ausloeschen(Rightsibling(t));

  };

    t->leftneighbor = 0L;
    t->llink = 0L;
    t->rlink = 0L;
    t->father = 0L;
  
    for_sourcelist(t->knoten,edge)
    { edge_set(graphed_edge(edge),ONLY_SET,EDGE_LINE,add_to_edgeline(new_edgeline(edge->tnode->x,edge->tnode->y),last_x(edge),last_y(edge)),0);
    }
    end_for_sourcelist(t->knoten,edge);
 
    for_targetlist(t->knoten,edge)
    {  edge_set(graphed_edge(edge),ONLY_SET,EDGE_LINE,add_to_edgeline(new_edgeline(edge->snode->x,edge->snode->y),last_x(edge),last_y(edge)),0);
    }
    end_for_targetlist(t->knoten,edge);


    t = 0L;
}

/************************************************************************************************************************/

void fehlerbehandlung()
  				/* Fehlerbehandlung 							*/
{ switch(nr_of_rt_error)
      { case 1:  error("There must exactly be one root\n");return;
        case 2:  error("The graph contains a cycle and is not a tree\n");return;
        case 3:  error("The graph must be directed\n");return;
        default: ;
      } /* case 													*/
} 

/************************************************************************************************************************/

void berechne_procedure()

{ Node t;
  knotenhoehe = (int *)malloc((count+1) * sizeof(int));
  for (i = 0; i <= count; ++i)	/* Array mit 0 initialisieren							*/
  { knotenhoehe[i] = 0;
  };
  t =  berechne_baum(eingabe->sgraph);   /* Uebertragen des Baumes aus der Sgraph Daten- 				*/
   	 			            /* struktur, in die Node Struktur.          				*/
  fehler = !Positiontree(t);	    /* Neue Koordinaten werden berechnet.					*/
  ausloeschen(t);			    /* Neu erzeugter Baum wird wieder abgebaut.		        	*/
  free(knotenhoehe);			/* Speicherplatz freigeben						*/
  fehlerbehandlung(); 
}


/************************************************************************************************************************/
/* 
Im Hauptprogramm werden die Konstanten siblingseparation, subtreeseparation und
levelseparation belegt, sowie der gasamte Programmablauf gesteuert. 
siblingseparation entspricht dem Abstand der Knoten auf der selben Stufe (level) im Baum.
subtreeseparation entspricht dem Abstand zweier Knoten die verschiedenen Baeumen angehoeren.
*/

void tree_layout_walker(info)
Sgraph_proc_info info;

{ Node t;
  Snode wurzel;
  fehler = false;
  nr_of_rt_error = 0;

  siblingseparation = tree_settings.siblingseparation;
  subtreeseparation = tree_settings.subtreeseparation;
  levelseparation   = tree_settings.vertical_separation;
  
  if (info != 0L)
  { eingabe = info;
    wurzel = rroot_of_graph(info->sgraph);	/* Die Wurzel des Baumes wird gesucht					*/
    if (wurzel != 0L)
    { markieren(wurzel->graph);			/* Die Knoten des Baums werden markiert fuer die anschliessende		*/
   
      fehler = fehler || zyklus(wurzel);		/* Suche nach Zyklen im Kreis.					*/
    }
    if (!fehler)
    { maxdepth = 10000;  /* = unendlich 										*/
      berechne_procedure();
    }
    else
    { fehlerbehandlung();
    }
  }
  info->recompute = TRUE;
}



