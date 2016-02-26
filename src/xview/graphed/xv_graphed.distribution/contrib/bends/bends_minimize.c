/* (C) Universitaet Passau 1986-1991 */

/*  Hauptprogramm der Diplomarbeit  */


#include <std.h>
#include <string.h>
#include <slist.h>
#include <sgraph.h>
#include <graphed.h>
#include "bends.h"

#define maxcap 1000000   /*  Eigentlich unendlich  */
#define maxcost 1000000   /*  Eigentlich unendlich  */
#define max1 100
#define max2 100


typedef struct tknickliste
  {
     int xpos,ypos;
     struct tknickliste *next;
  }  TKNICKLISTE;

typedef struct tknotenmenge
    {
       int nummer;
       int edname;
       char name[10];
       int grad;
       int echt;
       int teil;
       int xpos,ypos;
       int merkx,merky;
       struct tknotenmenge *next;
     }  TKNOTENMENGE;

typedef struct tkantenmenge
    {
       int nummer;
       char name[10];
       int anfang;
       int ende;
       int edanfang;
       int edende;
       int schnitt;
       int sbeh,tbeh,behsknoten;
       TKNICKLISTE *knicke;
       struct tkantenmenge *next;
     } TKANTENMENGE;

typedef struct thilfskantenmenge
    {
       int name;
       int herkunft;
       struct thilfskantenmenge *next;
    } THILFSKANTENMENGE;

typedef struct tfacemenge
    {
       int nummer;
       int grad;
       int geaendert;
       int behandelt;
       struct tfacemenge *next;
     }TFACEMENGE;


typedef struct tkantennamen
    {
       int name;
       struct tkantennamen *next;
    } TKANTENNAMEN;

typedef struct tkantentripel
   {
       int name;
       int s;
       int a,afix;
       int l;
       int dir;
       int bridge;
       int tried;
       int anfang,ende;
       int p;
       int fixed;
       struct tkantentripel *next;
   }TKANTENTRIPEL;


typedef struct tface
     {
       int nummer;
       TKANTENTRIPEL *face;
       }TFACE;

typedef struct tgraph
      {
        TFACE *flaeche;
        struct tgraph *next;
       } TGRAPH;


typedef struct tnetzknotenmenge
    {
      int nummer;  /*  Knotennummer  */
      int herkunft;   /*  Gibt an,welchem Knoten bzw. Face der Netzknoten
                          entspricht.  */
      char typ;    /*  s:quelle; t:Senke; v: v-Knoten; f: f-Knoten.  */
      int perm;    /*  permanent; schon behandelt?  */
      int cost;    /*  Kosten, um den Knoten zu erreichen  */
      int vor;     /*  Vorgaenger,von dem aus das geschieht  */
      int menge;   /*  Wieviel kann auf diesem Weg dorthin fliessen ? */
      int vorrat;   /*  Wieviel Einheiten muessen noch vom Knoten 
                        weggeschafft werden.  */
      struct tnetzknotenmenge *next;  /* Naechster Knoten der Liste */
    } TNETZKNOTENMENGE;



typedef struct tbogenmenge
    {
       char typ;    /*  v,f,s,t  */
       int anfang;
       int ende;
       int costen;
       int cap;     /*  Kapazitaet  */
       int flow;    /*  Fluss   */
       int fixed;
       int f1bogen;
       struct tbogenmenge *next;
     } TBOGENMENGE;



typedef struct tkantenliste
   {
     int nummer;   /*  intern */
     char name[10];    /*  auf Bildschirm  */
     int dest;
     int dop;
     int knick1x,knick1y,knick2x,knick2y; 
                                            /*  Koordinaten des Ausgangspunktes 
                                           und des ersten Knickes der Kante.  */
     int knick3x,knick3y,knick4x,knick4y;  /*  Koordinaten des letzten Knickes 
                                               und des Endpunktes der Kante.  */
     double fwinkel,lwinkel;
     int frichtung,lrichtung;             /*  Nummer des Quadranten  */
     int gefunden;
     int ext;
     struct tkantenliste *next;
   } TKANTENLISTE;



typedef struct tedgraph
    {
       int nummer;   /*  intern  */
       int ednr;
       char name[10];     /*  auf Bildschirm  */
       int visited;
       int kantenzahl;
       int zusammen;
       int x,y,size;
       TKANTENLISTE *kanten;
       struct tedgraph *next;
    } TEDGRAPH;



typedef struct tzahlliste
  {
     int zahl;
     struct tzahlliste *next;
  } TZAHLLISTE;






static Sgraph seingabe;
static int Einheit;

static TKNOTENMENGE *knotenmenge,*Vdachmenge;
static TKANTENMENGE *kantenmenge;
static THILFSKANTENMENGE *hilfskantenmenge;
static TFACEMENGE *facemenge;
static TGRAPH *graph,*laengengraph;
static TEDGRAPH *edgraph;
static TNETZKNOTENMENGE *netzknotenmenge;
static TBOGENMENGE *bogenmenge;
static int netzknotenanz,Flusswert,Kosten,Fixed,Fluss;
static int directed,Korrekt,Bogenzaehler;
static int knotenzahl,kantenzahl,originalkantenzahl,echtkantenzahl,facezahl;
static int linkskor,linksknoten,linksanfang,linksende,linksnummer,linkswahl;
static int Maxeinheit,Maxeinheit2,Maxlinks,Maxoben;
static int matrix[max1][max2];

char		*call_bends ();
void		call_call_bends ();
void		bends();
extern	int	wac_buffer;
extern	int	get_gridwidth();

char	*call_bends (menu, menu_item)
char	*menu, *menu_item;
{
	return call_sgraph_proc (call_call_bends);
}



void	call_call_bends (info)
Sgraph_proc_info info;
{
	int	grid = get_gridwidth (wac_buffer);

	seingabe = info->sgraph;
	if (seingabe != empty_sgraph && seingabe->nodes != seingabe->nodes->suc) {
		Einheit = bends_settings.grid;
		bends();
	}
	info->recenter = TRUE;
}

void bends()
{


void parse();
void berechne_knotenmenge();
void berechne_kantenmenge();
void halbiere_kanten();
void verdopple_kanten();
void suche_externes_face();
void berechne_winkel();
void berechne_faces();
void facegrad();
void knotengrad();
void init();
void tests();
void berechne_netzwerk();
void berechne_knoten();
void berechne_orthogonale_darstellung();
void finde_laengen();
void ausgeben();
void freigeben();

init();
parse(); 
berechne_knotenmenge();
halbiere_kanten();
berechne_kantenmenge();
verdopple_kanten();
tests();
if (Korrekt == 1)
  {
     berechne_winkel();
     suche_externes_face();
     berechne_faces();
     knotengrad();
     facegrad();
     berechne_knoten();

     berechne_netzwerk(graph);  /*  Aus dem Graphen wird das zugehoerige
                                    Netzwerk bestimmt  */
     berechne_orthogonale_darstellung();
     finde_laengen();   /*  Berechnet zu jeder Kante des orthogonalen
                            Graphen ihre Laenge. */
     ausgeben();   /*  Eine Datei fuer Graphed wird erstellt. */
     freigeben();     /*  Die belegten Speicherplaetze werden freigegeben.  */
  }  /*  Korrekt  */

}  /* end main */



void init()
/*  Setzt alle(?) globalen Variablen  */
{
int i,j;
knotenmenge = NULL;
Vdachmenge = NULL;
kantenmenge = NULL;
hilfskantenmenge = NULL;
facemenge = NULL;
graph = NULL;
edgraph = NULL;
laengengraph = NULL;
netzknotenmenge = NULL;
bogenmenge = NULL;
directed = 1;
linkskor = 0;
linksknoten = 0;
linksanfang = 0;
linksende = 0;
linksnummer = 0;
linkswahl = 0;
netzknotenanz = 0;
Flusswert = 0;
Kosten = 0;
Fixed = 0;
knotenzahl = 0;
kantenzahl = 0;
originalkantenzahl = 0;
echtkantenzahl = 0;
Korrekt = 1;
facezahl = 0;
Maxeinheit = 0;
Maxeinheit2 = 0;
Maxlinks = 0;
Maxoben = 0;
Bogenzaehler = 0;
for (i=0;i<max1;i++)
  for (j=0;j<max2;j++)
    matrix[i][j] = 0;
}  /*  init  */




int pot(basis,exp)
/*  Liefert basis**exp  */
{
int ergebnis = 1;
int h = exp;
while (h != 0)
  {
    ergebnis*=basis;
    h--;
  }  /*  while  */
return(ergebnis);
}  /* pot  */



int betrag(zahl)
/*  Liefert den Absolutbetrag von zahl */
{
int ergebnis;
if (zahl < 0)
  ergebnis = -zahl;
 else ergebnis = zahl;
return(ergebnis);
}  /* betrag */




int mache_ziffer(c)
/* Liefert den Zahlenwert von c  */
{
int ergebnis;
switch(c)
  {
   case '0': ergebnis = 0;break;
   case '1': ergebnis = 1;break;
   case '2': ergebnis = 2;break;
   case '3': ergebnis = 3;break;
   case '4': ergebnis = 4;break;
   case '5': ergebnis = 5;break;
   case '6': ergebnis = 6;break;
   case '7': ergebnis = 7;break;
   case '8': ergebnis = 8;break;
   case '9': ergebnis = 9;break;
   default :;
 }  /*  switch  */
return(ergebnis);
}  /*  mache_ziffer  */




void parse()   /* Eingabeprozedur  */
/*   Eine interne Datenstruktur wird aus den Sgraph-Daten erzeugt.  */
{
Snode sedge_real_source();
Snode sedge_real_target();
Sgraph heingabe;
Snode hknoten,knotensource,knotentarget;
Sedge hkante;
Edgeline edgeline,hedgeline;
TEDGRAPH *hedgraph,*hedgraph2;
TKANTENLISTE *hkantenliste,*hkantenliste2;
int i,zahl1x,zahl2x,zahl3x,zahl4x,zahl1y,zahl2y,zahl3y,zahl4y,gefunden;
int minx,maxx,miny,maxy,linkspruefennach,pruefnachx,prueflinksx,modus;
int pruefvorx,pruefvory,linkspruefen,pruefnachy,prueflinksy,diffvory;
int diffvorx,diffnachy,diffnachx,quotvor,quotnach,prueflinks,pruefnach;

heingabe = seingabe;
linkspruefen = 0;
linkspruefennach = 0;
minx = 1000000;
miny = 1000000;
maxx = 0;
maxy = 0;
linkskor = 100000;
linksknoten = 0;
linksanfang = 0;
linksende = 0;
edgraph = NULL;
i = 0;
for_all_nodes(heingabe,hknoten)
{
  hedgraph = (TEDGRAPH*) malloc(sizeof(TEDGRAPH));
  hedgraph->next = edgraph;
  edgraph = hedgraph;
  edgraph->visited = 0;
  edgraph->zusammen = 0;
  edgraph->x = hknoten->x;
  edgraph->y = hknoten->y;
  edgraph->kantenzahl = 0;
  edgraph->kanten = NULL;
  edgraph->nummer = hknoten->nr;
  if (hknoten->label != NULL)
    strcpy(edgraph->name,
          (char*) node_get(graphed_node(hknoten),NODE_LABEL));
   else strcpy(edgraph->name,"");
  knotenzahl++;
  edgraph->ednr = knotenzahl;
  for_sourcelist(hknoten,hkante)
    {
      hkantenliste = (TKANTENLISTE*) malloc(sizeof(TKANTENLISTE));
      hkantenliste->next = NULL;
      hkantenliste->nummer = kantenzahl + 1;
      if (hkante->label != NULL)
        strcpy(hkantenliste->name,
              (char*) edge_get(graphed_edge(hkante),EDGE_LABEL));
       else strcpy(hkantenliste->name,"");
      kantenzahl++;
      hkantenliste->dop = 0;
      hkantenliste->ext = 0;
      hkantenliste->gefunden = 0;
      hkantenliste->dest = hkante->tnode->nr;
      edgeline = (Edgeline) edge_get(graphed_edge(hkante),EDGE_LINE); 
      i = 0;
      for_edgeline(edgeline,hedgeline)
        {
          i++;
          if (i == 1)
            {
              zahl1x = hedgeline->x;
              if (zahl1x < minx)
                 minx = zahl1x;
                else if (zahl1x > maxx)
                 maxx = zahl1x;
              if (zahl1x < linkskor)
                {
                  linkskor = zahl1x;
                  linksknoten = hedgraph->nummer;
                  linksanfang = 0;
                  linksende = 0;
                }
              zahl1y = hedgeline->y;
              if (zahl1y < miny)
                 miny = zahl1y;
                else if (zahl1y > maxy)
                 maxy = zahl1y;
              zahl3x = zahl1x;
              zahl3y = zahl1y;
            }  /*  i == 1  */
           else if (i == 2)
            {
              zahl2x = hedgeline->x;
              if (zahl2x < minx)
                 minx = zahl2x;
                else if (zahl2x > maxx)
                 maxx = zahl2x;
              if (zahl2x < linkskor)
                {
                  linkskor = zahl2x;
                  linksknoten = hedgraph->nummer;
                  linksanfang = 0;
                  linksende = 0;
                }
              zahl2y = hedgeline->y;
              if (zahl2y < miny)
                 miny = zahl2y;
                else if (zahl2y > maxy)
                 maxy = zahl2y;
              zahl4x = zahl2x;
              zahl4y = zahl2y;
              if (linkspruefennach == 1)
                 pruefnachx = zahl4x;
              if (zahl4x < linkskor)
                {
                  linkskor = zahl4x;
                  linksnummer = kantenzahl;
                  prueflinksx = zahl4x;
                  pruefvorx = zahl3x;
                  pruefvory = zahl3y;
                  linkspruefen = 1;
                  linkspruefennach = 0;
                  linksanfang = hedgraph->nummer;
                  linksende = hkantenliste->dest;
                  linksknoten = 0;
                }
              if (linkspruefennach == 1)
                {
                  pruefnachy = zahl4y;
                  linkspruefennach = 0;
                }
              if (linkspruefen == 1)
                {
                  prueflinksy = zahl4y;
                  linkspruefen = 0;
                  linkspruefennach = 1;
                }
            }  /*  i == 2  */
           else  /* i > 2 */
            {
              zahl3x = zahl4x;
              zahl3y = zahl4y;
              zahl4x = hedgeline->x;
              if (zahl4x < minx)
                 minx = zahl4x;
                else if (zahl4x > maxx)
                 maxx = zahl4x;
              if (zahl4x < linkskor)
                {
                  linkskor = zahl4x;
                  linksknoten = hedgraph->nummer;
                  linksanfang = 0;
                  linksende = 0;
                }
              zahl4y = hedgeline->y;
              if (zahl4y < miny)
                 miny = zahl4y;
                else if (zahl4y > maxy)
                 maxy = zahl4y;
              if (linkspruefennach == 1)
                 pruefnachx = zahl4x;
              if (zahl4x < linkskor)
                {
                  linkskor = zahl4x;
                  linksnummer = kantenzahl;
                  prueflinksx = zahl4x;
                  pruefvorx = zahl3x;
                  pruefvory = zahl3y;
                  linkspruefen = 1;
                  linkspruefennach = 0;
                  linksanfang = hedgraph->nummer;
                  linksende = hkantenliste->dest;
                  linksknoten = 0;
                }
              if (linkspruefennach == 1)
                {
                  pruefnachy = zahl4y;
                  linkspruefennach = 0;
                }
              if (linkspruefen == 1)
                {
                  prueflinksy = zahl4y;
                  linkspruefen = 0;
                  linkspruefennach = 1;
                }
            }  /*  i > 2  */
        }
      end_for_edgeline(edgeline,hedgeline);
      linkspruefen = 0;
      linkspruefennach = 0;
      if (zahl4y == linkskor)
        {
          linksknoten = hkantenliste->dest;
          linksnummer = 0;
          linksanfang = 0;
          linksende = 0;
        }
      knotensource = sedge_real_source(hkante);
      knotentarget = sedge_real_target(hkante);
      if (knotensource == knotentarget)
        {
          hedgraph2 = edgraph;
          while (hedgraph2->nummer != knotensource->nr)
            hedgraph2 = hedgraph2->next;
          gefunden = 0;
          hkantenliste2 = hedgraph2->kanten;
          while ((gefunden == 0)&&(hkantenliste2 != NULL))
            {
               if ((hkantenliste2->knick1x == zahl1x)&&
                   (hkantenliste2->knick1y == zahl1y)&&
                   (hkantenliste2->knick2x == zahl2x)&&
                   (hkantenliste2->knick2y == zahl2y)&&
                   (hkantenliste2->knick3x == zahl3x)&&
                   (hkantenliste2->knick3y == zahl3y)&&
                   (hkantenliste2->knick4x == zahl4x)&&
                   (hkantenliste2->knick4y == zahl4y))
                 gefunden = 1;
               hkantenliste2 = hkantenliste2->next;
            }
          if (gefunden == 1)
            modus = 2;
           else modus = 1;
        }
      if ((knotensource->nr != hknoten->nr)||
         ((knotensource == knotentarget)&&(modus == 2)))
        {
          hkantenliste->knick1x = zahl4x;
          hkantenliste->knick1y = zahl4y;
          hkantenliste->knick2x = zahl3x;
          hkantenliste->knick2y = zahl3y;
          hkantenliste->knick3x = zahl2x;
          hkantenliste->knick3y = zahl2y;
          hkantenliste->knick4x = zahl1x;
          hkantenliste->knick4y = zahl1y;
        }
       else
        {
          hkantenliste->knick1x = zahl1x;
          hkantenliste->knick1y = zahl1y;
          hkantenliste->knick2x = zahl2x;
          hkantenliste->knick2y = zahl2y;
          hkantenliste->knick3x = zahl3x;
          hkantenliste->knick3y = zahl3y;
          hkantenliste->knick4x = zahl4x;
          hkantenliste->knick4y = zahl4y;
        }
      if ((hkantenliste->knick1x == hkantenliste->knick2x)&&
          (hkantenliste->knick1x == hkantenliste->knick3x)&&
          (hkantenliste->knick1x == hkantenliste->knick4x)&&
          (hkantenliste->knick1y == hkantenliste->knick2y)&&
          (hkantenliste->knick1y == hkantenliste->knick3y)&&
          (hkantenliste->knick1y == hkantenliste->knick4y))
         {
           kantenzahl--;
         }
        else
         {
           hkantenliste->next = edgraph->kanten;
           edgraph->kanten = hkantenliste;
         }
    }
  end_for_sourcelist(hknoten,hkante);
}
end_for_all_nodes(heingabe,hknoten);
if (linksnummer != 0)
  {
    diffvory = (double) (prueflinksy - pruefvory);
    if (diffvory < 0)
      diffvory = diffvory * (-1.0);
    diffvorx = (double) (pruefvorx - prueflinksx);
    if (diffvorx < 0)
      diffvorx = diffvorx * (-1.0);
    diffnachy = (double) (prueflinksy - pruefnachy);
    if (diffnachy < 0)
      diffnachy = diffnachy * (-1.0);
    diffnachx = (double) (pruefnachx - prueflinksx);
    if (diffnachx < 0)
      diffnachx = diffnachx * (-1.0);
    quotvor = diffvory/diffvorx;
    quotnach = diffnachy/diffnachx;
    if ((pruefvory < prueflinksy)&&(pruefnachy > prueflinksy))
      linkswahl = 1;
    if ((pruefvory > prueflinksy)&&(pruefnachy < prueflinksy))
      linkswahl = 0;
    if ((pruefvory < prueflinksy)&&(pruefnachy < prueflinksy)&&
        (quotvor > quotnach))
      linkswahl = 1;
    if ((pruefvory < prueflinksy)&&(pruefnachy < prueflinksy)&&
        (quotvor < quotnach))
      linkswahl = 0;
    if ((pruefvory > prueflinksy)&&(pruefnach > prueflinks)&&
        (quotvor > quotnach))
      linkswahl = 0;
    if ((pruefvory > prueflinksy)&&(pruefnachy > prueflinksy)&&
        (quotvor < quotnach))
      linkswahl = 1;
  }  /*  linksnummer != 0 */
if ((maxx - minx) > (maxy - miny))
  Maxeinheit = (maxx - minx);
 else Maxeinheit = (maxy - miny);
Maxlinks = minx;
Maxoben = miny;
}  /*  parse  */







int mache_zahl(s)  /*  Hilfsprocedur von parse  */
/*  wandelt den string s in eine Zahl um.  */
char s[10];
{
int mache_ziffer();
int pot();
int wert;
int ergebnis = 0;
int laenge = 0;
int i = 0;
while ((s[laenge] >= '0')&&(s[laenge] <= '9'))
   laenge++;
while ((s[i] >= '0')&&(s[i] <= '9'))
  {
     wert = mache_ziffer(s[i]);
     ergebnis += (wert * pot(10,laenge - (i+1)));
     i++;
  }  /*  while  */
return(ergebnis);
}  /*  mache_zahl  */


void berechne_knotenmenge()
/*  Erstellt die Knotenmenge aus den Informationen in edgraph.  */
{
TEDGRAPH *hedgraph;
TKNOTENMENGE *hknotenmenge;
hedgraph = edgraph;
while (hedgraph != NULL)
  {
    hknotenmenge = (TKNOTENMENGE *) malloc(sizeof(TKNOTENMENGE));
    hknotenmenge->nummer = hedgraph->ednr;
    hknotenmenge->edname = hedgraph->nummer;
    strcpy(hknotenmenge->name,hedgraph->name);
    hknotenmenge->grad = 0;
    hknotenmenge->echt = 1;
    hknotenmenge->xpos = -100;
    hknotenmenge->ypos = -100;
    hknotenmenge->merkx = 0;
    hknotenmenge->merky = 0;
    hknotenmenge->next = knotenmenge;
    knotenmenge = hknotenmenge;
    hedgraph = hedgraph->next;
  }  /*  while  */
}  /*  berechne_knotenmenge  */




void halbiere_kanten()
/*  In edgraph wird fuer jede Kante a -> b die zugehoerige Kante b -> a
    gestrichen, falls der Graph ungerichtet ist.  */
{
TEDGRAPH *hedgraph,*suchedgraph2;
TKANTENLISTE *hkantenliste,*suchkantenliste2,*loeschliste1,*loeschliste2;
int start,ziel,erster1,erster2,gefunden,betragx1,betragy1,betragx2,betragy2;
int betrag1,betrag2;
if (seingabe->directed == 0)
  {
    hedgraph = edgraph;
    while (hedgraph != NULL)
      {
        erster1 = 1;
        hkantenliste = hedgraph->kanten;
        while (hkantenliste != NULL)
          {
            if (hkantenliste->dop == 0)
            {
            start = hedgraph->nummer;
            ziel = hkantenliste->dest;
            suchedgraph2 = edgraph;
            suchkantenliste2 = suchedgraph2->kanten;
            erster2 = 1;
            while (suchkantenliste2 == NULL)
              {
                suchedgraph2 = suchedgraph2->next;
                suchkantenliste2 = suchedgraph2->kanten;
              }
            gefunden = 0;
            while (gefunden == 0)
              {
                if ((suchedgraph2->nummer == ziel)&&
                   (suchkantenliste2->dest == start)&&
                   (suchkantenliste2->knick1x == hkantenliste->knick4x)&&
                   (suchkantenliste2->knick1y == hkantenliste->knick4y)&&
                   (suchkantenliste2->knick2x == hkantenliste->knick3x)&&
                   (suchkantenliste2->knick2y == hkantenliste->knick3y)&&
                   (suchkantenliste2->knick3x == hkantenliste->knick2x)&&
                   (suchkantenliste2->knick3y == hkantenliste->knick2y)&&
                   (suchkantenliste2->knick4x == hkantenliste->knick1x)&&
                   (suchkantenliste2->knick4y == hkantenliste->knick1y))
                 gefunden = 1;
                else
                 if (suchkantenliste2->next != NULL)
                   {
                     loeschliste2 = suchkantenliste2;
                     suchkantenliste2 = suchkantenliste2->next;
                     erster2 = 0;
                   }
                  else
                    {
                      suchedgraph2 = suchedgraph2->next;
                      while (suchedgraph2->kanten == NULL)
                        suchedgraph2 = suchedgraph2->next;
                      suchkantenliste2 = suchedgraph2->kanten;
                      erster2 = 1;
                    }
              }  /*  gefunden == 0  */

            betragx1 = betrag(hkantenliste->knick1x - hedgraph->x);
            betragy1 = betrag(hkantenliste->knick1y - hedgraph->y);
            betragx2 = betrag(hkantenliste->knick2x - hedgraph->x);
            betragy2 = betrag(hkantenliste->knick2y - hedgraph->y);
            betrag1 = betragx1 + betragy1;
            betrag2 = betragx2 + betragy2;
            if (betrag1 < betrag2)
              {
                if (erster2 == 1)
                  suchedgraph2->kanten = suchedgraph2->kanten->next;
                 else loeschliste2->next = loeschliste2->next->next;
                hkantenliste->dop = 1;
              }
             else
              {
                if (erster1 == 1)
                  hedgraph->kanten = hedgraph->kanten->next;
                 else loeschliste1->next = loeschliste1->next->next;
                suchkantenliste2->dop = 1;
              }
            }  /*  hkantenliste->dop == 0  */
            if (loeschliste1->next != hkantenliste->next)
              loeschliste1 = hkantenliste;
            erster1 = 0;
            hkantenliste = hkantenliste->next;
          }  /*  hkantenliste  !=  NULL */
        hedgraph = hedgraph->next;
      }  /*  hedgraph != NULL */
    hedgraph = edgraph;
    while (hedgraph != NULL)
      {
        hkantenliste = hedgraph->kanten;
        while (hkantenliste != NULL)
          {
            hkantenliste->dop = 0;
            hkantenliste = hkantenliste->next;
          }
        hedgraph = hedgraph->next;
      }  /* while edgraph  != NULL */
  }  /*  directed  == 0 */
}  /*  halbiere_kanten  */




void berechne_kantenmenge()
/*  Erstellt die Kantenmenge aus den Informationen in edgraph.  */
{
int suche_ednummer();
TEDGRAPH *hedgraph;
TKANTENMENGE *hkantenmenge;
TKANTENLISTE *hkantenliste;
int knoten,anfang,ende;
hedgraph = edgraph;
while (hedgraph != NULL)
  {
    knoten = hedgraph->nummer;
    hkantenliste = hedgraph->kanten;
    while (hkantenliste != NULL)
     {
       hkantenmenge = (TKANTENMENGE*) malloc(sizeof(TKANTENMENGE));
       hkantenmenge->nummer = hkantenliste->nummer;
       strcpy(hkantenmenge->name,hkantenliste->name);
       anfang = suche_ednummer(knoten);
       hkantenmenge->anfang = anfang;
       hkantenmenge->edanfang = knoten;
       ende = suche_ednummer(hkantenliste->dest);
       hkantenmenge->ende = ende;
       hkantenmenge->edende = hkantenliste->dest;
       hkantenmenge->schnitt = 0;
       hkantenmenge->sbeh = 0;
       hkantenmenge->tbeh = 0;
       hkantenmenge->behsknoten = 0;
       hkantenmenge->knicke = NULL;
       hkantenmenge->next = kantenmenge;
       kantenmenge = hkantenmenge;
       hkantenliste = hkantenliste->next;
     }  /*  while  */
    hedgraph = hedgraph->next;
  }  /*  while  */
}  /*  berechne_kantenmenge  */






int suche_ednummer(zahl)   /*  Hilfsprocedur von berechne_kantenmenge  */
/*  Berechnet wird die ednr des ed-Knotens, der die nummer zahl
   traegt.  */
{
int ergebnis,gefunden;
TEDGRAPH *hedgraph;
hedgraph = edgraph;
gefunden = 0;
while (gefunden == 0)
  {
    if (hedgraph->nummer == zahl)
      {
          ergebnis = hedgraph->ednr;
          gefunden = 1;
      }
     else hedgraph = hedgraph->next;
  }  /*  while  */
return(ergebnis);
}  /*  suche_ednummer  */





void verdopple_kanten()
/*  In edgraph wird fuer jede Kante a -> b eine weitere Kante b -> a
    eingetragen.  */
{
void doppelkante();
TEDGRAPH *hedgraph;
TKANTENLISTE *hkantenliste;
int knoten,zaehler;
hedgraph = edgraph;
while (hedgraph != NULL)
  {
    knoten = hedgraph->nummer;
    hkantenliste = hedgraph->kanten;
    while (hkantenliste != NULL)
      {
        if (hkantenliste->dop == 0)
          {
            hkantenliste->dop = 1;
            doppelkante(knoten,hkantenliste);
          }
        hkantenliste = hkantenliste->next;
      }  /*  while  */
    hedgraph = hedgraph->next;
  }  /*  while  */
hedgraph = edgraph;
while (hedgraph != NULL)
  {
     hkantenliste = hedgraph->kanten;
     zaehler = 0;
     while (hkantenliste != NULL)
       {
          zaehler++;
          hkantenliste = hkantenliste->next;
       }  /*  while   */
     hedgraph->kantenzahl = zaehler;
     hedgraph = hedgraph->next;
  }  /*  while  */
return;
}  /*  verdopple_kanten  */




void suche_externes_face()
/*  traegt bei einer Kante ext = 1 ein.  */
{
TEDGRAPH *hedgraph;
TKANTENLISTE *hkantenliste;
int knoten;
int richtung = 0;
double winkel = 0.0;
int gefunden = 0;
hedgraph = edgraph;
if (linksknoten != 0)
{
 while (gefunden == 0)
  {
     if (hedgraph->nummer == linksknoten)
       {
          gefunden = 1;
          hkantenliste = hedgraph->kanten;
          while (hkantenliste != NULL)
            {
               if ((richtung == 0)||
                  ((richtung == 1)&&(hkantenliste->frichtung == 2))||
                  ((richtung == 2)&&(hkantenliste->fwinkel > winkel)&&
                    (hkantenliste->frichtung == 2))||
                  ((richtung == 1)&&(hkantenliste->fwinkel > winkel)))
                    {
                       winkel = hkantenliste->fwinkel;
                       richtung = hkantenliste->frichtung;
                    }
               hkantenliste = hkantenliste->next;
            }
          hkantenliste = hedgraph->kanten;
          gefunden = 0;
          while (gefunden == 0)
            {
               if ((hkantenliste->frichtung == richtung)&&
                   (hkantenliste->fwinkel == winkel))
                  {
                     gefunden = 1;
                     hkantenliste->ext = 1;
                  }
               else hkantenliste = hkantenliste->next;
            }
       }
     hedgraph = hedgraph->next;
  }  /*  while  */
 }
else   /*  linksknoten == 0   */
 {
   hkantenliste = hedgraph->kanten;
   while (gefunden == 0)
    {
       if (((hedgraph->nummer == linksanfang)&&
          (hkantenliste->dest == linksende)&&(linkswahl == 1)&&
          (hkantenliste->nummer == linksnummer))||
          ((hedgraph->nummer == linksende)&&(hkantenliste->dest == linksanfang)
          &&(hkantenliste->nummer == linksnummer)&&(linkswahl == 0)))
             {
                gefunden = 1;
                hkantenliste->ext = 1;
             }
       else
       if (hkantenliste->next != NULL)
          hkantenliste = hkantenliste->next;
         else
          {
            hedgraph = hedgraph->next;
            hkantenliste = hedgraph->kanten;
          }
    }  /*  while  */
 }
}   /*  suche_externes_face  */





void doppelkante(nummer,pointer)  
/*  Hilfsprocedur von verdopple_kanten  */
/*  Traegt in edgraph beim Knoten nummer eine Kante ein, deren Daten
    aus pointer entnommen wird.  */
TKANTENLISTE* pointer;
{
TEDGRAPH *hedgraph;
TKANTENLISTE *hkantenliste;

hedgraph = edgraph;
while (hedgraph->nummer != pointer->dest)
  hedgraph = hedgraph->next;
hkantenliste = (TKANTENLISTE*) malloc(sizeof(TKANTENLISTE));
strcpy(hkantenliste->name,pointer->name);
hkantenliste->nummer = pointer->nummer;
hkantenliste->dest = nummer;
hkantenliste->knick1x = pointer->knick4x;
hkantenliste->knick1y = pointer->knick4y;
hkantenliste->knick2x = pointer->knick3x;
hkantenliste->knick2y = pointer->knick3y;
hkantenliste->knick3x = pointer->knick2x;
hkantenliste->knick3y = pointer->knick2y;
hkantenliste->knick4x = pointer->knick1x;
hkantenliste->knick4y = pointer->knick1y;
hkantenliste->fwinkel = pointer->lwinkel;
hkantenliste->lwinkel = pointer->fwinkel;
hkantenliste->dop = 1;
hkantenliste->frichtung = 0;
hkantenliste->lrichtung = 0;
hkantenliste->gefunden = pointer->gefunden;
hkantenliste->ext =0;
hkantenliste->next = hedgraph->kanten;
hedgraph->kanten = hkantenliste;
return;
}  /*  doppelkante  */




void berechne_winkel()
/*  In edgraph werden die Winkel eingetragen:   */
/*  Def.: Waagrechte Kanten von links nach rechts gehen nach unten;
                            von rechts nach links gehen nach oben.  */
{
TEDGRAPH *hedgraph;
TKANTENLISTE *hkantenliste;
int richtung;
double xdiff,ydiff,winkel;
hedgraph = edgraph;
while (hedgraph != NULL)
  {
    hkantenliste = hedgraph->kanten;
    while (hkantenliste != NULL)
      {
         if (hkantenliste->knick2y - hkantenliste->knick1y >= 0)
           {
             ydiff = (double) (hkantenliste->knick2y - hkantenliste->knick1y);
             richtung = 2;  /*  nach unten  */
             xdiff = (double) (hkantenliste->knick1x - hkantenliste->knick2x);
           }  /*  if  */
           else
           {
             ydiff = (double) (hkantenliste->knick1y - hkantenliste->knick2y);
             richtung = 1;  /*  nach oben  */
             xdiff = (double) (hkantenliste->knick2x - hkantenliste->knick1x);
           }
         if (ydiff == 0.0)
           {
              ydiff = 0.0000000000000001;
              if (xdiff > 0)   /*  von rechts nach links  */
                 {
                    richtung = 1;
                    xdiff *= -1.0;
                 }
               /*  else
                 {
                    richtung = 2;
                    xdiff *= 1.0;
                 }   */
           }  /*  if  */
         winkel = xdiff/ydiff;
         hkantenliste->fwinkel = winkel;
         hkantenliste->frichtung = richtung;

         if (hkantenliste->knick4y - hkantenliste->knick3y >= 0)
           {
             ydiff = (double) (hkantenliste->knick4y - hkantenliste->knick3y);
             richtung = 2;  /*  nach unten  */
             xdiff = (double) (hkantenliste->knick3x - hkantenliste->knick4x);
           }  /*  if  */
           else
           {
             ydiff = (double) (hkantenliste->knick3y - hkantenliste->knick4y);
             richtung = 1;  /*  nach oben  */
             xdiff = (double) (hkantenliste->knick4x - hkantenliste->knick3x);
           }
         if (ydiff == 0.0)
           {
              ydiff = 0.0000000000000001;
              if (xdiff > 0)   /*  von rechts nach links  */
                 {
                    richtung = 1;
                    xdiff *= -1.0;
                 }
               /*  else
                 {
                    richtung = 2;
                    xdiff *= 1.0;
                 }   */
           }  /*  if  */
         winkel = xdiff/ydiff;
         hkantenliste->lwinkel = winkel;
         hkantenliste->lrichtung = richtung;
         hkantenliste = hkantenliste->next;
      }   /*  while  */
    hedgraph = hedgraph->next;
  }  /*  while  */
return;
}  /*  berechne_winkel  */




void berechne_faces()
/*  Ertsellt aus den Daten in edgraph die planare Darstellung ( Listen der
     Faces).  */
{
TEDGRAPH *hedgraph,*suchedgraph;
TKANTENLISTE *hkantenliste,*suchkantenliste,*merksuchkantenliste,*lkantenliste;
TKANTENLISTE *freeliste;
TGRAPH *hgraph,*hgraph2;
TKANTENTRIPEL *hkantentripel,*hkantentripel1,*hilf;
TFACE *hflaeche;
TFACEMENGE *hfacemenge;
int gefunden,richtung,dest,hkantenzahl;
double winkel;
int facezaehler = 0;
int externface = 0;
int extgefunden = 0;
if (seingabe->directed == 0)
  hkantenzahl = kantenzahl/2;
 else hkantenzahl = kantenzahl;
facezahl = hkantenzahl + 2 - knotenzahl;
while (facezaehler < facezahl)
  {
    facezaehler++;
    hfacemenge = (TFACEMENGE*)malloc(sizeof(TFACEMENGE));
    hfacemenge->nummer = facezaehler;
    hfacemenge->geaendert = 0;
    hfacemenge->behandelt = 0;
    hfacemenge->next = facemenge;
    facemenge = hfacemenge;
    hedgraph = edgraph;
    while (hedgraph->kanten == NULL)
      hedgraph = hedgraph->next;
    hkantenliste = hedgraph->kanten;
    suchedgraph = hedgraph;
    hedgraph->visited = 1;
    while (hkantenliste->gefunden == 0)
     {
       hkantenliste->gefunden = suchedgraph->visited;
       richtung = 0;
       suchedgraph = edgraph;
       while (suchedgraph->nummer != hkantenliste->dest)
         suchedgraph = suchedgraph->next;
       suchedgraph->visited++;
       suchkantenliste = suchedgraph->kanten;
       merksuchkantenliste = suchkantenliste;
       while (suchkantenliste != NULL)
         {
            if (richtung == 0)
               {
                 winkel = suchkantenliste->fwinkel;
                 richtung = suchkantenliste->frichtung;
               }
            else 
              {
                 if (richtung == hkantenliste->lrichtung)
                   {
                      if (((suchkantenliste->frichtung == richtung)&&
                          (suchkantenliste->fwinkel > winkel))||
                          ((suchkantenliste->frichtung != richtung)&&
                           (suchkantenliste->fwinkel < hkantenliste->lwinkel)))
                        {
                           winkel = suchkantenliste->fwinkel;
                           richtung = suchkantenliste->frichtung;
                        }
                   }
                   else if (winkel >= hkantenliste->lwinkel)
                   {
                      if ((richtung != suchkantenliste->frichtung)||

                          ((winkel < suchkantenliste->fwinkel)||
                          (hkantenliste->lwinkel > suchkantenliste->fwinkel))||

                          (hkantenliste->lwinkel == winkel))                  
                         {
                           winkel = suchkantenliste->fwinkel;
                           richtung = suchkantenliste->frichtung;
                         }
                   }
                   else
                   {
                      if ((richtung == suchkantenliste->frichtung)&&
                          (winkel < suchkantenliste->fwinkel)&&
                          (hkantenliste->lwinkel > suchkantenliste->fwinkel))
                         {
                           winkel = suchkantenliste->fwinkel;
                           richtung = suchkantenliste->frichtung;
                         }
                   }
              }  /*  if  */
            suchkantenliste = suchkantenliste->next;
         }   /*  while  */
       gefunden = 0;
       hkantenliste = merksuchkantenliste;
       while (gefunden == 0)
         {
            if ((hkantenliste->fwinkel == winkel)&&
                (hkantenliste->frichtung == richtung))
               gefunden = 1;
             else hkantenliste = hkantenliste->next;
         }
     }  /*  while */
    hflaeche = (TFACE*) malloc(sizeof(TFACE));
    hflaeche->nummer = 0;
    hflaeche->face = NULL;
    if (extgefunden == 0)
       hflaeche->nummer = facezaehler + 1;
       else hflaeche->nummer = facezaehler;
    gefunden = 1;
    hkantentripel = NULL;
    dest = hedgraph->nummer;
    while (gefunden == 1)
     {
       gefunden = 0;
       hkantentripel1 = (TKANTENTRIPEL*) malloc (sizeof(TKANTENTRIPEL));
       hkantentripel1->name = hkantenliste->nummer;
       hkantentripel1->s = 0;
       hkantentripel1->a = 0;
       hkantentripel1->afix = 0;
       hkantentripel1->l = 0;
       hkantentripel1->dir = 0;
       hkantentripel1->tried = 0;
       hkantentripel1->bridge = 0;
       hkantentripel1->anfang = 0;
       hkantentripel1->ende = 0;
       hkantentripel1->p = 0;
       hkantentripel1->fixed = 0;
       hkantentripel1->next = NULL;
       if (hkantentripel == NULL)
         hkantentripel = hkantentripel1;
         else
          {
            hilf = hkantentripel;
            while (hilf->next != NULL)
               hilf = hilf->next;
            hilf->next = hkantentripel1;
          }
       dest = hkantenliste->dest;
       if (hkantenliste->ext == 1)
         {
           externface = 1;
           extgefunden = 1;
         }
       lkantenliste = hedgraph->kanten;
       if (lkantenliste->gefunden == 1)
          {
           /*   free(hedgraph->kanten);  */
            hedgraph->kanten = lkantenliste->next;
          }
          else
          {
            while (lkantenliste->next->gefunden != 1)
              {
                 lkantenliste = lkantenliste->next;
              }
            freeliste = lkantenliste->next;
            lkantenliste->next = lkantenliste->next->next;
            /*  free(freeliste);  */
          }
       lkantenliste = hedgraph->kanten;
       while (lkantenliste != NULL)
         {
            if (lkantenliste->gefunden > 0)
              lkantenliste->gefunden--;
            lkantenliste = lkantenliste->next;
         }
       hedgraph = edgraph;
       while (hedgraph->nummer != dest)
         hedgraph = hedgraph->next;
       hkantenliste = hedgraph->kanten;
       while ((gefunden == 0)&&(hkantenliste != NULL))
         if (hkantenliste->gefunden == 1)
           gefunden = 1;
           else hkantenliste = hkantenliste->next;
     }
    hflaeche->face = hkantentripel;
    hgraph = (TGRAPH*) malloc(sizeof(TGRAPH));
    hgraph->flaeche = hflaeche;
    hgraph->next = NULL;
    if (externface == 1)
      {
        hflaeche->nummer = 1;
        hgraph->next = graph;
        graph = hgraph;
      }
     else
      {
        if (graph == NULL)
            graph = hgraph;
           else
            {
              hgraph2 = graph;
              while (hgraph2->next != NULL)
                hgraph2 = hgraph2->next;
              hgraph2->next = hgraph;
              hgraph->next = NULL;
            }
      }
    externface = 0;
    hedgraph = edgraph;
    while (hedgraph != NULL)
      {
        hedgraph->visited = 0;
        hkantenliste = hedgraph->kanten;
        while (hkantenliste != NULL)
           {
              hkantenliste->gefunden = 0;
              hkantenliste = hkantenliste->next;
           }
        hedgraph = hedgraph->next;
      }
  }  /*  while  */
}  /*  berechne_faces  */



void berechne_knoten() 
/*  Bestimmt die Werte fuer Knoten und Grad in TKANTENTRIPEL  */
/*  Fuer alle Segmente mit Grad >= 4 wird bei jeder Kante der Knoten
    davor bestimmt und eingetragen.  */

{
int suche_anfang();
int suche_ende();
TGRAPH *hgraph,*hhgraph;
TKANTENTRIPEL *hface,*hhface,*hface2;
TFACEMENGE *hfacemenge;
TKNOTENMENGE *hknotenmenge;
int name1,name2,anfang1,anfang,ende1,ende,gefunden,knoten,ok,zaehler;
zaehler = 0;
hfacemenge = facemenge;
while (hfacemenge != NULL)
  {
     if (hfacemenge->grad > 2)
       {
         zaehler++;
         hgraph = graph;
         while (hgraph->flaeche->nummer != hfacemenge->nummer)
            hgraph = hgraph->next;
         hface = hgraph->flaeche->face;
         anfang1 = suche_anfang(hface->name);
         ende1 = suche_ende(hface->name);
         hface = hface->next;
         anfang = suche_anfang(hface->name);
         ende = suche_ende(hface->name);
         if ((anfang1 == anfang)&&(ende1 == ende)||
             (anfang1 == ende)&&(anfang == ende1))
           {
             hhface = hgraph->flaeche->face;
             while (hhface->next != NULL)
               hhface = hhface->next;
             anfang = suche_anfang(hhface->name);
             ende = suche_ende(hhface->name);
             hface = hgraph->flaeche->face;
           }
         if ((anfang1 == anfang)||(anfang1 == ende))
            {
              name2 = anfang1;
              name1 = ende1;
            }
           else 
            {
              name2 = ende1;
              name1 = anfang1;
            }
         gefunden = 0;
         hknotenmenge = knotenmenge;
         while (gefunden == 0)
           {
              if (hknotenmenge->nummer == name2)
                 gefunden = 1;
                else hknotenmenge = hknotenmenge->next;
           }
         hgraph->flaeche->face->anfang = name1;
         hgraph->flaeche->face->ende = name2;
         if (hknotenmenge->grad == 1)
           hgraph->flaeche->face->p = 0;
          else if (hknotenmenge->grad == 2)
           hgraph->flaeche->face->p = 33;
          else if (hknotenmenge->grad == 3)
           hgraph->flaeche->face->p = 67;
          else hgraph->flaeche->face->p = 100;
         while (hface->next != NULL)
           {
             anfang = suche_anfang(hface->name);
             ende = suche_ende(hface->name);
             if (name2 == anfang)
                name1 = ende;
               else name1 = anfang;
             hface->anfang = name2;
             hface->ende = name1;
             hknotenmenge = knotenmenge;
             gefunden = 0;
             while (gefunden == 0)
               {
                  if (hknotenmenge->nummer == name1)
                     gefunden = 1;
                    else hknotenmenge = hknotenmenge->next;
               }
             if (hknotenmenge->grad == 1)
               hface->p = 0;
              else if (hknotenmenge->grad == 2)
               hface->p = 33;
              else if (hknotenmenge->grad == 3)
               hface->p = 67;
              else hface->p = 100;
             name2 = name1;
             hface = hface->next;
           }  /*  while  hface->next != NULL  */
          anfang = suche_anfang(hface->name);
          ende = suche_ende(hface->name);
          if (name2 == anfang)
             name1 = ende;
            else name1 = anfang;
          hface->anfang = name2;
          hface->ende = name1;
          hknotenmenge = knotenmenge;
          gefunden = 0;
          while (gefunden == 0)
            {
               if (hknotenmenge->nummer == name1)
                  gefunden = 1;
                 else hknotenmenge = hknotenmenge->next;
            }
          if (hknotenmenge->grad == 1)
            hface->p = 0;
           else if (hknotenmenge->grad == 2)
            hface->p = 33;
           else if (hknotenmenge->grad == 3)
            hface->p = 67;
           else hface->p = 100;
       }  /*  if grad > 3  */
     hfacemenge = hfacemenge->next;
  }  /*  while hfacemenge != NULL  */
hfacemenge = facemenge;
while (hfacemenge != NULL)
  {
    if (hfacemenge->grad == 1)
      {
        zaehler++;
        hgraph = graph;
        while (hgraph->flaeche->nummer != hfacemenge->nummer)
          hgraph = hgraph->next;
        hface = hgraph->flaeche->face;
        anfang = suche_anfang(hface->name);
        hknotenmenge = knotenmenge;
        while (hknotenmenge->nummer != anfang)
          hknotenmenge = hknotenmenge->next;
        hgraph->flaeche->face->anfang = anfang;
        hgraph->flaeche->face->ende = anfang;
        if (hknotenmenge->grad == 2)
          hgraph->flaeche->face->p = 33;
         else if (hknotenmenge->grad == 3)
          hgraph->flaeche->face->p = 67;
         else hgraph->flaeche->face->p = 100;
      }  /*  if grad == 1  */
    hfacemenge = hfacemenge->next;
  }  /*  while  */
if (zaehler == 0)  /*  graph besteht aus genau zwei Punkten  */
  {
    hgraph = graph;
    hface = hgraph->flaeche->face;
    hface->anfang = 1;
    hface->ende = 2;
    hface->next->anfang = 2;
    hface->next->ende = 1;
    if (knotenmenge->grad == 1)
      {
        hface->p = 0;
        hface->next->p = 0;
      }
     else if (knotenmenge->grad == 2)
      {
        hface->p = 33;
        hface->next->p = 33;
      }
     else if (knotenmenge->grad == 3)
      {
        hface->p = 67;
        hface->next->p = 67;
      }
     else
      {
        hface->p = 100;
        hface->next->p = 100;
      }
  }  /*  zaehler == 0  */
ok = 0;
while (ok == 0)
  {
    ok = 1;
    hfacemenge = facemenge;
    while (hfacemenge != NULL)
      {
        if ((hfacemenge->grad == 2)&&(facezahl > 1))
          {
            hgraph = graph;
            while (hgraph->flaeche->nummer != hfacemenge->nummer)
              hgraph = hgraph->next;
            hface = hgraph->flaeche->face;
            while (hface != NULL)
              {
                gefunden = 0;
                hhgraph = graph;
                if (hhgraph->flaeche->nummer == hgraph->flaeche->nummer)
                  hhgraph = hhgraph->next;
                hhface = hhgraph->flaeche->face;
                while (gefunden == 0)
                  {
                    if (hhface->name == hface->name)
                      gefunden = 1;
                     else
                      if (hhface->next != NULL)
                        hhface = hhface->next;
                       else
                         {
                           hhgraph = hhgraph->next;
                            if (hhgraph->flaeche->nummer == 
                                hgraph->flaeche->nummer)
                             hhgraph = hhgraph->next;
                           hhface = hhgraph->flaeche->face;
                         }
                  }  /*  while gefunden  */
                if (hhface->anfang != 0)
                  {
                    hface->anfang = hhface->ende;
                    hface->ende = hhface->anfang;
                    hknotenmenge = knotenmenge;
                    while (hknotenmenge->nummer != hface->anfang)
                      hknotenmenge = hknotenmenge->next;
                    if (hknotenmenge->grad == 1)
                      hface->p = 0;
                     else if (hknotenmenge->grad == 2)
                      hface->p = 33;
                     else if (hknotenmenge->grad == 3)
                      hface->p = 67;
                     else hface->p = 100;
                    if (hface->next != NULL)
                      hface2 = hface->next;
                     else hface2 = hgraph->flaeche->face;
                    hface2->anfang = hhface->anfang;
                    hface2->ende = hhface->ende;
                    hknotenmenge = knotenmenge;
                    while (hknotenmenge->nummer != hface2->anfang)
                      hknotenmenge = hknotenmenge->next;
                    if (hknotenmenge->grad == 1)
                      hface2->p = 0;
                     else if (hknotenmenge->grad == 2)
                      hface2->p = 33;
                     else if (hknotenmenge->grad == 3)
                      hface2->p = 67;
                     else hface2->p = 100;
                  }
                 else if (facezahl > 1)
                  ok = 0;
                    else
                         {
                           graph->flaeche->face->p = 33;
                           graph->flaeche->face->next->p = 33;
                         }
                 hface = hface->next;
               }  /*  while  hface != NULL  */
          }  /* if grad == 2  */
        hfacemenge = hfacemenge->next;
      }  /*  while  hfacemenge != NULL  */
  }  /*  while  ok == 0  */
}  /*  berechne_knoten  */




void tests()
/*  Es wird getestet, ob der Graph die Eigenschaften:
   1. Knotengrad <=4 
   2. Zusammenhang 
         erfuellt.                         */
{
TEDGRAPH *hedgraph;
TZAHLLISTE *liste,*hliste,*freeliste;
TKANTENLISTE *hkantenliste;
int gradkorrekt,zusammenkorrekt,knoten,zahl;
gradkorrekt = 1;
zusammenkorrekt = 1;
hedgraph = edgraph;
while (hedgraph != NULL)
  {
    if (hedgraph->kantenzahl > 4)
       gradkorrekt = 0;
    hedgraph = hedgraph->next;
  }  /*  while  */
if (gradkorrekt == 0)
   {
      error("Graph hat einen Knoten mit Grad > 4!\n");
      Korrekt = 0;
   }
liste = (TZAHLLISTE*) malloc(sizeof(TZAHLLISTE));
liste->zahl = edgraph->nummer;
liste->next = NULL;
while (liste != NULL)
  {
    knoten = liste->zahl;
    freeliste = liste;
    liste = liste->next;
    /*  free(freeliste);  */
    hedgraph = edgraph;
    while (hedgraph->nummer != knoten)
      hedgraph = hedgraph->next;
    hedgraph->zusammen = 1;
    hkantenliste = hedgraph->kanten;
    while (hkantenliste != NULL)
      {
        zahl = hkantenliste->dest;
        hedgraph = edgraph;
        while (hedgraph->nummer != zahl)
          hedgraph = hedgraph->next;
        if (hedgraph->zusammen == 0)
          {
             hedgraph->zusammen = 1;
             hliste = (TZAHLLISTE*) malloc(sizeof(TZAHLLISTE));
             hliste->zahl = zahl;
             hliste->next = liste;
             liste = hliste;
          }  /*  while  zusammen...  */
        hkantenliste = hkantenliste->next;
      }  /*  while hkantenliste...  */
  }  /*  while  liste != NULL  */
hedgraph = edgraph;
while (hedgraph != NULL)
  {
    if (hedgraph->zusammen == 0)
      zusammenkorrekt = 0;
    hedgraph = hedgraph->next;
  }  /*  while  */
if (zusammenkorrekt == 0)
   {
      error("Graph ist nicht zusammenhaengend!\n");
      Korrekt = 0;
   }
}  /*  tests  */






void berechne_netzwerk(graph)  /* Berechnung des zugehoerigen
                                  Netzwerkes aus dem Graphen  */
{
void bestimme_Vdach();
void bestimme_U();
void bestimme_A();
void bestimme_z();



bestimme_Vdach();
bestimme_U();
bestimme_A();
bestimme_z();

}





void knotengrad()  /* Hilfsprocedur zur Bestimmung des Grads der Knoten */
{
int i,g,num;
TKNOTENMENGE *hknotenmenge;
TKANTENMENGE *hkantenmenge;
hknotenmenge = knotenmenge;
while (hknotenmenge != NULL)
  {
    num = hknotenmenge->nummer;
    hkantenmenge = kantenmenge;
    g = 0;
    while (hkantenmenge != NULL)
      {
        if (hkantenmenge->anfang == num)
           g++;
        if (hkantenmenge->ende == num)
           g++;
        hkantenmenge = hkantenmenge->next;
      }
    hknotenmenge->grad = g;
    hknotenmenge = hknotenmenge->next;
  }
}




void facegrad()  /* Hilfsprocedur zur Bestimmung des Grads der Faces*/
{
void suche();
int g,num;
TKANTENTRIPEL *hkanten;
TFACE *hface;
TGRAPH *hgraph;
hgraph = graph;
while (hgraph != NULL)
  {
    hface = hgraph->flaeche;
    hkanten = hface->face;
    num = hface->nummer;
    g = 0;
    while (hkanten != NULL)
      {
        g++;
        hkanten = hkanten->next;
      }
    suche(num,g);
    hgraph = hgraph->next;
  }
}



void suche(zahl,grad)   /*   Hilfsprocedur von facegrad   */

/*  sucht in facemenge das face,dessen Nummer zahl ist, und traegt
    den Grad grad ein.   */

{
TFACEMENGE *hilf= facemenge;
int gefunden=0;
while ((gefunden == 0)&& (hilf != NULL))
  {
    if (hilf->nummer == zahl)
         gefunden = 1;
         else hilf = hilf->next;
  }
hilf->grad = grad;
}





/*--------------------------------------------------------------------------*/





void bestimme_Vdach()  /*  Hilfsprocedur zur Bestimmung der Knoten mit
                           Grad ungleich 4    */
{
int platz;
TKNOTENMENGE *hilf1,*hilf2;
platz = sizeof(TKNOTENMENGE);
hilf1 = knotenmenge;
while (hilf1 != NULL)
  {
    if (hilf1->grad != 4)
      {
        hilf2= (TKNOTENMENGE *) malloc(platz);
        hilf2->nummer = hilf1->nummer;
        hilf2 ->grad = hilf1->grad;
        hilf2->next = Vdachmenge;
        Vdachmenge = hilf2;
       }
   hilf1 = hilf1->next;
  }
}




void bestimme_U()  /* Hilfsprocedur zur Bestimmung der Knoten des Netzwerks */
{
void mache_netzknoten();
int i=2;
int j,vorrat;
TKNOTENMENGE *hilfv;
TFACEMENGE *hilff;
mache_netzknoten(1,0,'s',0);   /*  Quelle  */
hilfv = Vdachmenge;
while (hilfv != NULL)
  {
    j = hilfv->nummer;
    vorrat = 4 - hilfv->grad;
    mache_netzknoten(i,j,'v',vorrat);
    i++;
    hilfv = hilfv->next;
  };
hilff = facemenge;
while (hilff != NULL)
  {
    j = hilff->nummer;
    mache_netzknoten(i,j,'f',0);
    i++;
    hilff = hilff->next;
  }
mache_netzknoten(i,0,'t',0);    /*  Senke  */
netzknotenanz = i;
} 


void mache_netzknoten( num,herkunft,typ,vorrat)   
               /* Hilfsprocedur von bestimme_U  */
{ 
int platz=sizeof(TNETZKNOTENMENGE);
TNETZKNOTENMENGE *hilf;
hilf = (TNETZKNOTENMENGE *) malloc(platz);
hilf ->nummer = num;
hilf->herkunft = herkunft;
hilf->typ = typ;
hilf ->perm= 0;
hilf->cost = 0;
hilf-> vor = 0;
hilf-> menge = 0;
hilf->vorrat = vorrat;
hilf ->next = netzknotenmenge;
netzknotenmenge = hilf;
return;
}




void bestimme_A()   /* Hilfsprocedur zur Bestimmung der Boegen des Netzwerks */

{
void mache_bogen();
int suche_Bogen();
int ermittle_netzknotenname();
int inVdach();
TNETZKNOTENMENGE *nkm;
TGRAPH *hgraph;
TFACE *hflaeche;
TKANTENTRIPEL *hface;
TKANTENMENGE *hkantenmenge;
TFACEMENGE *hfacemenge;
TKNOTENMENGE *hVdachmenge;
int knotnum,knotnum1,knotnum2,facenummer,gefunden,gefunden1,gefunden2;
int facenum1,facenum2,vorhanden,anfang,ende,hgrad,cap,name,name1,name2;
int noetig;

/*  Beginn der Berechnung der v-Boegen.  */
nkm = netzknotenmenge;
while (nkm != NULL)
  {
    if (nkm->typ =='v')
      {
        knotnum = nkm->herkunft;
        hgraph = graph;
        while (hgraph != NULL)
          {
             hflaeche = hgraph->flaeche;
             facenummer = hflaeche->nummer;
             gefunden = 0;
             hface = hflaeche->face;
             while ((gefunden == 0)&&(hface != NULL))
                {
                   anfang = hface->anfang;
                   if (anfang == knotnum)
                      {
                         gefunden = 1;
                      }
                   else
                      {
                        ende = hface->ende;
                        if (ende == knotnum) gefunden = 1;
                      }
                                      hface = hface->next;
                }   /*  while  */
             if (gefunden == 1)
                 { 
                   noetig = inVdach(knotnum);
                   if (noetig == 1)
                     {
                       name1 = ermittle_netzknotenname('v',knotnum);
                       name2 = ermittle_netzknotenname('f',facenummer);
                       mache_bogen('v',name1,name2,0);
                     }  /*  if  */
                 }    /*  if   */
             hgraph = hgraph->next;
          }   /*  while  */
      }   /*  if  */
    nkm = nkm->next;
  }   /*  while  */
/*   Ende der Berechnung der v-Boegen.  */
/*   Beginn der Berechnung der f-Boegen.  */
hkantenmenge = kantenmenge;
while (hkantenmenge != NULL)
 {
   name = hkantenmenge->nummer;
   gefunden1 = 0;
   gefunden2 = 0;
   hgraph = graph;
   while((gefunden1 == 0)||(gefunden2 == 0))
     {
       hflaeche = hgraph->flaeche;
       facenummer = hflaeche->nummer;
       hface = hflaeche->face;
       while ((hface != NULL)&&((gefunden1 ==0)||(gefunden2 ==0)))
         {
           if (hface->name == name)
             {
               if (gefunden1 == 0)  
                 {
                   gefunden1 = 1;
                   facenum1 = facenummer;
                 }
                                   else 
                 {
                   gefunden2 = 1;
                   facenum2 = facenummer;
                 }
             }   /*  if  */
           hface = hface->next;
         }   /*  while  */
        hgraph = hgraph->next;
     }  /*  while  */
   name1 = ermittle_netzknotenname('f',facenum1);
   name2 = ermittle_netzknotenname('f',facenum2);
   vorhanden = suche_Bogen(name1,name2);
   if (vorhanden == 0)
    {
     if (facenum1 != facenum2)
       {
         mache_bogen('f',name1,name2,0);
         mache_bogen('f',name2,name1,0);
       }
       else 
       {
         mache_bogen('f',name1,name2,0);
       }
    }
    hkantenmenge = hkantenmenge->next;
  }  /*  while  */
/*   Ende der Berechnung der f-Boegen.  */
/*   Beginn der Berechnung der s- und t-Boegen. */
hfacemenge = facemenge;
while (hfacemenge != NULL)
  {
    facenummer = hfacemenge->nummer;
    if (facenummer!= 1)
      {
        hgrad = hfacemenge->grad;
        if (hgrad < 4)
          {
            cap = 4 - hgrad;
            name = ermittle_netzknotenname('f',facenummer);
            mache_bogen('s',1,name,cap);
          }
        else if (hgrad > 4)
          {
            cap = hgrad - 4;
            name = ermittle_netzknotenname('f',facenummer);
            mache_bogen('t',name,netzknotenanz,cap);
          }
      }
     else
      {
        hgrad = hfacemenge->grad;
        cap = hgrad + 4;
        name = ermittle_netzknotenname('f',facenummer);
        mache_bogen('t',name,netzknotenanz,cap);
      }
    hfacemenge = hfacemenge->next;
  }
hVdachmenge = Vdachmenge;
while (hVdachmenge != NULL)
  {
    knotnum= hVdachmenge->nummer;
    cap = (4 - hVdachmenge->grad);
    name = ermittle_netzknotenname('v',knotnum);
    mache_bogen('s',1,name,cap);
    hVdachmenge = hVdachmenge->next;
  }
/*  Ende der Berechnung der s- und t-Boegen.  */

}




int inVdach(knoten)  /*  Hilfsprocedur von bestimme_A  */
/*  Liefert 1, wenn knoten in Vdachmenge ist.  */
{
TKNOTENMENGE *hVdachmenge;
int gefunden = 0;
hVdachmenge = Vdachmenge;
while ((gefunden == 0)&&(hVdachmenge != NULL))
  {
    if (hVdachmenge->nummer == knoten)
       gefunden = 1;
    hVdachmenge = hVdachmenge->next;
  }
return(gefunden);
}  /*  inVdach  */



void mache_bogen(typ,anf,ende,cap)   /*  Hilfsprocedur von bestimme_A  */

/*  Erzeugt einen Bogen des Typs typ von anf nach ende und traegt ihn 
    in bogenmenge ein.  */

{

int platz=sizeof(TBOGENMENGE);
int name;
TBOGENMENGE *hilf;
TNETZKNOTENMENGE *hnetzknotenmenge;
hnetzknotenmenge = netzknotenmenge;
while ((hnetzknotenmenge->typ != 'f')||(hnetzknotenmenge->herkunft != 1))
  hnetzknotenmenge = hnetzknotenmenge->next;
name = hnetzknotenmenge->nummer;
hilf = (TBOGENMENGE *) malloc(platz);
hilf ->anfang = anf;
hilf->ende = ende;
hilf->typ = typ;
if (typ == 'f')
  {
    if (hilf->ende == name)
       hilf->costen = 9980;
      else hilf->costen = 10000;
  }
  else hilf->costen = 0;
if ((typ == 'f')||(typ == 'v'))
  hilf->cap = maxcap;
   else hilf->cap = cap;
hilf->flow = 0;
hilf->fixed = -1;
hilf->f1bogen = 0;
hilf ->next = bogenmenge;
bogenmenge = hilf;
Bogenzaehler++;
return;
}



int suche_Bogen(f1,f2)    /*  Hilfsprocedur von bestimme_A .   */
/*  Prueft,ob der Bogen von face f1 nach face f2 schon in Bogenmenge
    existiert.   */

{
int gefunden = 0;
TBOGENMENGE *hbogenmenge;
hbogenmenge = bogenmenge;
while (( gefunden == 0)&&(hbogenmenge != NULL))
  {
    if (hbogenmenge->typ == 'f')
      {
        if ((hbogenmenge->anfang == f1)&&(hbogenmenge->ende == f2))
           gefunden = 1;
      }
    hbogenmenge = hbogenmenge->next;
  }
return(gefunden);
}



int suche_anfang(name)  
 /* Hilfsprocedur von bestimme_A und von hilfsknoteneinfuegen */
/*  Liefert den Anfangsknoten der Kante name  */

{
TKANTENMENGE *hmenge;
int anfang;
int gefunden = 0;
hmenge = kantenmenge;
while (gefunden == 0)
  {
    if (hmenge->nummer == name)
       {
         gefunden = 1;
         anfang = hmenge->anfang;
       }
     else hmenge = hmenge->next;
  }
return(anfang);
}




int suche_ende(name)   /* Hilfsprocedur von bestimme_A  */
/*  Liefert den Endknoten der Kante name  */

{
TKANTENMENGE *hmenge;
int ende;
int gefunden = 0;
hmenge = kantenmenge;
while (gefunden == 0)
  {
    if (hmenge->nummer == name)
       {
         gefunden = 1;
         ende = hmenge->ende;
       }
     else hmenge = hmenge->next;
  }
return(ende);
}


int ermittle_netzknotenname(typ,herkunft)  /*  Hilfsprocedur von bestimme_A  */
/*  Liefert aus netzknotenmenge die Nummer des Knotens,dessen Herkunft
    herkunft und dessen Typ typ ist.  */
{
TNETZKNOTENMENGE *hnetzknotenmenge;
int nummer;
int gefunden = 0;
hnetzknotenmenge = netzknotenmenge;
while (gefunden == 0)
  {
   if ((hnetzknotenmenge->typ == typ)&&(hnetzknotenmenge->herkunft == herkunft))
      {
        gefunden = 1;
        nummer = hnetzknotenmenge->nummer;
      }
     else hnetzknotenmenge = hnetzknotenmenge->next;
  }
return(nummer);
}




void bestimme_z()  /* Hilfsprocedur zur Bestimmung des Flusses des Netzwerks */

{
TBOGENMENGE *hbogenmenge;
hbogenmenge = bogenmenge;
Flusswert = 0;
while (hbogenmenge != NULL)
  {
    if (hbogenmenge->typ == 's')
      Flusswert+= hbogenmenge->cap;
    hbogenmenge = hbogenmenge->next;
  }
return;
}


/*****************************************************************************/
/*****************************************************************************/



void berechne_orthogonale_darstellung()
/*  Es wird die orthogonale Darstellung ohne Laengenangaben erstellt.
    Dazu muss ein Min_Cost_Flow Problem geloest werden.  */
{
void bestimme_bruecken();
void Min_Cost_Flow();
void orthogonalisiere();
int voroptimieren();
void knickhilfsknoten();
int optimieren();
void fluss_fixieren();
void modifizieren();
int weiter;
bestimme_bruecken();
Min_Cost_Flow();  /*  Berechnet aus dem Netzwerk den zugehoerigen
                      Fluss.  */
orthogonalisiere();    /*  Berechnet aus dem Fluss die zugehoerige
                           orthogonale Darstellung.  */
weiter = 1;
while (weiter != 0)
  {
    weiter = voroptimieren();
    if (weiter != 0)
      {
        Min_Cost_Flow();
        fluss_fixieren(weiter);
        orthogonalisiere();
        modifizieren(weiter);
      }
  }  /*  while weiter != 0  */
knickhilfsknoten();
weiter = 1;
while(weiter != 0)
  {
     weiter = optimieren();
  }  /*  while weiter != 0  */

}  /*  berechne_orthogonale_darstellung  */




void Min_Cost_Flow()  
 /*  Hier wird versucht, ein geeigneter Min_Cost_Flow-
                           Algorithmus zu implementieren.  */
{
void kuerzeste_Wege();
void augmentiere();
void initialisiere();
void initfluss();
void wiederherstellen();
int vor = 1;
int fix = 0;
Kosten = 0;
initialisiere();   /*  Netzknoten: perm = 0; cost setzen ...   */
while (fix < Fixed)
  {
     kuerzeste_Wege(vor);
     augmentiere();
     fix++;
     wiederherstellen();
  }  /*  while  */
Fluss = Fixed;
vor = 0;
initfluss;
while (Fluss < Flusswert)
  {
    kuerzeste_Wege(vor);
    augmentiere();
    Fluss++;
    wiederherstellen();
  }  /*  while  */
}   /*  Min_Cost_Flow  */




void initialisiere()   /*  Hilfsprocedur von Min_Cost_Flow  */
/*  Fuer jeden Knoten in netzknotenmenge werden die Werte fuer perm
    und cost eingetragen. */
{
int sbogen();
int f1bogen();
void initfluss();
int knoten,kosten,cap,capf1;
TBOGENMENGE *hbogenmenge;
TNETZKNOTENMENGE *hnetzknotenmenge;
TKNOTENMENGE *hVdachmenge;
hnetzknotenmenge = netzknotenmenge;
hbogenmenge = bogenmenge;
while (hbogenmenge != NULL)
  {
    hbogenmenge->flow = 0;
    hbogenmenge = hbogenmenge->next;
  }  /*  while  */
while (hnetzknotenmenge != NULL)
  {
    if (hnetzknotenmenge->typ == 's')
      {
        hnetzknotenmenge->perm = 1;
        hnetzknotenmenge->cost = 0;
        hnetzknotenmenge->vor = 0;
        hnetzknotenmenge->menge = Flusswert;
      }   /*  if  */
     else
      {
        if (hnetzknotenmenge->typ == 'v')
           {
             hVdachmenge = Vdachmenge;
             while (hVdachmenge->nummer != hnetzknotenmenge->herkunft)
               hVdachmenge = hVdachmenge->next;
             hnetzknotenmenge->vorrat = (4 - hVdachmenge->grad);
           }  /*  if */
        hnetzknotenmenge->perm = 0;
        knoten = hnetzknotenmenge->nummer;
        cap = sbogen(knoten);
        if (cap < 0)
         {
           hnetzknotenmenge->cost = maxcost;
         }
          else 
         {
          hnetzknotenmenge->cost = 0;
          hnetzknotenmenge->vor = 1;
          hnetzknotenmenge->menge = cap;
         }
      }
    hnetzknotenmenge = hnetzknotenmenge->next;
  }
return;
}   /*  initialisiere  */





void initfluss()  /*  Hilfsprocedur von initialisiere  */
/*  Ein Fluss der Staerke 1 wird eingetragen von s -> knoten -> f1 -> t 
    fuer alle Knoten vom Typ 'v', die einen Bogen nach f1 besitzen
    und noch flow = 0 darauf gilt.    */

{
int f1bogen();
int sbogen();
TBOGENMENGE *hbogenmenge;
TNETZKNOTENMENGE *hnetzknotenmenge;
int gefunden,f1knoten,capf1,aend,knoten,cap;          
hnetzknotenmenge = netzknotenmenge;
gefunden = 0;
while (gefunden == 0)
  {
    if ((hnetzknotenmenge->typ == 'f')&&(hnetzknotenmenge->herkunft == 1))
      gefunden = 1;
     else hnetzknotenmenge = hnetzknotenmenge->next;
  }
f1knoten = hnetzknotenmenge->nummer;
hnetzknotenmenge = netzknotenmenge;
while (hnetzknotenmenge != NULL)
  {
    if (hnetzknotenmenge->typ == 'v')
      {
        knoten = hnetzknotenmenge->nummer;
        capf1 = f1bogen(knoten);
        cap = sbogen(knoten);
        if (capf1 > 0)
          {
            aend = 0;
            hbogenmenge = bogenmenge;
            gefunden = 0;
            while (gefunden == 0)
              {
                  if ((hbogenmenge->anfang == knoten)&&
                      (hbogenmenge->ende == f1knoten))
                  gefunden = 1;
                    else hbogenmenge = hbogenmenge->next;
              }
            if ((hbogenmenge->flow == 0)&&(hnetzknotenmenge->vorrat > 0))
              aend = 1;
            if (aend == 1)
              hbogenmenge->flow ++;
            hbogenmenge->f1bogen = 1;
            hbogenmenge = bogenmenge;
            gefunden = 0;
            while (gefunden == 0)
              {
                if ((hbogenmenge->typ == 's')&&(hbogenmenge->ende == knoten))
                  gefunden = 1;
                 else hbogenmenge = hbogenmenge->next;
              }
            if (aend == 1)
              hbogenmenge->flow ++;
            hbogenmenge->f1bogen = 1;
            hbogenmenge = bogenmenge;
            gefunden = 0;
            while (gefunden == 0)
              {
                if ((hbogenmenge->typ == 't')&&
                    (hbogenmenge->anfang == f1knoten))
                  gefunden = 1;
                 else hbogenmenge = hbogenmenge->next;
              }
            if (aend == 1)
              hbogenmenge->flow ++;
            hbogenmenge->f1bogen = 1;
            if (aend == 1)
              {
                Fluss++;
                hnetzknotenmenge->menge--;
                if (cap == 1)
                  hnetzknotenmenge->cost = maxcost;
              }
           }
      }  
    hnetzknotenmenge = hnetzknotenmenge->next;
  }  /*  while  */

}  /*  initfluss */





void wiederherstellen()   /*  Hilfsprocedur von Min_Cost_Flow  */
/*  Fuer jeden Knoten in netzknotenmenge werden die Werte fuer perm
    und cost eingetragen.  */
{
int sbogen();
int knoten,kosten,cap;
TNETZKNOTENMENGE *hnetzknotenmenge;
hnetzknotenmenge = netzknotenmenge;
while (hnetzknotenmenge != NULL)
 {
    if (hnetzknotenmenge->typ == 's')
      {
        hnetzknotenmenge->perm = 1;
        hnetzknotenmenge->cost = 0;
        hnetzknotenmenge->vor = 0;
        hnetzknotenmenge->menge --;
      }   /*  if  */
     else
      {
        knoten = hnetzknotenmenge->nummer;
        cap = sbogen(knoten);
        if (cap < 0)
         {
          hnetzknotenmenge->cost = maxcost;
          hnetzknotenmenge->vor = 0;
          hnetzknotenmenge->menge = 0;
         }  /*  if  */
          else 
         {
          if (hnetzknotenmenge->perm == 0)
            {
              if (hnetzknotenmenge->cost != maxcost)
                {
                  if ( cap == 0)
                    hnetzknotenmenge->cost = maxcost;
                   else
                   hnetzknotenmenge->cost = 0;
                  hnetzknotenmenge->vor = 1;
                  hnetzknotenmenge->menge = cap;
                }  /*  if  */
            }  /*  if  */
          else
            {
              hnetzknotenmenge->vor = 1;
              hnetzknotenmenge->menge = cap;
              hnetzknotenmenge->perm = 0;
              if (hnetzknotenmenge->menge == 0)
                hnetzknotenmenge->cost = maxcost;
                else hnetzknotenmenge->cost = 0;
            }  /* else  */
          }  /*  else  */
        hnetzknotenmenge->perm = 0;
     }  /*  else  */
    hnetzknotenmenge = hnetzknotenmenge->next;
  }  /* while  */
return;

}   /*  wiederherstellen  */



int sbogen(knoten)   /*  Hilfsprocedur von initialisiere und wiederherstellen */
/*  Prueft, ob ein Bogen von s zu knoten existiert.Wenn ja,so wird
    der Wert der Kapazitaet dieses Bogens zurueckgegeben,
    sonst  -1.  */
{
TBOGENMENGE *hbogenmenge;
int gefunden = 0;
int cap = -1;
hbogenmenge = bogenmenge;
while ((gefunden == 0)&&(hbogenmenge != NULL))
  {
    if (hbogenmenge->typ == 's')
      {
        if (hbogenmenge->ende == knoten)
          {
            gefunden = 1;
            cap = hbogenmenge->cap - hbogenmenge->flow;
          }  /*  if  */
      }  /*  if  */
    hbogenmenge = hbogenmenge->next;
  }  /*  while  */
return(cap);
}  /*  sbogen  */





int f1bogen(knoten)   /*  Hilfsprocedur von initialisiere  */
/*  Prueft, ob ein Bogen von knoten zu f1 existiert.Wenn ja,so wird
    der Wert der Kapazitaet dieses Bogens zurueckgegeben,
    sonst  -1.  */
{
TBOGENMENGE *hbogenmenge;
TNETZKNOTENMENGE *hnetzknotenmenge;
int f1gefunden,f1knoten;
int gefunden = 0;
int cap = -1;
hnetzknotenmenge = netzknotenmenge;
f1gefunden = 0;
while (f1gefunden == 0)
  {
    if ((hnetzknotenmenge->typ == 'f')&&(hnetzknotenmenge->herkunft == 1))
      f1gefunden = 1;
     else hnetzknotenmenge = hnetzknotenmenge->next;
  }  /*  while  */
f1knoten = hnetzknotenmenge->nummer;
hbogenmenge = bogenmenge;
while ((gefunden == 0)&&(hbogenmenge != NULL))
  {
        if ((hbogenmenge->anfang == knoten)&&(hbogenmenge->ende == f1knoten))
          {
            gefunden = 1;
            cap = hbogenmenge->cap - hbogenmenge->flow;
          }  /*  if  */
    hbogenmenge = hbogenmenge->next;
  }  /*  while  */
return(cap);
}  /*  f1bogen  */





void kuerzeste_Wege(vor)   /*  Hilfsprocedur von Min_Cost_Flow  */
/*  Es wird ein Pfad von s nach t gesucht, der minimale Kosten aufweist,
    und auf dem man den Fluss erhoehen kann.  */
{ 
void behandle_knoten();
void behandle_nachfolger();
int suche_minknoten();
TNETZKNOTENMENGE *hnetzknotenmenge;
int knoten;
int i = 1;
while (i < netzknotenanz)
  {
    knoten = suche_minknoten();
    if (knoten != netzknotenanz)
         behandle_nachfolger(knoten,vor);
    i++;
 }  /* while */
hnetzknotenmenge = netzknotenmenge;
while (hnetzknotenmenge != NULL)
  {
   hnetzknotenmenge->perm = 0;
   hnetzknotenmenge = hnetzknotenmenge->next;
  }
return;
}  /*  kuerzeste_Wege  */

 

void augmentiere()   /*  Hilfsprocedur von Min_Cost_Flow  */
/*  Laengs eines markierten Pfades wird der bestehende Fluss
    um den Wert 1 erhoeht.  */
{
TNETZKNOTENMENGE *hnetzknotenmenge;
TBOGENMENGE *hbogenmenge;
int vor,nach,gefunden,kosten,rueck;
vor = netzknotenanz;
nach = 0;
while (nach != 1)
  {
    nach = vor;
    if (nach < 0) 
      {
        nach = -nach;
        rueck = 1;
      }  /*  if  */
      else rueck = 0;
    if (nach != 1)
      {
        hnetzknotenmenge = netzknotenmenge;
        while (hnetzknotenmenge->nummer != nach)
          hnetzknotenmenge = hnetzknotenmenge->next;
        vor = hnetzknotenmenge->vor;
        hnetzknotenmenge->perm = 1;
        if (hnetzknotenmenge->typ == 'v')
          {
            if (rueck == 0)
              hnetzknotenmenge->vorrat--;
             else hnetzknotenmenge->vorrat++;
          }
        hbogenmenge = bogenmenge;
        gefunden = 0;
        while (gefunden == 0)
          { 
           if (vor > 0)
            {
              if ((hbogenmenge->anfang == vor)&&(hbogenmenge->ende == nach))
                {
                  gefunden = 1;
                  hbogenmenge->flow++;
                  kosten = hbogenmenge->flow * hbogenmenge->costen;
                  Kosten += kosten;
                }  /*  if  */
               else hbogenmenge = hbogenmenge->next;
            }  /*  if vor > 0  */
           else if (vor < 0)
            {
              if ((hbogenmenge->anfang == nach)&&(hbogenmenge->ende == -vor))
                {
                  gefunden = 1;
                  hbogenmenge->flow--;
                  kosten = hbogenmenge->flow * hbogenmenge->costen;
                  Kosten -= kosten;
                }  /*  if  */
               else hbogenmenge = hbogenmenge->next;
            }  /*  vor < 0  */
          }  /*  while  */
       }  /*  if  */
  }  /* while  */
}  /*  augmentiere  */




int suche_minknoten()   /*  Hilfsprocedur von kuerzeste_Wege  */
/*  Liefert die Nummer eines Knoten mit perm = 0 und unter diesen
    einen mit minimalen Kosten.   */
{
TNETZKNOTENMENGE *hnetzknotenmenge;
int kosten = maxcost + 1;
int knoten = 0; /*  Wenn knoten = 0 bleibt,so gab es keinen Knoten mehr
                    mit perm = 0; also ist kein Fluss moeglich.
                    In diesem Fall ist wahrscheinlich der Graph nicht planar. */
hnetzknotenmenge = netzknotenmenge;
while (hnetzknotenmenge != NULL)
  {
    if (hnetzknotenmenge->perm == 0)
      {
        if (hnetzknotenmenge->cost < kosten)
          {
            kosten = hnetzknotenmenge->cost;
            knoten = hnetzknotenmenge->nummer;
          }  /*  if  */
      }  /*  if  */
    hnetzknotenmenge = hnetzknotenmenge->next;
  }  /*  while  */
return(knoten);
}   /*  suche_minknoten  */



void behandle_nachfolger(knoten,vor) 
  /*  Hilfsprocedur von kuerzeste_Wege  */
/*  Bei allen Nachfolgern von knoten werden u. U. die Werte cost,vor
    und menge geaendert.  */
{
TBOGENMENGE *hbogenmenge;
TNETZKNOTENMENGE *hnetzknotenmenge;
int zielknoten,costs,cap,kosten,f1knoten;
hbogenmenge = bogenmenge;
hnetzknotenmenge = netzknotenmenge;
while ((hnetzknotenmenge->typ != 'f')||(hnetzknotenmenge->herkunft != 1))
   hnetzknotenmenge = hnetzknotenmenge->next;
f1knoten = hnetzknotenmenge->nummer;
hnetzknotenmenge = netzknotenmenge;
while (hnetzknotenmenge->nummer != knoten)
   hnetzknotenmenge = hnetzknotenmenge->next;
kosten = hnetzknotenmenge->cost;
hnetzknotenmenge->perm = 1;
while (hbogenmenge != NULL)
  {
    if (hbogenmenge->anfang == knoten)
      {
        zielknoten = hbogenmenge->ende;
        costs = hbogenmenge->costen;
        cap = hbogenmenge->cap - hbogenmenge->flow;
        if ((hbogenmenge->typ == 'v')&&(hbogenmenge->flow == 0)&&
            (hbogenmenge->ende == f1knoten))
          costs -= 20;
        if ((hbogenmenge->ende != f1knoten)&&
            (hbogenmenge->typ == 'v')&&(hbogenmenge->flow == 1))
           costs += 20;
        if ((vor == 1)&&((hbogenmenge->fixed - hbogenmenge->flow) > 0)
            &&(hbogenmenge->fixed > -1))
           costs -= 500000;
        if ((hbogenmenge->fixed > -1)&&
            (hbogenmenge->flow == hbogenmenge->fixed))
              costs += 500000;
        hnetzknotenmenge = netzknotenmenge;
        while ( hnetzknotenmenge->nummer != zielknoten)
          hnetzknotenmenge = hnetzknotenmenge->next;
        if ((costs + kosten) < hnetzknotenmenge->cost)
          {
            if (cap > 0)
              {
                hnetzknotenmenge->cost = (costs + kosten);
                hnetzknotenmenge->vor = knoten;
                hnetzknotenmenge->menge = 1;
              }  /*  if  */
          }  /*  if  */
      }  /*  if anfang == knoten */
     else if(hbogenmenge->ende == knoten)
      {
        zielknoten = hbogenmenge->anfang;
        costs = hbogenmenge->costen;
        cap = hbogenmenge->flow;
        if ((hbogenmenge->typ == 'v')&&(cap == 1)&&
            (hbogenmenge->ende == f1knoten))
          costs -= 20;
        if ((vor == 1)&&(hbogenmenge->fixed > -1))
          costs -= 500000;
        hnetzknotenmenge = netzknotenmenge;
        while ( hnetzknotenmenge->nummer != zielknoten)
          hnetzknotenmenge = hnetzknotenmenge->next;
        if ((kosten - costs) < hnetzknotenmenge->cost)
          {
            if (cap > 0)
              {
                hnetzknotenmenge->cost = (kosten - costs);
                hnetzknotenmenge->vor = -knoten;
                hnetzknotenmenge->menge = 1;
              }  /*  if  */
          }  /*  if  */
      }  /*  ende == knoten  */
    hbogenmenge = hbogenmenge->next;
  }  /*  while  */
return;
}   /*  behandle_nachfolger  */





/******************************************************************************
******************************************************************************/


void orthogonalisiere()  /*  Bestimmt die orthogonale Darstellung aus dem Fluss
                          */
{
void konstruiere_a();
void konstruiere_s();
konstruiere_a();
konstruiere_s();
return;
}  /*  orthogonalisiere  */




void konstruiere_a()  /*  Hilfsprocedur von orthogonalisiere  */
/*  Berechnet die a-Felder der orthogonalen Darstellung  */
{
int suche_facenummer();
int ermittle_knotenname();
TFACEMENGE *hfacemenge;
TKNOTENMENGE *hknotenmenge;
TGRAPH *hgraph;
TBOGENMENGE *hbogenmenge;
TFACE *hface;
TKANTENTRIPEL *kanten,*hkanten,*htripel,*nextkanten;
int gefunden,found,anfang,ende,knoten,facenummer;
int zahl1,zahl2,grad1,grad2;
hgraph = graph;
while (hgraph != NULL)
  {
    hface = hgraph->flaeche;
    htripel = hface->face;
    while (htripel != NULL)
     {
       htripel->a = 90;
       htripel = htripel->next;
     }  /*  while  */
    hgraph = hgraph->next;
  }  /*  while  */
/*  Ende der Initialisierung; Sinn: Wo keine Boegen existieren, ist der
    Fluss = 0; dies wird dadurch simuliert,dass alle a-Felder auf 90
    gesetzt werden.  */
/*  Nun beginnt die Behandlung der Knoten von Grad 1:  */
hbogenmenge = bogenmenge;
while (hbogenmenge != NULL)
  {
    if ((hbogenmenge->typ == 'v')&&(hbogenmenge->flow == 3))
      {
         knoten = ermittle_knotenname(hbogenmenge->anfang);
         facenummer = suche_facenummer(hbogenmenge->ende);
         gefunden = 0;
         hgraph = graph;
         while (gefunden == 0)
           {
             hface = hgraph->flaeche;
             if (hface->nummer == facenummer)
               gefunden = 1;
              else hgraph = hgraph->next;
           }  /*  gefunden == 0  */
         kanten = hface->face;
         gefunden = 0;
         while (gefunden == 0)
           {
             if ((knoten != kanten->anfang)&&(knoten != kanten->ende))
               {
                  if (kanten->next != NULL)
                    kanten = kanten->next;
                   else kanten = hface->face;
               }
              else
               {
                  if (kanten->next != NULL)
                    nextkanten = kanten->next;
                   else nextkanten = hface->face;
                  if ((knoten != nextkanten->anfang)
                    &&(knoten != nextkanten->ende))
                    {
                      if (kanten->next != NULL)
                        kanten = kanten->next;
                       else kanten = hface->face;
                    }
                   else
                    {
                      gefunden = 1;
                      kanten->a = 360;
                    }
               }
           }  /*  gefunden == 0  */
      }  /*  if  */
    hbogenmenge = hbogenmenge->next;
  }  /*  while */
/*  Ende der Behandlung der Knoten von Grad 1.  */
hbogenmenge = bogenmenge;
while (hbogenmenge != NULL)
  {
    if (hbogenmenge->typ == 'v')
      {
        found = 0;
        hgraph = graph;
        gefunden = 0;
        facenummer = suche_facenummer(hbogenmenge->ende);
        while (gefunden == 0)
          {
            hface = hgraph->flaeche;
            if (hface->nummer == facenummer)
              gefunden = 1;
              else hgraph = hgraph->next;
          }  /*  while  */
        knoten = ermittle_knotenname(hbogenmenge->anfang);
        kanten = hface->face;
        hkanten = kanten;
        while (kanten->next != NULL)
         {
          if (kanten->a == 90)
          {
           anfang = kanten->anfang;
           ende = kanten->ende;
           if ((anfang == knoten)||(ende == knoten))
             {
               anfang = kanten->next->anfang;
               ende = kanten->next->ende;
               if ((anfang == knoten)||(ende == knoten))
                 {
                 if (found == 0)
                   kanten->a = (hbogenmenge->flow + 1)* 90;
                   else kanten->a = 90;
                 found = 1;
                 }  /*  if  */
             }  /*  if  */
          }  /*  if  */
           kanten = kanten->next;
         }  /*  while  */
        /*  Nun folgt der Vergleich der letzten mit der ersten Kante der
            Liste.   */
       if (kanten->a == 90)
       {
        anfang = kanten->anfang;
        ende = kanten->ende;
        if ((anfang == knoten)||(ende == knoten))
          {
            anfang = hkanten->anfang;
            ende = hkanten->ende;
            if ((anfang == knoten)||(ende == knoten))
              {
                if (found == 0)
                  kanten->a = (hbogenmenge->flow + 1)* 90;
                  else kanten->a = 90;
              }  /*  if  */
          }  /*  if  */
       }  /*  if  */
      }  /*  if  */
    hbogenmenge = hbogenmenge->next;
  }  /*  while  */
hfacemenge = facemenge;
while (hfacemenge->nummer != 1)
   hfacemenge = hfacemenge->next;
if (hfacemenge->grad == 2)
  {
    zahl1 = graph->flaeche->face->a;
    zahl2 = graph->flaeche->face->next->a;
    if (zahl1 != zahl2)
      {
         hknotenmenge = knotenmenge;
         while (hknotenmenge->nummer != graph->flaeche->face->ende)
           hknotenmenge = hknotenmenge->next;
         grad1 = hknotenmenge->grad;
         hknotenmenge = knotenmenge;
         while (hknotenmenge->nummer != graph->flaeche->face->next->ende)
           hknotenmenge = hknotenmenge->next;
         grad2 = hknotenmenge->grad;
         if ((zahl1 == 90)||(zahl2 == 90))
           {
             if (grad1 == 4)
               {
                 graph->flaeche->face->a = 90;
                 if (zahl1 != 90)
                   graph->flaeche->face->next->a = zahl1;
                  else graph->flaeche->face->next->a = zahl2;
               }
              else
               {
                 graph->flaeche->face->next->a = 90;
                 if (zahl1 != 90)
                   graph->flaeche->face->a = zahl1;
                  else graph->flaeche->face->a = zahl2;
               }
           }
          else  /*  kein 90 Grad Winkel dabei  */
           {
             if (grad1 == 3)
               {
                 graph->flaeche->face->a = 180;
                 if (zahl1 != 180)
                   graph->flaeche->face->next->a = zahl1;
                  else graph->flaeche->face->next->a = zahl2;
               }
              else
               {
                 graph->flaeche->face->next->a = 180;
                 if (zahl1 != 180)
                   graph->flaeche->face->a = zahl1;
                  else graph->flaeche->face->a = zahl2;
               }
           }
      }  /*  zahl1 != zahl2  */
  }  /*  if grad = 2  */
}  /*  konstruiere_a  */





int suche_facenummer(zahl)  /*  Hilfsprocedur von konstruiere_a  */
/*  Liefert die Nummer des Faces,dessen Netzknotennummer zahl ist.  */
{
TNETZKNOTENMENGE *hnetzknotenmenge;
int ergebnis;
int gefunden = 0;
hnetzknotenmenge = netzknotenmenge;
while (gefunden == 0)
  {
    if (hnetzknotenmenge->nummer == zahl)
     {
      gefunden = 1;
      ergebnis = hnetzknotenmenge->herkunft;
     }  /*  if  */
    hnetzknotenmenge = hnetzknotenmenge->next;
  }  /*  while  */
return(ergebnis);
}  /*  suche_facenummer  */




int ermittle_knotenname(nummer)  /* Hilfsprocedur von konstruiere_a */
/*  Liefert aus netzknotenmenge die Herkunft des Knotens,dessen Nummer
    nummer ist.  */
{
TNETZKNOTENMENGE *hnetzknotenmenge;
int herkunft;
int gefunden = 0;
hnetzknotenmenge = netzknotenmenge;
while (gefunden == 0)
  {
   if (hnetzknotenmenge->nummer == nummer)
      {
        gefunden = 1;
        herkunft = hnetzknotenmenge->herkunft;
      }
     else hnetzknotenmenge = hnetzknotenmenge->next;
  }
return(herkunft);
}  /*  ermittle_knotenname  */





void konstruiere_s()  /*  Hilfsprocedur von orthogonalisiere  */
/*  Berechnet die s-Felder der orthogonalen Darstellung  */
{
int suche_facenummer();
TBOGENMENGE *hbogenmenge;
TGRAPH *hgraph1,*hgraph2;
int fluss,facenummer1,facenummer2,gefunden1,gefunden2,found;
TFACE *face1,*face2;
TKANTENTRIPEL *tripel1,*tripel2;
hgraph1 = graph;
while (hgraph1 != NULL)
  {
    tripel1 = hgraph1->flaeche->face;
    while (tripel1 != NULL)
      {
         tripel1->s = 0;
         tripel1 = tripel1->next;
      }  /*  while  */
    hgraph1 = hgraph1->next;
  }  /*  while  */
hbogenmenge = bogenmenge;
while (hbogenmenge != NULL)
  {
    if (hbogenmenge->typ == 'f')
      {
        fluss = hbogenmenge->flow;
        if (fluss > 0)
          {
             facenummer1 = suche_facenummer(hbogenmenge->anfang);
             facenummer2 = suche_facenummer(hbogenmenge->ende);
             hgraph1 = graph;
             gefunden1 = 0;
             while (gefunden1 == 0)
                {
                   face1 = hgraph1->flaeche;
                   if (face1->nummer == facenummer1)
                      gefunden1 = 1;
                     else hgraph1 = hgraph1->next;
                }  /*  while  */
             hgraph2 = graph;
             gefunden2 = 0;
             while (gefunden2 == 0)
               {
                  face2 = hgraph2->flaeche;
                  if (face2->nummer == facenummer2)
                      gefunden2 = 1;
                     else hgraph2 = hgraph2->next;
               }  /*  while  */
             face1 = hgraph1->flaeche;
             tripel1 = face1->face;
             found = 0;
             while ((tripel1 != NULL)&&(found == 0))
               {
                 face2 = hgraph2->flaeche;
                 tripel2 = face2->face;
                 while ((tripel2 != NULL)&&(found == 0))
                    {
                       if (tripel1->name == tripel2->name)
                         {
                            found = 1;
                            tripel1->s -= fluss;
                            tripel2->s += fluss;
                         }  /*  if  */
                        else tripel2 = tripel2->next;
                    }  /*  while  */
                 tripel1 = tripel1->next;
               }  /*  while  */
          }  /*  if  */
      }  /*  if  */
    hbogenmenge = hbogenmenge->next;
  }  /*  while  */
}  /*   konstruiere_s  */






int modify(alt,zuweisung)  /*  Hilfsprozedur von optimieren und voroptimieren */
  /*  Berechnet wird der Wert der Wahrscheinlichkeit fuer einen 90 Grad -
      Winkel bei Zuweisung des Winkels zuweisung und einem alten Wert von alt.*/

{
int ergebnis;
if (zuweisung == 90)
  {
    if (alt == 100)
      ergebnis = 100;
     else if (alt == 67)
      ergebnis = 50;
     else if (alt == 33)
      ergebnis = -1;
     else if (alt == 50)
      ergebnis = -2;
  }  /*  zuweisung == 90  */
 else if (zuweisung == 180)
  {
    if (alt == 67)
      ergebnis = 100;
     else if (alt == 33)
      ergebnis = -2;
     else if (alt == 50)
      ergebnis = 100;
  }  /*  zuweisung == 180  */
 else if (zuweisung == 270)
   if (alt == 33)
      ergebnis = 100;
return(ergebnis);
}  /*  modify  */






int suche_anderes_face(kante,facename)    /*  Hilfsprocedur von optimieren  */
/*  bestimmt die Nummer des  Faces ungleich facename, zu dem die Kante 
    kante gehoert. */
{
TGRAPH *hgraph;
TKANTENTRIPEL *hface;
int ergebnis = 0;
int gefunden = 0;
hgraph = graph;
while ((gefunden == 0)&&(hgraph != NULL))
  {
     if (hgraph->flaeche->nummer == facename)
       hgraph = hgraph->next;
     if (hgraph != NULL)
       {
         hface = hgraph->flaeche->face;
         while ((hface != NULL)&&(gefunden == 0))
           {
              if (hface->name == kante)
                {
                   gefunden = 1;
                   ergebnis = hgraph->flaeche->nummer;
                }   /*  if  */
              else hface = hface->next;
           }  /*  while  */
         hgraph = hgraph->next;
       }  /*  if hgraph != NULL  */
  }  /*  while  */
if (ergebnis == 0)
  ergebnis = facename;
return(ergebnis);
}  /*  suche_anderes_face  */







int voroptimieren()  /*  Hilfsprocedur von berechne_orthogonale_darstellung  */
/*  Alle internen Segmente mit genau vier 90-Grad Winkeln werden behandelt.
    Zurueckgegeben wird 1, wenn eine Aenderung vorgenommen wurde. */
{
TGRAPH *hgraph,*refgraph;
TKANTENTRIPEL *hface,*refface,*anderer1,*anderer2,*gegen1,*gegen2;
TKANTENTRIPEL *fix1,*fix2,*fix3,*krit,*kritl,*kritr;
TFACEMENGE *hfacemenge;
TBOGENMENGE *hbogenmenge;
int suche_faceknoten();
int suche_knotenknoten();
int name1,name2,name3,name4,knotenname1,knotenname2,knotenname3,knotenname4;
int i,z,facename,anfang,anfang2,kost,zaehler1;
int geaendert,neukost,neuname1,neuname2,neuname3,neuname4;
int zaehler,kost1,kost2,dir,d1,d2,d3,grad,mini,maxi,minim;
int fixiert;
geaendert = 0;
hfacemenge = facemenge;
while (hfacemenge != NULL)
  {
    if ((hfacemenge->grad < 5)&&(hfacemenge->behandelt == 0))
      {
         /* modifizieren(hfacemenge->nummer); */
         hfacemenge->behandelt = 1;
      }
    hfacemenge = hfacemenge->next;
  }  /*  while  */
hfacemenge = facemenge;
while ((hfacemenge != NULL)&&(geaendert == 0)) 
  {
    if (hfacemenge->behandelt == 0)
      {
        hfacemenge->behandelt = 1;
        hgraph = graph;
        while (hgraph->flaeche->nummer != hfacemenge->nummer)
          hgraph = hgraph->next;
        if (hgraph->flaeche->nummer != 1)
          {
             hface = hgraph->flaeche->face;
             zaehler = 0;
             while (hface != NULL)
               {
                 if (hface->a == 90)
                    zaehler++;
                   else if (hface->a != 180)
                    zaehler = 1000;
                 hface = hface->next;
               }
             if (zaehler == 4)
               {
                  hfacemenge->behandelt = 1;
                  hface = hgraph->flaeche->face;
                  fixiert = 0;
                  while (hface != NULL)
                    {
                      if (hface->p == 100)
                        fixiert++;
                      hface = hface->next;
                    }
                  if (fixiert == 0)
                    {
                      geaendert = 1;
                      zaehler1 = 0;
                      hface = hgraph->flaeche->face;
                      anfang = ((hfacemenge->grad - 2)/2);
                      kost = -1;
                      neukost = 100;
                      while (hface != NULL)
                            /*  Mit Hilfe von kost wird die geeignetste 
                                Stelle fuer die 90-Grad- Winkel gesucht.  */
                        {
                          zaehler1++;
                          neukost = 0;
                          neuname1 = hface->ende;
                          if (hface->p > 0)
                            neukost += hface->p;
                          if (hface->next != NULL)
                             hface = hface->next;
                            else hface = hgraph->flaeche->face;
                          neuname2 = hface->ende;
                          if (hface->p > 0)
                            neukost += hface->p;
                          for (i=1;i<=anfang;i++)
                            {
                              if (hface->next != NULL)
                                hface = hface->next;
                               else hface = hgraph->flaeche->face;
                            }
                          neuname3 = hface->ende;
                          if (hface->p > 0)
                            neukost += hface->p;
                          if (hface->next != NULL)
                             hface = hface->next;
                            else hface = hgraph->flaeche->face;
                          neuname4 = hface->ende;
                          if (hface->p > 0)
                            neukost += hface->p;
                          if (neukost > kost)
                            {
                                kost = neukost;
                                name1 = neuname1;
                                name2 = neuname2;
                                name3 = neuname3;
                                name4 = neuname4;
                            }
                          hface = hgraph->flaeche->face;
                          for (i=1;i<=zaehler1;i++)
                            hface = hface->next;
                        }  /*  while  */
                    }  /*  fixiert == 0  */
                   else if (fixiert == 1)
                    {
                      geaendert = 1;
                      hface = hgraph->flaeche->face;
                      anfang = ((hfacemenge->grad - 2)/2);
                      if (hface->p == 100)
                        {
                          anderer1 = hface->next;
                          anderer2 = hface;
                          while (anderer2->next != NULL)
                            anderer2 = anderer2->next;
                        }
                        else
                        {
                          anderer2 = hface;
                          while (hface->p != 100)
                            {
                               anderer2 = anderer2->next;
                               hface = hface->next;
                            }
                          if (hface->next != NULL)
                            anderer1 = hface->next;
                           else anderer1 = hgraph->flaeche->face;
                        }
                      name1 = hface->ende;
                      kost1 = 0;
                      if (anderer1->p > 0)
                        kost1 += anderer1->p;
                      kost2 = 0;
                      if (anderer2->p > 0)
                        kost2 += anderer2->p;
                      hface = anderer1;
                      for (i=1;i<=anfang;i++)
                        {
                          if (hface->next != NULL)
                            hface = hface->next;
                           else hface = hgraph->flaeche->face;
                        }
                      gegen1 = hface;
                      if (hface->p > 0)
                        kost1 += hface->p;
                      if (hface->next != NULL)
                        hface = hface->next;
                       else hface = hgraph->flaeche->face;
                      name2 = hface->ende;
                      if (hface->next != NULL)
                        hface = hface->next;
                       else hface = hgraph->flaeche->face;
                      if (hface->p > 0)
                        kost2 += hface->p;
                      gegen2 = hface;
                      if (kost1 < kost2)
                        {
                          name3 = anderer1->ende;
                          name4 = gegen1->ende;
                        }
                       else
                        {
                          name3 = anderer2->ende;
                          name4 = gegen2->ende;
                        }
                    }  /*  fixiert == 1  */
                   else if (fixiert == 2)
                    {
                      geaendert = 1;
                      hface = hgraph->flaeche->face;
                      grad = hfacemenge->grad;
                      while (hface->p != 100)
                        hface = hface->next;
                      fix1 = hface;
                      hface = hface->next;
                      d1 = 0;
                      while (hface->p != 100)
                        {
                           hface = hface->next;
                           d1++;
                        }
                      fix2 = hface;
                      if (hface->next != NULL)
                        hface = hface->next;
                        else hface = hgraph->flaeche->face;
                      d2 = 0;
                      while (hface->p != 100)
                        {
                          if (hface->next != NULL)
                            hface = hface->next;
                           else hface = hgraph->flaeche->face;
                          d2++;
                        }
                      if (d1 < d2)
                        {
                          mini = d1;
                          maxi = d2;
                        }
                        else
                        {
                          mini = d2;
                          maxi = d1;
                        }
                      if (mini <= (grad/4))  /*  Betrachte fix1 und fix2 als
                                                zusammen  */
                        {
                           anfang = ((grad - ((mini + 1)*2))/2);
                           anfang2 = (grad - (2 * anfang) - (mini + 1));
                           if (mini == d1)
                             hface = fix2;
                             else hface = fix1;
                           for (i=1;i<=anfang;i++)
                             {
                               if (hface->next != NULL)
                                 hface = hface->next;
                                 else hface = hgraph->flaeche->face;
                             }
                           name1 = hface->ende;
                           for (i=1;i<=anfang2;i++)
                             {
                              if (hface->next != NULL)
                                hface = hface->next;
                               else hface = hgraph->flaeche->face;
                             }
                           name2 = hface->ende;
                        }
                       else  /*  Betrachte fix1 und fix2 als auseinander  */
                        {
                          if (mini != maxi)
                            {
                              if (mini == d1)
                                {
                                   kritl = fix1;
                                   kritr = fix2;
                                }
                                else
                                {
                                   kritl = fix2;
                                   kritr = fix1;
                                }
                              if (kritr->next != NULL)
                                 name1 = kritr->next->ende;
                                else name1 = hgraph->flaeche->face->ende;
                              hface = hgraph->flaeche->face;
                              if (kritl == hface)
                                {
                                   while (hface->next != NULL)
                                     hface = hface->next;
                                   name2 = hface->ende;
                                }
                               else
                                {
                                  while (hface->next != kritl)
                                    hface = hface->next;
                                  name2 = hface->ende;
                                }
                            }
                           else  /*  mini == maxi  */
                            {
                               anderer1 = fix1->next;
                               if (fix2->next != NULL)
                                 gegen1 = fix2->next;
                                else gegen1 = hgraph->flaeche->face;
                               hface = hgraph->flaeche->face;
                               if (fix1 == hface)
                                 {
                                   while (hface->next != NULL)
                                     hface = hface->next;
                                   anderer2 = hface;
                                 }
                                else
                                 {
                                   while (hface->next != fix1)
                                     hface = hface->next;
                                   anderer2 = hface;
                                 }
                               hface = hgraph->flaeche->face;
                               while (hface->next != fix2)
                                 hface = hface->next;
                               gegen2 = hface;
                               kost1 = 0;
                               kost2 = 0;
                               if (anderer1->p > 0)
                                  kost1 += anderer1->p;
                               if (anderer2->p > 0)
                                  kost2 += anderer2->p;
                               if (gegen1->p > 0)
                                  kost1 += gegen1->p;
                               if (gegen2->p > 0)
                                  kost2 += gegen2->p;
                               if (kost1 < kost2)
                                 {
                                   name1 = anderer1->ende;
                                   name2 = gegen1->ende;
                                 }
                                else
                                 {
                                   name1 = anderer2->ende;
                                   name2 = gegen2->ende;
                                 }
                            }
                        }
                      name3 = fix1->ende;
                      name4 = fix2->ende;
                    }  /*  fixiert == 2  */
                   else if (fixiert == 3)
                    {
                      geaendert = 1;
                      hface = hgraph->flaeche->face;
                      while (hface->p != 100)
                        hface = hface->next;
                      fix1 = hface;
                      d1 = 0;
                      hface = hface->next;
                      while (hface->p != 100)
                        {
                          hface = hface->next;
                          d1++;
                        }
                      fix2 = hface;
                      d2 = 0;
                      hface = hface->next;
                      while (hface->p != 100)
                        {
                          hface = hface->next;
                          d2++;
                        }
                      fix3 = hface;
                      if (hface->next != NULL)
                        hface = hface->next;
                        else hface = hgraph->flaeche->face;
                      d3 = 0;
                      while (hface->p != 100)
                        {
                           if (hface->next != NULL)
                              hface = hface->next;
                             else hface = hgraph->flaeche->face;
                           d3++;
                        }
                      name1 = fix1->ende;
                      name2 = fix2->ende;
                      name3 = fix3->ende;
                      if (d1 > d2)
                        {
                          if (d1 > d3)
                            {
                              if (d3 > d2)
                                {
                                   krit = fix1;
                                   dir = 1;
                                }
                               else
                                {
                                   krit = fix2;
                                   dir = -1;
                                }
                            }
                            else
                            {
                              krit = fix1;
                              dir = -1;
                            }
                        }
                       else  /*  d2 > d1  */
                        {
                           if (d2 > d3)
                             {
                                if (d3 > d1)
                                   {
                                     krit = fix3;
                                     dir = -1;
                                   }
                                 else
                                   {
                                     krit = fix2;
                                     dir = 1;
                                   }
                             }
                            else
                             {
                               krit = fix3;
                               dir = 1;
                             }
                        }
                       minim = d1;
                       if (d2 < minim)
                         minim = d2;
                       if (d3 < minim)
                         minim = d3;
                       if (dir == 1)
                          {
                            for (i=1;i <= (minim + 1);i++)
                              {
                                if (krit->next != NULL)
                                  krit = krit->next;
                                 else krit = hgraph->flaeche->face;
                              }
                            name4 = krit->ende;
                          }
                         else
                          {
                            for (i=1;i <= (minim + 1);i++)
                              {
                                hface = hgraph->flaeche->face;
                                if (hface == krit)
                                  {
                                    while (hface->next != NULL)
                                       hface = hface->next;
                                    krit = hface;
                                  }
                                 else
                                  {
                                    while (hface->next != krit)
                                      hface = hface->next;
                                    krit = hface;
                                  }
                              }
                            name4 = krit->ende;
                          }
                    }  /*  fixiert == 3  */
                   else  /*  fixiert == 4  */
                    {
                    }  /*  fixiert == 4  */
                  if (geaendert == 1)
                    {
                       facename = suche_faceknoten(hfacemenge->nummer);
                       knotenname1 = suche_knotenknoten(name1);
                       knotenname2 = suche_knotenknoten(name2);
                       knotenname3 = suche_knotenknoten(name3);
                       knotenname4 = suche_knotenknoten(name4);
                       hbogenmenge = bogenmenge;
                       z = 0;
                       while ((hbogenmenge != NULL)&&(z < 4 ))
                         {
                           if (((hbogenmenge->anfang == knotenname1)||
                              (hbogenmenge->anfang == knotenname2)||
                              (hbogenmenge->anfang == knotenname3)||
                              (hbogenmenge->anfang == knotenname4))&&
                              (hbogenmenge->ende == facename))
                             {
                               z++;
                               hbogenmenge->costen+= 10;
                             }
                           hbogenmenge = hbogenmenge->next;
                         }  /*  while  */
                    }  /*  if geaendert == 1  */
               }  /*  if zaehler == 4  */
          }  /*  if intern  */
      }  /*  if  */
    if (geaendert == 0)
      hfacemenge = hfacemenge->next;
  }  /*  while  */
if (geaendert == 0)
  return(geaendert);
 else return(hfacemenge->nummer);
}  /*  voroptimieren  */




int optimieren()
/*  Hilfsprocedur von berechne_orthogonale_darstellung  */
/*  Alle internen Segmente  werden behandelt.
    Zurueckgegeben wird die Nummer des behandelten Segments.  */
{
int suche_faceknoten();
int suche_knotenknoten();
int modify();
TGRAPH *hgraph,*suchgraph,*hhgraph,*refgraph;
TKANTENTRIPEL *hface,*suchface,*anfang,*suchanfang,*hilfface,*hhface,*merke;
TKANTENTRIPEL *letzter,*refface,*neumerke;
TFACEMENGE *hfacemenge,*suchfacemenge,*hhfacemenge,*hilffacemenge;
TBOGENMENGE *hbogenmenge;
int gefunden,h,gem,suchgem,sgefunden,facenummer,unterbrechung,uebergang,suchh;
int fixa,fixb,merk,kuerzlaenge,aktuellaenge,vert,horiz,l,richtung;
int z,knoten1,knoten2,knoten3,knotenname1,knotenname2,name1,name2;
int knotenname3,i,facename,ges1,ges2,ges3,ges4;
int anfangrichtung,maxi,dir,dir2,fix1,fix2,maxdir,fix,k,l1,l2,modus;
int gesamt,gegenmax,soll,kritdir,krit,diff,zaehler,zaehler1_90,zaehle;
int zaehler2_90,zaehlerkrit_90,zaehler180,zaehler270,zaehl90,versuch;
int hilfzaehler,gegendir,gefundenmax,gefundengegen,aendere,zaehler90,rest;
int verwende,hdir,manipuliere,merkzaehler,veraendert,diffzaehler,kritzaehler;
int hzaehler,hzaehler180,facenummer1,dirmax,dirz,kritfacenummer,ok,name;
int geaendert = 0;
hfacemenge = facemenge;
while ((hfacemenge != NULL)&&(geaendert == 0))
  {
    if ((hfacemenge->grad >= 6)&&(hfacemenge->behandelt == 0)&&
        (hfacemenge->nummer != 1))
      {
        hgraph = graph;
        while (hgraph->flaeche->nummer != hfacemenge->nummer)
          hgraph = hgraph->next;
        hface = hgraph->flaeche->face;
        ok = 1;
        while ((hface != NULL)&&(ok == 1))
          {
             if (hface->a == 270)
               {
                 hhgraph = graph;
                 hhface = hhgraph->flaeche->face;
                 gefunden = 0;
                 while ((gefunden == 0)&&(hhface != NULL))
                   {
                     if ((hhface->ende == hface->ende)&&
                         (hhgraph->flaeche->nummer != hgraph->flaeche->nummer))
                       {
                         gefunden = 1;
                         name = hhgraph->flaeche->nummer;
                       }
                     if (hhface->next != NULL)
                       hhface = hhface->next;
                      else if (hhgraph->next != NULL)
                        {
                          hhgraph = hhgraph->next;
                          hhface = hhgraph->flaeche->face;
                        }
                       else hhface = NULL;
                   }  /*  while  */
                 if (gefunden == 1)
                   {
                     hhfacemenge = facemenge;
                     while (hhfacemenge->nummer != name)
                       hhfacemenge = hhfacemenge->next;
                     if (hhfacemenge->behandelt == 0)
                       ok = 0;
                   }
               }  /*  if hface->a == 270  */
             hface = hface->next;
          }  /*  while ok  */
        if (ok == 1)
           {
              geaendert = 1;
              hfacemenge->behandelt = 1;
              hface = hgraph->flaeche->face;
              while (hface->next != NULL)
                hface = hface->next;
              letzter = hface;
              hface = hgraph->flaeche->face;
              while (hface != NULL)
                {
                  facenummer = 
                    suche_anderes_face(hface->name,hgraph->flaeche->nummer);
                  hhfacemenge = facemenge;
                  gefunden = 0;
                  while (gefunden == 0)
                    {
                      if (hhfacemenge->nummer == facenummer)
                        gefunden = 1;
                       else hhfacemenge = hhfacemenge->next;
                    }
                  if (hface->next != NULL)
                     hhface = hface->next;
                    else hhface = hgraph->flaeche->face;
                  facenummer1 = 
                    suche_anderes_face(hhface->name,hgraph->flaeche->nummer);
                  hilffacemenge = facemenge;
                  gefunden = 0;
                  while (gefunden == 0)
                    {
                      if (hilffacemenge->nummer == facenummer1)
                        gefunden = 1;
                       else hilffacemenge = hilffacemenge->next;
                    }
                  if ((hhfacemenge->behandelt == 1)
                      &&(hfacemenge->nummer != facenummer)||
                       (hilffacemenge->behandelt == 1)
                        &&(hfacemenge->nummer != facenummer1)||
                       (hface->bridge == 1))
                    {
                      if (hface->afix == 0)
                        {
                          refgraph = graph;
                          refface = refgraph->flaeche->face;
                          while (refface != NULL)
                            {
                               if ((refface->ende == hface->ende)&&
                                   (refface != hface))
                                  {
                                     hilffacemenge = facemenge;
                                     while (hilffacemenge->nummer != 
                                      refgraph->flaeche->nummer)
                                        hilffacemenge = hilffacemenge->next;
                                     if ((hilffacemenge->behandelt == 0)||
                                        (refgraph->flaeche->nummer == 
                                          hfacemenge->nummer))
                                     refface->p = modify(refface->p,hface->a);
                                  }
                               if (refface->next != NULL)
                                 refface = refface->next;
                                else
                                  {
                                     if (refgraph->next != NULL)
                                       {
                                          refgraph = refgraph->next;
                                          refface = refgraph->flaeche->face;
                                       }
                                     else refface = NULL;
                                  }
                            }  /*  while  */
                        }   /*  if  */
                      hface->afix = hface->a;
                      if (letzter->afix == 0)
                        {
                          refgraph = graph;
                          refface = refgraph->flaeche->face;
                          while (refface != NULL)
                            {
                               if ((refface->ende == letzter->ende)&&
                                   (refface != letzter))
                                  {
                                     hilffacemenge = facemenge;
                                     while (hilffacemenge->nummer != 
                                      refgraph->flaeche->nummer)
                                        hilffacemenge = hilffacemenge->next;
                                     if ((hilffacemenge->behandelt == 0)||
                                        (refgraph->flaeche->nummer == 
                                          hfacemenge->nummer))
                                     refface->p = modify(refface->p,letzter->a);
                                  }
                               if (refface->next != NULL)
                                 refface = refface->next;
                                else
                                  {
                                     if (refgraph->next != NULL)
                                       {
                                          refgraph = refgraph->next;
                                          refface = refgraph->flaeche->face;
                                       }
                                     else refface = NULL;
                                  }
                            }  /*  while  */
                        }   /*  if  */
                      letzter->afix = letzter->a;
                    }  /*  if  */
                  hface = hface->next;
                  if (letzter->next != NULL)
                    letzter = letzter->next;
                   else letzter = hgraph->flaeche->face;
                }  /*  while  */
              hface = hgraph->flaeche->face;
              while (hface != NULL)
                {
                  if (hface->p == 100)
                    hface->afix = 90;
                   else if (hface->p == 0)
                    hface->afix = 360;
                   else if (hface->p == -1)
                    hface->afix = 270;
                   else if (hface->p == -2)
                    hface->afix = 180;
                   hface = hface->next;
                }  /*  while  */
              unterbrechung = 0;
              uebergang = 0;
              h = 0;
              hface = hgraph->flaeche->face;
              anfang = hface;
              kritfacenummer = 
                suche_anderes_face(hface->name,hgraph->flaeche->nummer);
              while (uebergang == 0)
                {
                  facenummer = 
                    suche_anderes_face(hface->name,hgraph->flaeche->nummer);
                  suchanfang = hface;
                  suchh = 0;
                  gefunden = 1;
                  while (gefunden == 1)
                    {
                      gefunden = 0;
                      suchgraph = graph;
                      while (suchgraph->flaeche->nummer != facenummer)
                        suchgraph = suchgraph->next;
                      suchface = suchgraph->flaeche->face;
                      while ((gefunden == 0)&&(suchface != NULL)&&
                             (hface->afix == 0)&&(hface->bridge == 0)&&
                             (hface->name <= echtkantenzahl))
                        {
                          if (hface->name == suchface->name)
                            {
                              gefunden = 1;
                              suchh++;
                            }
                         else suchface = suchface->next;
                        }  /*  while  */
                      if (gefunden == 1)
                        {
                          if (hface->next != NULL)
                            hface = hface->next;
                           else
                            {
                              if (unterbrechung == 1)
                                {
                                   hface = hgraph->flaeche->face;
                                   uebergang = 1;
                                }
                               else  /*  hface ist in suchface enthalten  */
                                {
                                   gefunden = 0;
                                   suchh = hfacemenge->grad;
                                }
                            }  /*  else  */
                        }  /*  if gefunden  */
                       else
                        {
                          unterbrechung = 1;
                          if (hface->next != NULL)
                            hface = hface->next;
                           else
                             {
                                uebergang = 1;
                                hface = hgraph->flaeche->face;
                             }
                        }
                    }  /*  while  gefunden == 1  */
                  if (suchh > h)
                    {
                      h = suchh;
                      anfang = suchanfang;
                      kritfacenummer = facenummer;
                    }
                }  /*  while  uebergang == 0  */

           /*  Nun werden Stuecke der Laenge >= 2 auf den "kurzen" Seiten
               auf Laenge 1 verkuerzt.  */
              hface = hgraph->flaeche->face;
              ges1 = 0;
              ges2 = 0;
              ges3 = 0;
              ges4 = 0;
              richtung = 1;  /*  nach links  */
              while (hface != NULL)
                {
                  if (hface == anfang)
                    anfangrichtung = richtung;
                  if (richtung == 1)
                    ges1++;
                    else if (richtung == 2)
                     ges2++;
                     else if (richtung == 3)
                      ges3++;
                      else ges4++;
                  if (hface->a == 90)
                     richtung++;
                    else if (hface->a == 270)
                     richtung+= 3;
                  if (richtung > 4)
                    richtung-= 4;
                  hface = hface->next;
                }  /*  while  */
              maxi = ges1;
              maxdir = 1;
              if (ges2 > maxi)
                {
                  maxi = ges2;
                  maxdir = 2;
                }   /*  if  */
              if (ges3 > maxi)
                {
                  maxi = ges3;
                  maxdir = 3;
                }   /*  if  */
              if (ges4 > maxi)
                {
                  maxi = ges4;
                  maxdir = 4;
                }   /*  if  */
              gegendir = maxdir + 2;
              if (gegendir > 4)
                gegendir -= 4;
              veraendert = 1;
              while (veraendert == 1)
                {
                  veraendert = 0;
                  zaehler = 1;
                  merke = anfang;
                  if (anfang == hgraph->flaeche->face)
                    {
                      while (merke->next != NULL)
                        merke = merke->next;
                    }
                   else
                    {
                      while (merke->next != anfang)
                        if (merke->next != NULL)
                          merke = merke->next;
                         else merke = hgraph->flaeche->face;
                    }
                  hface = anfang;
                  dir = anfangrichtung;
                  zaehler180 = 1;
                  while ((zaehler180 == 1)&&(zaehler < h))
                  {
                  while (((dir == maxdir)||(dir == gegendir))&&(zaehler < h))
                    {
                      merke = hface;
                      merkzaehler = zaehler + 1;
                      if (hface->a == 90)
                        dir++;
                      if (dir > 4)
                         dir -= 4;
                      if (hface->next != NULL)
                        hface = hface->next;
                       else hface = hgraph->flaeche->face;
                      zaehler++;
                    }  /*  while */
                  if ((zaehler <= h)&&(dir != maxdir)&&(dir != gegendir))
                    {
                       hhface = hface;
                       hzaehler = zaehler;
                       zaehler180 = 0;
                       while ((hhface->a == 180)&&(hzaehler < h))
                         {
                            hzaehler++;
                            zaehler180++;
                            if (hhface->next != NULL)
                              hhface = hhface->next;
                             else hhface = hgraph->flaeche->face;
                         }  /*  while  */
                    } /*  if zaehler <= h  */
                  if (zaehler180 == 1)
                    {
                      if (hface->next != NULL)
                        hface = hface->next;
                        else hface = hgraph->flaeche->face;
                      zaehler++;
                      if (hface->a == 90)
                        dir++;
                      if (dir > 4)
                        dir -= 4;
                      if (hface->next != NULL)
                        hface = hface->next;
                        else hface = hgraph->flaeche->face;
                      zaehler++;
                    }
                  }  /*  while  */
                  if (zaehler180 != 1)
                     {
                        veraendert = 1;
                        hhface = anfang;
                        hzaehler = 1;
                        while (hhface != merke)
                          {
                            hzaehler++;
                            if (hhface->next != NULL)
                              hhface = hhface->next;
                             else hhface = hgraph->flaeche->face;
                          }
                        if (hzaehler > h)
                          hzaehler = 0;
                        hzaehler180 = 0;
                        dirmax = 1;
                        dirz = 1;
                        zaehler90 = -1;
                        while (hzaehler < h)
                          {
                            if (hhface->a == 180)
                              {
                                if ((dirmax == 1)||(dirz == 2))
                                  hzaehler180++;
                                  else dirz++;
                              }
                             else
                              {
                                dirz = 1;
                                if (dirmax == 1)
                                  dirmax = 0;
                                 else dirmax = 1;
                                if (hhface->a == 90)
                                  zaehler90++;
                              }
                            hzaehler++;
                            if (hhface->next != NULL)
                              hhface = hhface->next;
                             else hhface = hgraph->flaeche->face;
                          }
                        if ((hzaehler180 > 0)&&(zaehler90 > 0))
                          {
                             if (merke->next != NULL)
                                merke = merke->next;
                               else merke = hgraph->flaeche->face;
                             if (merke->a == 90)
                               {
                                 merke->a = 180;
                                 merke->l++;
                                 if (merke->next != NULL)
                                   merke = merke->next;
                                  else merke = hgraph->flaeche->face;
                                 while (merke->a != 180)
                                   if (merke->next != NULL)
                                     merke = merke->next;
                                    else merke = hgraph->flaeche->face;
                                 merke->a = 90;
                                 merke->l--;
                                 hhface = anfang;
                                 while (hhface != merke)
                                   {
                                     hhface->dir = 1;
                                     if (hhface->next != NULL)
                                       hhface = hhface->next;
                                      else hhface = hgraph->flaeche->face;
                                   }
                                 hhface->dir = 1;
                               }  /*  merke->a == 90  */
                              else  /*  merke->a == 180  */
                               {
                                 if (merke->next != NULL)
                                    merke = merke->next;
                                   else merke = hgraph->flaeche->face;
                                 merke->a = 90;
                                 merke->l--;
                                 if (merke->next != NULL)
                                    merke = merke->next;
                                   else merke = hgraph->flaeche->face;
                                 while (merke->a == 180)
                                   {
                                     if (merke->next != NULL)
                                       merke = merke->next;
                                      else merke = hgraph->flaeche->face;
                                   }
                                 merke->a = 180;
                                 merke->l++;
                                  hhface = anfang;
                                 while (hhface != merke)
                                   {
                                     hhface->dir = 1;
                                     if (hhface->next != NULL)
                                       hhface = hhface->next;
                                      else hhface = hgraph->flaeche->face;
                                   }
                                 hhface->dir = 1;
                              }  /*  else  */
                          }  /*  if hzaehler180 > 0  */
                         else
                          {
                             if ((zaehler180 == 0)||(zaehler90 <= 0))
                               {
                                  hhface = anfang;
                                  kritzaehler = 0;
                                  while (hhface != merke)
                                    {
                                      if ((hhface->a == 180)&&
                                          (hhface->dir == 0))
                                         kritzaehler++;
                                      if (hhface->next != NULL)
                                        hhface = hhface->next;
                                       else hhface = hgraph->flaeche->face;
                                    }
                                  if (kritzaehler > 0)
                                    {
                                      hhface = merke;
                                      neumerke = merke;
                                      while (hhface->a != 180)
                                        {
                                           hhface = anfang;
                                           if (neumerke == hhface)
                                             {
                                               while (hhface->next != NULL)
                                                 hhface = hhface->next;
                                             }
                                            else
                                             {
                                               while (hhface->next != neumerke)
                                                 if (hhface->next != NULL)
                                                   hhface = hhface->next;
                                                  else hhface = 
                                                    hgraph->flaeche->face;
                                             }
                                           neumerke = hhface;
                                        }
                                      if ((merke->afix == 0)
                                          &&(hhface->afix == 0))
                                        {
                                          merke->a = 180;
                                          merke->l++;
                                          hhface->a = 90;
                                          hhface->l--;
                                          hhface = anfang;
                                          while (hhface != merke)
                                            {
                                              hhface->dir = 1;
                                              if (hhface->next != NULL)
                                                hhface = hhface->next;
                                              else hhface = 
                                                   hgraph->flaeche->face;
                                            }
                                          hhface->dir = 1;
                                        }
                                      else veraendert = 0;
                                    }  /*  kritzaehler > 0  */
                                  else veraendert = 0;
                               }  /* if  */
                              else  /*  zaehler180 > 1  */
                               {
                                 merke->a = 180;
                                 merke->l++;
                                 for (i=(merkzaehler + 1);i < zaehler;i++)
                                   if (merke->next != NULL)
                                     merke = merke->next;
                                    else merke = hgraph->flaeche->face;
                                 merke->a = 90;
                                 merke->l--;
                               }  /*  zaehler180 > 1  */
                          }  /*  zaehler >= h  */
                     }  /*  if zaehler180 != 0  */
                }  /*  while geaendert == 1  */
              /* Nun beginnt die Berechnung von l, d.h. die Laenge idealer
                Kantenfolgen */
              hface = hgraph->flaeche->face;
              ges1 = 0;
              ges2 = 0;
              ges3 = 0;
              ges4 = 0;
              richtung = 1;  /*  nach links  */
              while (hface != NULL)
                {
                  if (hface == anfang)
                    anfangrichtung = richtung;
                  if (richtung == 1)
                    ges1++;
                    else if (richtung == 2)
                     ges2++;
                     else if (richtung == 3)
                      ges3++;
                      else ges4++;
                  if (hface->a == 90)
                     richtung++;
                    else if (hface->a == 270)
                     richtung+= 3;
                  if (richtung > 4)
                    richtung-= 4;
                  hface = hface->next;
                }  /*  while  */
              maxi = ges1;
              maxdir = 1;
              if (ges2 > maxi)
                {
                  maxi = ges2;
                  maxdir = 2;
                }   /*  if  */
              if (ges3 > maxi)
                {
                  maxi = ges3;
                  maxdir = 3;
                }   /*  if  */
              if (ges4 > maxi)
                {
                  maxi = ges4;
                  maxdir = 4;
                }   /*  if  */
              gegendir = maxdir + 2;
              if (gegendir > 4)
                gegendir -= 4;
              if (maxdir == 1)
                {
                  gegendir = 3;
                  gegenmax = ges3;
                }
               else if (maxdir == 2)
                {
                  gegendir = 4;
                  gegenmax = ges4;
                }
               else if (maxdir == 3)
                {
                  gegendir = 1;
                  gegenmax = ges1;
                }
               else 
                {
                  gegendir = 2;
                  gegenmax = ges2;
                }
              gesamt = maxi + gegenmax;
              soll = ((gesamt/2)-gegenmax);
              kritdir = maxdir + 2;
              if (kritdir > 4)
                kritdir -= 4;
              if (kritdir == 1)
                 krit = ges1;
                else if (kritdir == 2)
                 krit = ges2;
                else if (kritdir == 3)
                 krit = ges3;
                else krit = ges4;
              diff = (soll - krit);
              if (diff < 0)
                diff = 0;
              hface = anfang;
              gefundenmax = 0;
              gefundengegen = 0;
              dir = anfangrichtung;
              zaehler270 = 0;
              i = 0;
              while (((gefundenmax == 0)||(gefundengegen == 0))&&(i < h))
                {
                   if (dir == maxdir)
                      gefundenmax = 1;
                     else if (dir == gegendir)
                      gefundengegen = 1;
                   if (hface->a == 90)
                     dir++;
                    else if (hface->a == 270)
                     {
                       dir+= 3;
                       zaehler270++;
                     }
                   if (dir > 4) dir-= 4;
                   i++;
                   if (hface->next != NULL)
                     hface = hface->next;
                    else hface = hgraph->flaeche->face;
                }  /*  while  */
        if ((zaehler270 == 0)&&(gefundenmax == 1)&&(gefundengegen == 1)&&
            (soll > 0))
           {
              hface = anfang;
              dir = anfangrichtung;
              i = 0;
              zaehler = 1;
              while ((dir != maxdir)&&(dir != gegendir)&&(i < h))
                {
                  zaehler++;
                  if (hface->a == 90)
                    dir++;
                   else if (hface->a == 270)
                    dir += 3;
                  if (dir > 4)
                    dir -= 4;
                  i++;
                  if (hface->next != NULL)
                    hface = hface->next;
                   else hface = hgraph->flaeche->face;
                }  /*  while  */
              if (dir == maxdir)
                modus = 2;
                else if (dir == gegendir)
                 modus = 1;
                else modus = 3;
              manipuliere = 1;
              while (manipuliere == 1)
              {
              manipuliere = 0;
              if (modus == 1)
                {
                  while (hface->a != 90)
                    {
                      if (hface->next != NULL)
                        hface = hface->next;
                       else hface = hgraph->flaeche->face;
                      i++;
                    }
                  hhface = hface;
                  zaehler180 = 0;
                  while (i < h)
                   {
                     if (hhface->a == 180)
                       zaehler180++;
                     i++;
                     if (hhface->next != NULL)
                       hhface = hhface->next;
                      else hhface = hgraph->flaeche->face;
                   }  /*  while  */
                  if (soll < zaehler180)
                    aendere = soll;
                   else aendere = zaehler180;
                  zaehler90 = 0;
                  for (i=1;i<=aendere;i++)
                    {
                       if (hface->a != 180)
                         {
                            zaehler90++;
                            hface->a = 180;
                            hface->l++;
                         }
                       if (hface->next != NULL)
                         hface = hface->next;
                        else hface = hgraph->flaeche->face;
                    }
                  i = 0;
                  dirz = 1;
                  dirmax = 1;
                  while (i < zaehler90)
                    {
                       if (hface->a == 180)
                         {
                            if ((dirmax == 1)||(dirz == 2))
                              {
                                dirz = 1;
                                hface->a = 90;
                                i++;
                                hface->l--;
                                if (dirmax == 1)
                                {
                                  if (hface->next != NULL)
                                    hface = hface->next;
                                   else hface = hgraph->flaeche->face;
                                  if (hface->a == 90)
                                    {
                                       hface->a = 180;
                                       i--;
                                       hface->l++;
                                    }
                                }
                              }  /*  dirmax == 1 ...  */
                            else
                              {
                                dirz++;
                              }
                         }
                        else
                         {
                           dirz = 1;
                           if (dirmax == 1)
                             dirmax = 0;
                            else dirmax = 1;
                         }
                       if (hface->next != NULL)
                         hface = hface->next;
                        else hface = hgraph->flaeche->face;
                    }
                }  /*  modus == 1  */
               else if (modus == 2)
                {
                  zaehler180 = 0;
                  while (hface->a != 90)
                    {
                       zaehler180++;
                       if (hface->next != NULL)
                         hface = hface->next;
                        else hface = hgraph->flaeche->face;
                    }
                  aendere = soll;
                  if (aendere <= zaehler180)
                    {
                      zaehler += zaehler180;
                      zaehle = (zaehler - aendere);
                      hface = anfang;
                      for (i=1;i<zaehle;i++)
                        if (hface->next != NULL)
                          hface = hface->next;
                         else hface = hgraph->flaeche->face;
                      zaehler90 = 1;
                      hface->a = 90;
                      hface->l--;
                      if (hface->next != NULL)
                         hface = hface->next;
                        else hface = hgraph->flaeche->face;
                      if (hface->a == 90)
                        {
                          zaehler90--;
                          hface->a = 180;
                          hface->l++;
                        }
                      if (hface->next != NULL)
                         hface = hface->next;
                        else hface = hgraph->flaeche->face;
                      if (hface->a == 180)
                        {
                          zaehler90++;
                          hface->a = 90;
                          hface->l--;
                        }
                      if (hface->next != NULL)
                         hface = hface->next;
                        else hface = hgraph->flaeche->face;
                      if (zaehler90 > 0)
                        {
                          while (hface->a != 90)
                           if (hface->next != NULL)
                             hface = hface->next;
                            else hface = hgraph->flaeche->face;
                          hface->a= 180;
                          hface->l++;
                        }
                      if (zaehler90 > 1)
                        {
                          while (hface->a != 90)
                            if (hface->next != NULL)
                              hface = hface->next;
                             else hface = hgraph->flaeche->face;
                          hface->a= 180;
                          hface->l++;
                        }
                    }  /* if aendere <= zaehler180  */
                   else
                    {
                      zaehle = zaehler;
                      hface = anfang;
                      for (i=1;i<zaehle;i++)
                        if (hface->next != NULL)
                          hface = hface->next;
                         else hface = hgraph->flaeche->face;
                      zaehler90 = 0;
                      diffzaehler = 0;
                      if (hface->a == 180)
                        {
                          hface->a = 90;
                          hface->l--;
                          zaehler90++;
                        }
                      if (hface->next != NULL)
                         hface = hface->next;
                        else hface = hgraph->flaeche->face;
                      if (hface->a == 90)
                        {
                          hface->a = 180;
                          hface->l++;
                          zaehler90--;
                        }
                       else if (hface->a == 180)
                         diffzaehler++;
                      if (hface->next != NULL)
                         hface = hface->next;
                        else hface = hgraph->flaeche->face;
                      if (hface->a == 180)
                        {
                          hface->a = 90;
                          hface->l--;
                          zaehler90++;
                        }
                      if (hface->next != NULL)
                         hface = hface->next;
                        else hface = hgraph->flaeche->face;
                      while (hface->a != 90)
                        {
                           if (hface->next != NULL)
                             hface = hface->next;
                            else hface = hgraph->flaeche->face;
                           diffzaehler++;
                        }
                      if (zaehler90 > 0)
                        {
                           hface->a= 180;
                           hface->l++;
                        }
                      while (hface->a != 90)
                        if (hface->next != NULL)
                          hface = hface->next;
                         else hface = hgraph->flaeche->face;
                      if (zaehler90 > 1)
                        {
                          hface->a= 180;
                          hface->l++;
                        }
                      dir = anfangrichtung;
                      hface = anfang;
                      diff = 0;
                      while (dir != gegendir)
                        {
                           zaehler++;
                           diff++;
                           if (hface->a == 90)
                             dir++;
                           if (dir > 4)
                             dir -= 4;
                           if (hface->next != NULL)
                             hface = hface->next;
                             else hface = hgraph->flaeche->face;
                        }
                      anfang = hface;
                      if (zaehler90 > 0)
                        soll -= (diffzaehler + zaehler90);
                      gefunden = 0;
                      i = 0;
                      while ((gefunden == 0)&&(zaehler < h))
                        {
                           if (dir == maxdir)
                              gefunden = 1;
                             else
                              {
                                 zaehler++;
                                 if (hface->a == 90)
                                   dir++;
                                 if (dir > 4)
                                   dir -= 4;
                                 if (hface->next != NULL)
                                    hface = hface->next;
                                   else hface = hgraph->flaeche->face;
                              }   /* else */
                        }  /*  while  */
                      h -= diff;
                      if (gefunden == 1)
                        {
                          hface = anfang;
                          zaehler = 1;
                          modus = 1;
                          manipuliere = 1;
                        }
                    }  /*  aendere > zaehler180  */
                }  /*  modus == 2  */
             }   /*  while manipuliere == 1 */
      }  /*  grosses if  */
         /*  Nun werden die Aenderungen im anderen Segment auch vorgenommen.  */
              hhgraph = graph;
              while (hhgraph->flaeche->nummer != kritfacenummer)
                hhgraph = hhgraph->next;
              hface = hgraph->flaeche->face;
              while (hface != NULL)
                {
                   if (hface->l == 1)  /*  90 wurde zu 180  */
                     {
                       hhface = hhgraph->flaeche->face;
                       gefunden = 0;
                       while (gefunden == 0)
                         {
                           if ((hhface->ende == hface->ende)&&
                               (hhface->a >= 180))
                             gefunden = 1;
                            else hhface = hhface->next;
                         }
                        hhface->a -= 90;
                     }  /*  if  */
                    else if (hface->l == -1)
                     {
                       hhface = hhgraph->flaeche->face;
                       gefunden = 0;
                       while (gefunden == 0)
                         {
                           if (hhface->ende == hface->ende)
                             gefunden = 1;
                            else hhface = hhface->next;
                         }
                        hhface->a += 90;
                     }  /*  else if  */
                   hface = hface->next;
                }  /*  while  */
          /*  Nun werden die Werte l wieder zurueckgesetzt.  */
              hface = hgraph->flaeche->face;
              while (hface != NULL)
                {
                  hface->l = 0;
                  hface = hface->next;
                }
           }  /*  if ok  */
         else hfacemenge = hfacemenge->next;
      }  /*  if  */
     else hfacemenge = hfacemenge->next;
  }  /*  while  */
if (geaendert == 0)
  return(geaendert);
 else return(hfacemenge->nummer);
}  /*  optimieren  */





void fluss_fixieren(zahl) 
 /*  Hilfsprocedur von berechne_orthogonale_darstellung  */
/*  Jeder Fluss ueber das eben in voroptimieren behandelte Segment mit der
     Nummer zahl wird nun im Netzwerk als fixed gekennzeichnet.  */
{
TBOGENMENGE *hbogenmenge;
TNETZKNOTENMENGE *hnetzknotenmenge;
int nummer;
hnetzknotenmenge = netzknotenmenge;
while ((hnetzknotenmenge->typ != 'f')||(hnetzknotenmenge->herkunft != zahl))
  hnetzknotenmenge = hnetzknotenmenge->next;
nummer = hnetzknotenmenge->nummer;
hbogenmenge = bogenmenge;
while (hbogenmenge != NULL)
  {
    if (hbogenmenge->anfang == nummer)
       {
          if (hbogenmenge->fixed == -1)
            Fixed += hbogenmenge->flow;
           else Fixed += (hbogenmenge->flow - hbogenmenge->fixed);
       }
    if ((hbogenmenge->anfang == nummer)||(hbogenmenge->ende == nummer))
      {
        hbogenmenge->fixed = hbogenmenge->flow;
      }   /*  if */
    hbogenmenge = hbogenmenge->next;
  }  /*  while  */
}  /*  fluss_fixieren  */




void modifizieren(zahl) 
 /*  Hilfsprocedur von berechne_orthogonale_darstellung  */
/*  Die Winkel des behandelten Segments  bewirken Aenderungen der Werte 
   fuer p in anderen Segmenten. Diese aenderungen werden vorgenommen. */
{
int modify();
TGRAPH *hgraph,*refgraph;
TKANTENTRIPEL *hface,*refface;
hgraph = graph;
while (hgraph->flaeche->nummer != zahl)
  hgraph = hgraph->next;
hface = hgraph->flaeche->face;
while (hface != NULL)
  {
    refgraph = graph;
    refface = refgraph->flaeche->face;
    while (refface != NULL)
      {
        if (refface->ende == hface->ende)
          {
            refface->p = modify(refface->p,hface->a);
            refface->fixed = 1;
          }
        if (refface->next != NULL)
          refface = refface->next;
         else
          {
            refgraph = refgraph->next;
            if (refgraph != NULL)
              refface = refgraph->flaeche->face;
             else refface = NULL;
          }
       }  /*  while  refface != NULL  */
    hface = hface->next;
  }  /*  while  hface != NULL  */
}  /*  modifizieren  */









int suche_faceknoten(face)
/*  Hilfsprocedur von aendere_kosten */
/* zurueckgegeben wird die netzknotennummer von Segment Nr. face  */
{
TNETZKNOTENMENGE *hnetzknotenmenge;
int gefunden;
gefunden = 0;
hnetzknotenmenge = netzknotenmenge;
while (gefunden == 0)
  {
     if ((hnetzknotenmenge->typ != 'f')||(hnetzknotenmenge->herkunft != face))
        hnetzknotenmenge = hnetzknotenmenge->next;
       else gefunden = 1;
  }
return(hnetzknotenmenge->nummer);
}  /*  suche_faceknoten  */





int suche_knotenknoten(name)
/*  Hilfsprocedur von aendere_kosten */
/* zurueckgegeben wird die netzknotennummer von Knoten Nr. name.
   Gibt es diesen Knoten nicht, so wird 0 zurueckgegeben.  */
{
TNETZKNOTENMENGE *hnetzknotenmenge;
int gefunden;
gefunden = 0;
hnetzknotenmenge = netzknotenmenge;
while ((gefunden == 0)&&(hnetzknotenmenge !=  NULL))
  {
     if ((hnetzknotenmenge->typ != 'v')||(hnetzknotenmenge->herkunft != name))
        hnetzknotenmenge = hnetzknotenmenge->next;
       else gefunden = 1;
  }
if (hnetzknotenmenge != NULL)
  return(hnetzknotenmenge->nummer);
  else return(0);
}  /*  suche_knotenknoten  */







/******************************************************************************
******************************************************************************/

void finde_laengen()     
{
void bestimme_richtungen();
void kopiere();
void mache_rechtecke();
void kompaktifiziere();
void kuerze();
void berechne_laengen();
void berechne_position();
void kuerze_laengengraph();
void berechne_knicke();


bestimme_richtungen();
kopiere();
mache_rechtecke();
kompaktifiziere();
kuerze();
berechne_laengen();
berechne_position();
kuerze_laengengraph();
berechne_knicke();
}  /*  finde_laengen  */




void kopiere()   /*  Hilfsprocedur von finde_laengen */
/*  der Graph graph wird nach laengengraph kopiert.  */
{
TGRAPH *hilfsgraph,*hilfslaengengraph;
TFACE *hilfsflaeche,*laengenflaeche;
TKANTENTRIPEL *hilfstripel,*laengentripel,*hilfslaengentripel;
hilfsgraph = graph;
laengengraph = ( TGRAPH*) malloc(sizeof(TGRAPH));
laengenflaeche = (TFACE*) malloc(sizeof(TFACE));
laengengraph->flaeche = laengenflaeche;
laengengraph->next = NULL;
hilfsflaeche = hilfsgraph->flaeche;
laengenflaeche->nummer = hilfsflaeche->nummer;
laengentripel = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
hilfstripel = hilfsflaeche->face;
laengenflaeche->face = laengentripel;
laengentripel->name = hilfstripel->name;
laengentripel->a = hilfstripel->a;
laengentripel->afix = hilfstripel->afix;
laengentripel->tried = hilfstripel->tried;
laengentripel->bridge = hilfstripel->bridge;
laengentripel->p = hilfstripel->p;
laengentripel->dir = hilfstripel->dir;
laengentripel->tried = 0;
laengentripel->l = 0;
laengentripel->s = hilfstripel->s;
laengentripel->anfang = hilfstripel->anfang;
laengentripel->ende = hilfstripel->ende;
laengentripel->next = NULL;
hilfslaengentripel = laengentripel;
while (hilfstripel->next != NULL)
  {
    hilfstripel = hilfstripel->next;
    laengentripel->next = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
    laengentripel->next->name = hilfstripel->name;
    laengentripel->next->a = hilfstripel->a;
    laengentripel->next->afix = hilfstripel->afix;
    laengentripel->next->dir = hilfstripel->dir;
    laengentripel->next->bridge = hilfstripel->bridge;
    laengentripel->next->anfang = hilfstripel->anfang;
    laengentripel->next->ende = hilfstripel->ende;
    laengentripel->next->tried = hilfstripel->tried;
    laengentripel->next->tried = 0;
    laengentripel->next->l = 0;
    laengentripel->next->s = hilfstripel->s;
    laengentripel->next->p = hilfstripel->p;
    laengentripel->next->next = NULL;
    laengentripel = laengentripel->next;
  }  /*  while  */
laengentripel = hilfslaengentripel;
hilfsgraph = hilfsgraph->next;
hilfslaengengraph = laengengraph;
while (hilfsgraph != NULL)
  {
   laengengraph->next = ( TGRAPH*) malloc(sizeof(TGRAPH));
   laengenflaeche = (TFACE*) malloc(sizeof(TFACE));
   laengengraph->next->flaeche = laengenflaeche;
   laengengraph->next->next = NULL;
   hilfsflaeche = hilfsgraph->flaeche;
   laengenflaeche->nummer = hilfsflaeche->nummer;
   laengentripel = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
   hilfstripel = hilfsflaeche->face;
   laengenflaeche->face = laengentripel;
   laengentripel->name = hilfstripel->name;
   laengentripel->a = hilfstripel->a;
   laengentripel->afix = hilfstripel->afix;
   laengentripel->dir = hilfstripel->dir;
   laengentripel->bridge = hilfstripel->bridge;
   laengentripel->anfang = hilfstripel->anfang;
   laengentripel->tried = hilfstripel->tried;
   laengentripel->ende = hilfstripel->ende;
   laengentripel->p = hilfstripel->p;
   laengentripel->tried = 0;
   laengentripel->l = 0;
   laengentripel->s = hilfstripel->s;
   laengentripel->next = NULL;
   hilfslaengentripel = laengentripel;
   while (hilfstripel->next != NULL)
     {
       hilfstripel = hilfstripel->next;
       laengentripel->next = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
       laengentripel->next->name = hilfstripel->name;
       laengentripel->next->a = hilfstripel->a;
       laengentripel->next->afix = hilfstripel->afix;
       laengentripel->next->dir = hilfstripel->dir;
       laengentripel->next->bridge = hilfstripel->bridge;
       laengentripel->next->p = hilfstripel->p;
       laengentripel->next->tried = hilfstripel->tried;
       laengentripel->next->anfang = hilfstripel->anfang;
       laengentripel->next->ende = hilfstripel->ende;
       laengentripel->next->tried = 0;
       laengentripel->next->l = 0;
       laengentripel->next->s = hilfstripel->s;
       laengentripel->next->next = NULL;
       laengentripel = laengentripel->next;
     }  /*  while  */
   laengentripel = hilfslaengentripel;
   hilfsgraph = hilfsgraph->next;
   laengengraph = laengengraph->next;
  }  /*  while  */
laengengraph = hilfslaengengraph;
}  /*  kopiere  */




void knickhilfsknoten()  
 /*  Hilfsprocedur von berechne_orthogonale_darstellung  */
/*  Weist jedem Knick einen Hilfsknoten zu.  */
{
void hilfsknoteneinfuegen();
TGRAPH *hgraph,*hgraph2;
TFACE *hflaeche;
TKANTENTRIPEL *hface,*hface2;
TKANTENMENGE *hkantenmenge;
int name,noetig,gefunden,gefunden1,fertig;
int anfang,ende;
int verkehrt;
int nameanfang,nameende;
echtkantenzahl = kantenzahl;
fertig = 0;
while (fertig == 0)
{
fertig = 1;
hgraph = graph;
gefunden = 0;
noetig = 1;
while ((gefunden == 0)&&(hgraph != NULL))
  {
    hflaeche = hgraph->flaeche;
    hface = hflaeche->face;
    while ((gefunden == 0)&&(hface != NULL))
     {
       if (noetig == 1)
        {
          if (hface->s != 0)
            {
              name = hface->name;
              fertig = 0;
              hilfsknoteneinfuegen(hface,1,0,0,0);
              noetig = 0;
            }  /*  if hface-> != 0  */
         hface = hface->next;
        }  /*  if noetig == 1  */
       else    /*  noetig == 0  */
       {
        if (hface->name == name)
          {
            gefunden1 = 0;
            hkantenmenge = kantenmenge;
            while (gefunden1 == 0)
              {
                 if (hkantenmenge->nummer == name)
                    gefunden1 = 1;
                   else hkantenmenge = hkantenmenge->next;
              }  /*  while  */
            nameanfang = hkantenmenge->anfang;
            nameende = hkantenmenge->ende;
            anfang = hface->anfang;
            ende = hface->ende;
            if (anfang == nameanfang)
              verkehrt = 0;
             else verkehrt = 1;
            hilfsknoteneinfuegen(hface,2,anfang,ende,verkehrt);
            gefunden = 1;
          }  /*  if hface->name == name */
        hface = hface->next;
       }  /*  else  */
      }  /*  while  */
    hgraph = hgraph->next;
  }  /*  while  gefunden == 0 && hgraph != NULL  */
    hgraph2 = graph;
    while (hgraph2 != NULL)
      {
        hface2 = hgraph2->flaeche->face;
        while (hface2 != NULL)
          {
             if (hface2->anfang == 0)
               {
                  hface2->anfang = suche_anfang(hface2->name);
                  hface2->ende = suche_ende(hface2->name);
               }  /*  if  */
             hface2 = hface2->next;
          }  /*  while  */
             hgraph2 = hgraph2->next;
  }  /*  while  hgraph2 != NULL  */


}   /*  while  fertig == 0  */
originalkantenzahl = kantenzahl;


return;
}  /*  knickhilfsknoten  */



void hilfsknoteneinfuegen(pointer,modus,anfang,ende,verkehrt)  
 /*  Hilfsprocedur von knickhilfsknoten  */
/*   ersetzt eine Kante mit Knicken durch entsprechend viele Kanten ohne
     Knicke und entsprechende Hilfsknoten.  */
TKANTENTRIPEL *pointer;
{
void kanteneinfuegen();
void knoteneinfuegen();
void kanteneintragen();
void mache_hilfskante();
void kantentauschen();
int betrag();
TKANTENTRIPEL *hpointer;
int laenge,zahl,i;
int winkel,winkel1,winkel2,name1,name2,name3,name4,name5,name;

if (pointer->s != 0)
{
laenge = betrag(pointer->s);
name = pointer->name;
if (modus != 1)
  {
    for (i=1;i<=laenge + 1;i++)
      {
        name1 = (kantenzahl + laenge + 2 - i);
        mache_hilfskante(name1,name);
      }
    zahl = knotenzahl;
    for (i=1;i<=laenge;i++)
      knoteneinfuegen();
    for (i=1;i<=laenge + 1;i++)
      {
        if (verkehrt == 0)
          {
           name1 = (kantenzahl + laenge + 2 - i);
           if (i == 1)
             name2 = anfang;
            else
             name2 = (zahl + i - 1);
           if ( i == (laenge + 1))
             name3 = ende;
            else
             name3 = zahl + i;
           
           if (i != ((laenge/2) + 1))
             kanteneintragen(name1,name2,name3);
            else
             kantentauschen(name,anfang,ende,name1,name2,name3);
           }  /*  if  verkehrt */
          else  /*  verkehrt = 1  */
           {
             name1 = (kantenzahl + laenge + 2 - i);
             if (i == 1)
               name2 = anfang;
              else
               name2 = (zahl + i - 1);
             if ( i == (laenge + 1))
               name3 = ende;
              else
               name3 = zahl + i;
             if (i != ((laenge/2) + 1))
               kanteneintragen(name1,name3,name2);
              else
               kantentauschen(name,anfang,ende,name1,name3,name2);
            }  /*  else verkehrt */
        }  /*  for  */
  }  /*  if (modus != 1)  */

if (pointer->s > 0)
   winkel1 = 270;
  else winkel1 = 90;
winkel2 = pointer->a;
if (modus == 1)
  name1 = kantenzahl + 1;
  else
  name1 = kantenzahl + laenge + 1;
pointer->name = name1;
pointer->s = 0;
pointer->anfang = 0;
pointer->a = winkel1;
hpointer = pointer;
for (i=2;i<=laenge + 1;i++)
  {
    if (modus == 1)
      name1 = (kantenzahl + i);
     else
      name1 = (kantenzahl + laenge + 2 - i);
    if (i == (laenge + 1))
      winkel = winkel2;
     else
      winkel = winkel1;
    kanteneinfuegen(hpointer,name1,winkel);
    hpointer = hpointer->next;
  }  /*  for  */
if (modus != 1)
  kantenzahl += (laenge + 1);
}   /*  if laenge > 0  */

return;
}   /*  hilfsknoteneinfuegen   */




void kanteneinfuegen(pointer,name,winkel)
   /*  Hilfsprocedur von hilfsknoteneinfuegen  */
/*  Fuegt in graph an der Stelle pointer eine Kante mit dem
    Namen name, dem String "" und dem Winkel winkel ein.  */
TKANTENTRIPEL *pointer;
{
TKANTENTRIPEL *hilf;
hilf = (TKANTENTRIPEL *) malloc(sizeof(TKANTENTRIPEL));
hilf->name = name;
hilf->s = 0;
hilf->a = winkel;
hilf->afix = 0;
hilf->l = 0;
hilf->dir = 0;
hilf->bridge = 0;
hilf->tried = 0;
hilf->ende = 0;
hilf->p = 0;
hilf->fixed = 0;
hilf->anfang = 0;
hilf->next = pointer->next;
pointer->next= hilf;
return;
}  /*  kanteneinfuegen  */



void knoteneinfuegen() 
/*  Hilfsprocedur von hilfsknoteneinfuegen  */
/*  Erweitert die Knotenmenge um einen Knoten mit Grad 2. */
{
TKNOTENMENGE *hilf;
hilf = (TKNOTENMENGE *) malloc(sizeof(TKNOTENMENGE));
knotenzahl++;
hilf->nummer = knotenzahl;
hilf->edname = 0;
strcpy(hilf->name,"");
hilf->grad = 2;
hilf->echt = 0;
hilf->teil = 0;
hilf->xpos = -100;
hilf->ypos = -100;
hilf->merkx = 0;
hilf->merky = 0;
hilf->next = knotenmenge;
knotenmenge = hilf;
return;
}  /* knoteneinfuegen  */




void kanteneintragen(name,name1,name2) 
   /*  Hilfsprocedur von hilfsknoteneinfuegen  */
/* Traegt die Kante name:(name1,name2) in die kantenmenge ein.  */
{
TKANTENMENGE *hilf;
hilf = (TKANTENMENGE *) malloc(sizeof(TKANTENMENGE));
hilf->nummer = name;
strcpy(hilf->name,"");
hilf->anfang = name1;
hilf->ende = name2;
hilf->edanfang = 0;
hilf->edende = 0;
hilf->schnitt = 0;
hilf->sbeh = 0;
hilf->tbeh = 0;
hilf->behsknoten = 0;
hilf->knicke = NULL;
hilf->next = kantenmenge;
kantenmenge = hilf;
return;
}



void kantentauschen(lname,lanfang,lende,name,anfang,ende) 
  /*  Hilfsprocedur von hilfsknoteneinfuegen  */
/*  Die Kante lname : lanfang -> lende wird aus kantenmenmge geloescht.  
    Die Kante name : (name1,name2) dafuer eingetragen.  */
{
TKANTENMENGE *hilf,*hilf1,*freemenge;
char kantenname[10];
int fertig = 0;
hilf = kantenmenge;
hilf1 = NULL;
while ((hilf != NULL)&&(fertig == 0))
  {
   if (((hilf->nummer == lname)&&(hilf->anfang == lanfang)
         &&(hilf->ende == lende))
      || ((hilf->nummer == lname)&&(hilf->anfang == lende)
         &&(hilf->ende == lanfang)))
      {
        fertig = 1;
        strcpy(kantenname,hilf->name);
        if (hilf1 == NULL)
           {
             freemenge = kantenmenge;
             kantenmenge = hilf->next;
           }
          else 
           {
             freemenge = hilf1->next;
             hilf1->next = hilf->next;
           }
         /*  free(freemenge); */
      }   /*  if  */
    else 
     {
       hilf1 = hilf;
       hilf = hilf->next;
     }
  }  /*  while  */
hilf = (TKANTENMENGE *) malloc(sizeof(TKANTENMENGE));
hilf->nummer = name;
strcpy(hilf->name,kantenname);
hilf->anfang = anfang;
hilf->ende = ende;
hilf->edanfang = 0;
hilf->edende = 0;
hilf->schnitt = 0;
hilf->sbeh = 0;
hilf->tbeh = 0;
hilf->behsknoten = 0;
hilf->knicke = NULL;
hilf->next = kantenmenge;
kantenmenge = hilf;
return;
}  /*  kantentauschen  */



void bestimme_richtungen()    /*  Hilfsprocedur von finde_laengen  */
/*  Fuer jede Kante wird die Richtung festgelegt.
    1 = von links nach rechts;
    2 = von oben nach unten;
    3 = von rechts nach links;
    4 = von unten nach oben.
    Die erste Kante des externen Faces laeuft von 
                                         rechts nach links ( Richtung 3).  */
{
void setze_richtung();
TGRAPH *hgraph;
TKANTENTRIPEL *hface,*erster,*letzter;
int gefunden;
int geaendert = 0;
int richtung = 3;
int zaehler = 1;
hgraph = graph;
while (hgraph != NULL)
  {
    hface = hgraph->flaeche->face;
    while (hface != NULL)
      {
        hface->dir = 0;
        hface = hface->next;
      }
    hgraph = hgraph->next;
  }
hgraph = graph;
hface = hgraph->flaeche->face;
while (hface != NULL)
  {
    hface->dir = richtung;
    setze_richtung(hface->name,richtung);
    if (hface->a == 90)
      richtung = (richtung + 1);
    else if (hface->a == 270)
      richtung = (richtung + 3);
    else if (hface->a == 360)
      richtung = (richtung + 2);
    if (richtung > 4)
      richtung = (richtung % 4);
    hface = hface->next;
  }  /*  while  */
while ((hgraph->next != NULL)||(geaendert == 1))
  {
    if (hgraph->next == NULL)
      {
        hgraph = graph->next;
        geaendert = 0;
      }
      else hgraph = hgraph->next;
    hface = hgraph->flaeche->face;
    gefunden = 0;
    while ((hface != NULL)&&(gefunden == 0))
      {
        if (hface->dir == 0)
          hface = hface->next;
        else
          {
            gefunden = 1;
            richtung = hface->dir;
            erster = hgraph->flaeche->face;
            letzter = hface;
            if (hface->a == 90)
              richtung = (richtung + 1);
            else if (hface->a == 270)
              richtung = (richtung + 3);
            else if (hface->a == 360)
              richtung = (richtung + 2);
            if (richtung > 4)
              richtung = (richtung % 4);
            if (hface->next == NULL)
              hface = erster;
              else hface = hface->next;
            while (hface != letzter)
              {
                if (hface->dir == 0)
                  {
                    hface->dir = richtung;
                    setze_richtung(hface->name,richtung);
                    geaendert = 1;
                  }
                if (hface->a == 90)
                  richtung = (richtung + 1);
                else if (hface->a == 270)
                  richtung = (richtung + 3);
                else if (hface->a == 360)
                  richtung = (richtung + 2);
                if (richtung > 4)
                  richtung = (richtung % 4);
                if (hface->next == NULL)
                  hface = erster;
                  else hface = hface->next;
              }  /*  while  */
          }
      }  /*  while  */
  }  /*  while  */
}  /*  bestimme_richtungen  */





void setze_richtung(name,richtung) /*  Hilfsprocedur von bestimme_richtungen  */
/*  sucht in graph Kanten mit Namen name und setzt die Richtung auf 
    richtung + 2 mod 4 , wenn sie vorher 0 war.  */
{
TGRAPH *hgraph;
TKANTENTRIPEL *hface;
int gefunden = 0;
int geaendert = 0;
hgraph = graph;
while ((gefunden < 2)&&(geaendert == 0))
  {
    hface = hgraph->flaeche->face;
    while (hface != NULL)
     {
       if (hface->name == name)
         {
           if (hface->dir == 0)
             {
                richtung = (richtung + 2);
                if (richtung > 4)
                   richtung = (richtung % 4);
                hface->dir = richtung;
                geaendert = 1;
             }
           else gefunden++;
         }
       hface = hface->next;
     }  /*  while  */
    hgraph = hgraph->next;
  }  /*  while  */
return;
}  /*  setze_richtung  */






void mache_rechtecke()   /*  Hilfsprocedur von finde_laengen  */
/*  Teilt jedes Face in Rechteckige Flaechen ein, wobei Hilfsknoten
    und Hilfskanten eingefuehrt werden.  */
{
void ersetze();
TGRAPH *hgraph,*newgraph;
TFACE *hflaeche,*newflaeche;
TKANTENTRIPEL *hface,*newface,*hface2,*neutripel,*neutripel2,*naechster;
int erfolgreich,modus,geaendert,uebergang,hername;
TKANTENTRIPEL *anfang, *hilf,*nexthilf,*neutripelext,*neutripelint;
TKANTENTRIPEL *loesch,*letzter;
hgraph = graph;
newgraph = graph;
if (newgraph->next != NULL)
  while (newgraph->next != NULL)
     newgraph = newgraph->next;
while (hgraph != NULL)
  {
   hflaeche = hgraph->flaeche;
   if (hflaeche->nummer != 1)
    {
     geaendert = 1;
     while (geaendert == 1)
      {
        uebergang = 0;
        geaendert = 0;
        hface = hflaeche->face;
        erfolgreich = 0;

        anfang = hface;  /*  Anfang der Liste  */
        while ((erfolgreich == 0)&&(hface != NULL))
           {
            if ((hface->a == 360)||(hface->a == 270))
              {
                if (hface->next != NULL)
                  hilf = hface->next;
                  else 
                       {
                         hilf = anfang;
                         letzter = hface;
                         uebergang = 1;
                       }
                while (hilf->a == 180)
                    if (hilf->next != NULL)
                        hilf = hilf->next;
                   else 
                         {
                           letzter = hilf;
                           hilf = anfang;
                           uebergang = 1;
                         }
                if (hilf->a == 90)
                 {
                  if (hilf->next != NULL)
                     hilf = hilf->next;
                     else 
                           {
                             letzter = hilf;
                             hilf = anfang;
                             uebergang = 1;
                           }
                  while (hilf->a == 180)
                     if (hilf->next != NULL)
                       hilf = hilf->next;
                       else 
                            {
                              letzter = hilf;
                              hilf = anfang;
                              uebergang = 1;
                            }
                  if (hilf->a == 90)
                   {
                     erfolgreich = 1;
                   }  /*  if  */
                 }  /*  if  */
              }  /*  if  */
            if (erfolgreich == 0)
              hface = hface->next;
          }  /*  while  */

          if (erfolgreich == 1)
           {
            if (uebergang == 1)
               letzter->next = anfang;
            geaendert = 1;
            hface->a = hface->a - 90;
            neutripel = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
            neutripel->name = kantenzahl + 1;
            kantenzahl++;
            neutripel->a = 90;
            neutripel->s = 0;
            neutripel->afix = 0;
            neutripel->l = 0;
            neutripel->dir = 0;
            neutripel->bridge = 0;
            neutripel->tried = 0;
            neutripel->anfang = 0;
            neutripel->ende = 0;
            neutripel->p = 0;
            neutripel->fixed = 0;
            neutripel->next = NULL;
            hface2 = hilf;
            if (hilf->next != NULL)
              {
                hilf = hilf->next;
                neutripel->next = hilf;
              }
              else hilf = anfang;
            hername = hilf->name;
            hilf->name = kantenzahl + 1;
            kantenzahl++;
            mache_hilfskante(kantenzahl,hername);
            neutripel2 = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
            neutripel2->name = kantenzahl + 1;
            kantenzahl++;
            mache_hilfskante(kantenzahl,hername);
            neutripel2->a = 90;
            neutripel2->s = 0;
            neutripel2->afix = 0;
            neutripel2->l = 0;
            neutripel2->dir = 0;
            neutripel2->bridge = 0;
            neutripel2->tried = 0;
            neutripel2->anfang = 0;
            neutripel2->ende = 0;
            neutripel2->p = 0;
            neutripel2->fixed = 0;
            neutripel2->next = NULL;
            hface2->next = neutripel2;
            neutripel2->next = NULL;
            newgraph->next = (TGRAPH *) malloc(sizeof(TGRAPH));
            newgraph = newgraph->next;
            newgraph->next = NULL;
            newgraph->flaeche = (TFACE*) malloc(sizeof(TFACE));
            newflaeche = newgraph->flaeche;
            newflaeche->nummer = facezahl + 1;
            facezahl++;
            neutripel2 = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
            neutripel2->name = kantenzahl - 2;
            neutripel2->a = 90;
            neutripel2->s = 0;
            neutripel2->afix = 0;
            neutripel2->l = 0;
            neutripel2->dir = 0;
            neutripel2->bridge = 0;
            neutripel2->tried = 0;
            neutripel2->anfang = 0;
            neutripel2->ende = 0;
            neutripel2->p = 0;
            neutripel2->fixed = 0;
            neutripel2->next = NULL;
            newflaeche->face = neutripel2;
            if (hface->next != NULL)
               neutripel2->next = hface->next;
               else neutripel2->next = anfang;
            hface->next = neutripel;
            if (uebergang == 1)
              {
                 hflaeche->face = neutripel->next;
                 neutripel->next = NULL;
              }  /*  if  */
            ersetze(hername);
           }   /*  if erfolgreich  */
          }   /*  while geaendert  */
      }  /*  if  internes Face  */
    else
      {
       geaendert = 1;
       while (geaendert == 1)
       {
        geaendert = 0;
        uebergang = 0;
        hface = hflaeche->face;
        erfolgreich = 0;
        anfang = hface;
        while ((erfolgreich == 0)&&(hface != NULL))
          {
            if (hface->a == 360)
              {
                if (hface->next != NULL)
                  hilf = hface->next;
                  else 
                       {
                         letzter = hface;
                         hilf = anfang;
                         uebergang = 1;
                       }
                while (hilf->a == 180)
                  if (hilf->next != NULL)
                    hilf = hilf->next;
                    else 
                         {
                           letzter = hilf;
                           hilf = anfang;
                           uebergang = 1;
                         }
                if (hilf->a == 360)
                  {
                    erfolgreich = 1;
                    modus = 1;
                  }  /*  if  */
                else if (hilf->a == 90)
                  {
                   if (hilf->next != NULL)
                     hilf = hilf->next;
                     else 
                           {
                              letzter = hilf;
                              hilf = anfang;
                              uebergang = 1;
                           }
                   while (hilf->a == 180)
                     if (hilf->next != NULL)
                       hilf = hilf->next;
                       else 
                            {
                              letzter = hilf;
                              hilf = anfang;
                              uebergang = 1;
                            }
                   if (hilf->a == 360)
                     {
                      erfolgreich = 1;
                      modus = 2;
                     }  /*  if  */
                   else if (hilf->a == 270)
                     {
                      erfolgreich = 1;
                      modus = 3;
                     }  /*  else if  */
                   else if (hilf->a == 90)
                    {
                     erfolgreich = 1;
                     modus = 4;
                    }  /*  else if  */
                   else;
                 }  /*  else if  */
            }   /*  360  */
             else if (hface->a == 270)
              {
                if (hface->next != NULL)
                  hilf = hface->next;
                  else 
                       {
                         letzter = hface;
                         hilf = anfang;
                         uebergang = 1;
                       }
                while (hilf->a == 180)
                  if (hilf->next != NULL)
                    hilf = hilf->next;
                    else 
                         {
                           letzter = hilf;
                           hilf = anfang;
                           uebergang = 1;
                         }
                if (hilf->a == 90)
                  {
                    if (hilf->next != NULL)
                     hilf = hilf->next;
                     else 
                           {
                             letzter = hilf;
                             hilf = anfang;
                             uebergang = 1;
                           }
                   while (hilf->a == 180)
                     if (hilf->next != NULL)
                       hilf = hilf->next;
                       else 
                            {
                              letzter = hilf;
                              hilf = anfang;
                              uebergang = 1;
                            }
                   if (hilf->a == 90 )
                     {
                           if (hilf->next == NULL)
                             {
                               letzter = hilf;
                               uebergang = 1;
                             }
                           erfolgreich = 1;
                           modus = 5;
                     }  /*  if  */
                   else if (hilf->a == 270)
                     {
                        erfolgreich = 1;
                        modus = 6;
                     }  /*  if  */
                 }   /*  if 90  */
              }  /*  270  */
             if (erfolgreich == 0)
               hface = hface->next;
          }   /*  while  */
         if (erfolgreich == 1)
          {
            if (uebergang == 1)
               letzter->next = anfang;
            if (hilf->next != NULL)
              nexthilf = hilf->next;
             else nexthilf = hgraph->flaeche->face;
            geaendert = 1;
            hface->a = hface->a - 90;
            if (hface->next != NULL)
               naechster = hface->next;
               else naechster = anfang;
            if (modus == 1)
              {
                neutripelext = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelext->name = kantenzahl + 1;
                kantenzahl++;
                neutripelext->a = 270;
                neutripelext->s = 0;
                neutripelext->afix = 0;
                neutripelext->l = 0;
                neutripelext->dir = 0;
                neutripelext->bridge = 0;
                neutripelext->tried = 0;
                neutripelext->anfang = 0;
                neutripelext->ende = 0;
                neutripelext->p = 0;
                neutripelext->fixed = 0;
                neutripelext->next 
                 = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelext->next->name = kantenzahl + 1;
                kantenzahl++;
                neutripelext->next->a = 270;
                neutripelext->next->s = 0;
                neutripelext->next->afix = 0;
                neutripelext->next->l = 0;
                neutripelext->next->dir = 0;
                neutripelext->next->bridge = 0;
                neutripelext->next->tried = 0;
                neutripelext->next->anfang = 0;
                neutripelext->next->ende = 0;
                neutripelext->next->p = 0;
                neutripelext->next->fixed = 0;
                neutripelext->next->next
                 = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelext->next->next->name = kantenzahl + 1;
                kantenzahl++;
                neutripelext->next->next->a = 270;
                neutripelext->next->next->s = 0;
                neutripelext->next->next->afix = 0;
                neutripelext->next->next->l = 0;
                neutripelext->next->next->dir = 0;
                neutripelext->next->next->bridge = 0;
                neutripelext->next->next->tried = 0;
                neutripelext->next->next->anfang = 0;
                neutripelext->next->next->ende = 0;
                neutripelext->next->next->p = 0;
                neutripelext->next->next->fixed = 0;
                neutripelext->next->next->next = hilf->next;
                hilf->a = 90;
                hilf->next = NULL;

                neutripelint = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelint->name = kantenzahl;
                neutripelint->a = 90;
                neutripelint->s = 0;
                neutripelint->afix = 0;
                neutripelint->l = 0;
                neutripelint->dir = 0;
                neutripelint->bridge = 0;
                neutripelint->tried = 0;
                neutripelint->anfang = 0;
                neutripelint->ende = 0;
                neutripelint->p = 0;
                neutripelint->fixed = 0;
                neutripelint->next 
                 = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelint->next->name = kantenzahl - 1;
                neutripelint->next->a = 90;
                neutripelint->next->s = 0;
                neutripelint->next->afix = 0;
                neutripelint->next->l = 0;
                neutripelint->next->dir = 0;
                neutripelint->next->bridge = 0;
                neutripelint->next->tried = 0;
                neutripelint->next->anfang = 0;
                neutripelint->next->ende = 0;
                neutripelint->next->p = 0;
                neutripelint->next->fixed = 0;
                neutripelint->next->next
                 = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelint->next->next->name = kantenzahl - 2;
                neutripelint->next->next->a = 90;
                neutripelint->next->next->s = 0;
                neutripelint->next->next->afix = 0;
                neutripelint->next->next->l = 0;
                neutripelint->next->next->dir = 0;
                neutripelint->next->next->bridge = 0;
                neutripelint->next->next->tried = 0;
                neutripelint->next->next->anfang = 0;
                neutripelint->next->next->ende = 0;
                neutripelint->next->next->p = 0;
                neutripelint->next->next->fixed = 0;
                neutripelint->next->next->next = naechster;
                hface->next = neutripelext;
                newgraph->next = (TGRAPH*) malloc(sizeof(TGRAPH));
                newgraph = newgraph->next;
                newgraph->next = NULL;
                newgraph->flaeche = (TFACE*) malloc(sizeof(TFACE));
                newflaeche = newgraph->flaeche;
                newflaeche->nummer = facezahl + 1;
                facezahl++;
                newflaeche->face = neutripelint;
                if (uebergang == 1)
                  {
                    hface->next = NULL;
                    hflaeche->face = neutripelext;
                  }  /*  if  */
              }  /*  modus = 1  */
            else if ((modus == 2)||(modus ==3))
              {
                neutripelext = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelext->name = kantenzahl + 1;
                kantenzahl++;
                neutripelext->a = 270;
                neutripelext->s = 0;
                neutripelext->afix = 0;
                neutripelext->l = 0;
                neutripelext->dir = 0;
                neutripelext->bridge = 0;
                neutripelext->tried = 0;
                neutripelext->anfang = 0;
                neutripelext->ende = 0;
                neutripelext->p = 0;
                neutripelext->fixed = 0;
                neutripelext->next 
                 = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelext->next->name = kantenzahl + 1;
                kantenzahl++;
                if (modus == 2)
                   neutripelext->next->a = 270;
                else if (modus == 3)
                   neutripelext->next->a = 180;
                neutripelext->next->s = 0;
                neutripelext->next->afix = 0;
                neutripelext->next->l = 0;
                neutripelext->next->dir = 0;
                neutripelext->next->bridge = 0;
                neutripelext->next->tried = 0;
                neutripelext->next->anfang = 0;
                neutripelext->next->ende = 0;
                neutripelext->next->p = 0;
                neutripelext->next->fixed = 0;
                neutripelext->next->next = hilf->next;
                hilf->a = 90;
                hilf->next = NULL;

                neutripelint = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelint->name = kantenzahl;
                neutripelint->a = 90;
                neutripelint->s = 0;
                neutripelint->afix = 0;
                neutripelint->l = 0;
                neutripelint->dir = 0;
                neutripelint->bridge = 0;
                neutripelint->tried = 0;
                neutripelint->anfang = 0;
                neutripelint->ende = 0;
                neutripelint->p = 0;
                neutripelint->fixed = 0;
                neutripelint->next 
                 = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelint->next->name = kantenzahl - 1;
                neutripelint->next->a = 90;
                neutripelint->next->s = 0;
                neutripelint->next->afix = 0;
                neutripelint->next->l = 0;
                neutripelint->next->dir = 0;
                neutripelint->next->bridge = 0;
                neutripelint->next->tried = 0;
                neutripelint->next->anfang = 0;
                neutripelint->next->ende = 0;
                neutripelint->next->p = 0;
                neutripelint->next->fixed = 0;
                neutripelint->next->next = naechster;
                hface->next = neutripelext;
                newgraph->next = (TGRAPH*) malloc(sizeof(TGRAPH));
                newgraph = newgraph->next;
                newgraph->next = NULL;
                newgraph->flaeche = (TFACE*) malloc(sizeof(TFACE));
                newflaeche = newgraph->flaeche;
                newflaeche->nummer = facezahl + 1;
                facezahl++;
                newflaeche->face = neutripelint;
                if (uebergang == 1)
                  {
                    hface->next = NULL;
                    hflaeche->face = neutripelext;
                  }  /*  if  */
              }  /* modus 2,3  */
            else if ((modus == 5)||(modus == 4))
              {
                neutripelext = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelext->name = kantenzahl + 1;
                kantenzahl++;
                neutripelext->a = 90;
                neutripelext->s = 0;
                neutripelext->afix = 0;
                neutripelext->l = 0;
                neutripelext->dir = 0;
                neutripelext->bridge = 0;
                neutripelext->tried = 0;
                neutripelext->anfang = 0;
                neutripelext->ende = 0;
                neutripelext->p = 0;
                neutripelext->fixed = 0;
                neutripelext->next = nexthilf;
                hername = nexthilf->name;
                neutripelext->next->name = kantenzahl + 1;
                if (hilf->next == NULL)
                  neutripelext->next = NULL;
                kantenzahl++;
                mache_hilfskante(kantenzahl,hername);
                hface->next = neutripelext;

                hilf->next = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                hilf->next->name = kantenzahl + 1;
                kantenzahl++;
                mache_hilfskante(kantenzahl,hername);
                hilf->next->a = 90;
                hilf->next->s = 0;
                hilf->next->afix = 0;
                hilf->next->l = 0;
                hilf->next->dir = 0;
                hilf->next->bridge = 0;
                hilf->next->tried = 0;
                hilf->next->anfang = 0;
                hilf->next->ende= 0;
                hilf->next->p = 0;
                hilf->next->fixed = 0;
                hilf->next->next 
                 = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                hilf->next->next->name = kantenzahl - 2;
                hilf->next->next->a = 90;
                hilf->next->next->s = 0;
                hilf->next->next->afix = 0;
                hilf->next->next->l = 0;
                hilf->next->next->dir = 0;
                hilf->next->next->bridge = 0;
                hilf->next->next->tried = 0;
                hilf->next->next->anfang = 0;
                hilf->next->next->ende= 0;
                hilf->next->next->p = 0;
                hilf->next->next->fixed = 0;
                hilf->next->next->next = NULL;
                newgraph->next = (TGRAPH*) malloc(sizeof(TGRAPH));
                newgraph = newgraph->next;
                newgraph->next = NULL;
                newgraph->flaeche = (TFACE*) malloc(sizeof(TFACE));
                newflaeche = newgraph->flaeche;
                newflaeche->nummer = facezahl + 1;
                facezahl++;
                newflaeche->face = naechster;
                if (uebergang == 1)
                  {
                    hface->next = NULL;
                    hflaeche->face = neutripelext;
                  }  /*  if  */
                ersetze(hername);
             }  /* modus 4,5  */
            else if (modus == 6)
              {
                neutripelext = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelext->name = kantenzahl + 1;
                kantenzahl++;
                neutripelext->a = 270;
                neutripelext->s = 0;
                neutripelext->afix = 0;
                neutripelext->l = 0;
                neutripelext->dir = 0;
                neutripelext->bridge = 0;
                neutripelext->tried = 0;
                neutripelext->anfang = 0;
                neutripelext->ende = 0;
                neutripelext->p = 0;
                neutripelext->fixed = 0;
                neutripelext->next = 
                   (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelext->next->name = kantenzahl + 1;
                kantenzahl++;
                neutripelext->next->a = 180;
                neutripelext->next->next = hilf->next;
                hilf->a = 90;
                hilf->next = NULL;

                neutripelint = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelint->name = kantenzahl;
                neutripelint->a = 90;
                neutripelint->s = 0;
                neutripelint->afix = 0;
                neutripelint->l = 0;
                neutripelint->dir = 0;
                neutripelint->bridge = 0;
                neutripelint->tried = 0;
                neutripelint->anfang = 0;
                neutripelint->ende = 0;
                neutripelint->p = 0;
                neutripelint->fixed = 0;
                neutripelint->next 
                 = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
                neutripelint->next->name = kantenzahl - 1;
                neutripelint->next->a = 90;
                neutripelint->next->s = 0;
                neutripelint->next->afix = 0;
                neutripelint->next->l = 0;
                neutripelint->next->dir = 0;
                neutripelint->next->bridge = 0;
                neutripelint->next->tried = 0;
                neutripelint->next->anfang = 0;
                neutripelint->next->ende = 0;
                neutripelint->next->p = 0;
                neutripelint->next->fixed = 0;
                neutripelint->next->next = naechster;
                newgraph->next = (TGRAPH*) malloc(sizeof(TGRAPH));
                newgraph = newgraph->next;
                newgraph->next = NULL;
                newgraph->flaeche = (TFACE*) malloc(sizeof(TFACE));
                newflaeche = newgraph->flaeche;
                newflaeche->nummer = facezahl + 1;
                facezahl++;
                newflaeche->face = neutripelint;
                hface->next = neutripelext;
                if (uebergang == 1)
                  {
                    hface->next = NULL;
                    hflaeche->face = neutripelext;
                  }  /*  if  */
              }  /* modus 6  */
          }  /*  if  */
       }   /*  while geaendert  */
      }    /*  else  , also externes Face  */
   hgraph = hgraph->next;
  }  /*  while  */
}  /*  mache_rechtecke  */





void mache_hilfskante (kante,herkunft) 
/*  traegt die neue Hilfskante kante mit ihrer Herkunft herkunft in
 die Liste hilfskantenmenge ein.  */

{
THILFSKANTENMENGE *hilf;
hilf = (THILFSKANTENMENGE *) malloc(sizeof(THILFSKANTENMENGE));
hilf->name = kante;
hilf->herkunft = herkunft;
hilf->next = hilfskantenmenge;
hilfskantenmenge = hilf;
return;

}  /*  mache_hilfskante  */



void ersetze(name)   /*  Hilfsprocedur von mache_rechtecke  */
/*  sucht die Kante name in graph und ersetzt sie durch zwei
   Kanten kantenzahl - 1 und kantenzahl.  */
{
TGRAPH *hilfsgraph;
TKANTENTRIPEL *hilfsface,*hilf;
int gefunden = 0;
int winkel;
hilfsgraph = graph;
while ((gefunden == 0)&&(hilfsgraph != NULL))
  {
    hilfsface = hilfsgraph->flaeche->face;
    while ((gefunden == 0)&&(hilfsface != NULL))
      {
        if (hilfsface->name == name)
          {
             gefunden = 1;
             winkel = hilfsface->a;
             hilfsface->name = kantenzahl - 1;
             hilfsface->a = 180;
             hilf = hilfsface->next;
             hilfsface->next = (TKANTENTRIPEL*) malloc(sizeof(TKANTENTRIPEL));
             hilfsface->next->name = kantenzahl;
             hilfsface->next->a = winkel;
             hilfsface->next->s = 0;
             hilfsface->next->l = 0;
             hilfsface->next->afix = 0;
             hilfsface->next->dir = 0;
             hilfsface->next->bridge = 0;
             hilfsface->next->tried = 0;
             hilfsface->next->anfang = 0;
             hilfsface->next->ende = 0;
             hilfsface->next->p = 0;
             hilfsface->next->fixed = 0;
             hilfsface->next->next = hilf;
          }   /*  if  */
        hilfsface = hilfsface->next;
      }  /*  while  */
    hilfsgraph = hilfsgraph->next;
  }  /*  while  */
return;
}  /*  ersetze  */




void kompaktifiziere()  /*  Hilfsprocedur von finde_laengen  */
/*  Hier werden den Begrenzungslinien der Rechtecke Laengen zugewiesen.  */
{
void linit();
void erhoehe();
TGRAPH *hgraph;
TKANTENTRIPEL *hface,*hkante1,*hkante2,*hkante3,*hkante4;
int sum1,sum2,sum3,sum4,richtung,name;
linit();
hgraph = graph;
while (hgraph != NULL)
  {
    if ((hgraph->flaeche->nummer == 1)&&(facezahl > 1))
      hgraph = hgraph->next;
    hkante1 = NULL;
    hkante2 = NULL;
    hkante3 = NULL;
    hkante4 = NULL;
    richtung = 1;
    sum1 = 0;
    sum2 = 0;
    sum3 = 0;
    sum4 = 0;
    hface = hgraph->flaeche->face;
    while (hface != NULL)
      {
        if (richtung > 4)
          richtung -= 4;
        if (richtung == 1)
         {
          sum1 += hface->l;
          if (hkante1 == NULL)
            hkante1 = hface;
            else if (hkante1->l > hface->l)
              hkante1 = hface;
         }
        else if (richtung == 2)
         {
          sum2 += hface->l;
          if (hkante2 == NULL)
            hkante2 = hface;
            else if (hkante2->l > hface->l)
              hkante2 = hface;
         }
        else if (richtung == 3)
         {
          sum3 += hface->l;
          if (hkante3 == NULL)
            hkante3 = hface;
            else if (hkante3->l > hface->l)
              hkante3 = hface;
         }
        else if (richtung == 4)
         {
          sum4 += hface->l;
          if (hkante4 == NULL)
            hkante4 = hface;
            else if (hkante4->l > hface->l)
              hkante4 = hface;
         }
        if (hface->a == 90)
          richtung++;
        else if (hface->a == 270)
          {
            if (richtung == 1)
              richtung = 4;
            else richtung--;
          }
        hface = hface->next;
      }   /*  while  */
    if (sum1 < sum3)
      {
        name = hkante1->name;
        erhoehe(name);
        hgraph = graph;
      }
    else if (sum3 < sum1)
      {
        name = hkante3->name;
        erhoehe(name);
        hgraph = graph;
      }
    else if (sum2 < sum4)
      {
        name = hkante2->name;
        erhoehe(name);
        hgraph = graph;
      }
    else if (sum4 < sum2)
      {
        name = hkante4->name;
        erhoehe(name);
        hgraph = graph;
      }
    else hgraph = hgraph->next;
  }  /*  while  */
}  /*  kompaktifiziere  */



void linit()   /*  Hilfsprocedur von kompaktifiziere  */
/*   Alle Laengen werden auf 1 gesetzt.  */
{
TGRAPH *hgraph;
TKANTENTRIPEL *hface;
hgraph = graph;
while (hgraph != NULL)
  {
    hface = hgraph->flaeche->face;
    while (hface != NULL)
     {
       hface->l = 1;
       hface = hface->next;
     }  /*  while  */
    hgraph = hgraph->next;
  }   /*  while  */
}  /*  linit  */




void erhoehe(name)   /*  Hilfsprocedur von kompaktifiziere  */
/*  Die beiden Kanten mit dem Namen name werden um 1 verlaengert.  */
{
TGRAPH *hgraph;
TKANTENTRIPEL *hface;
int zaehler = 0;
hgraph = graph;
while ((hgraph != NULL)&&(zaehler < 2))
  {
    hface = hgraph->flaeche->face;
    while ((hface != NULL)&&(zaehler < 2))
      {
        if (hface->name == name)
          {
            zaehler++;
            hface->l++;
          }  /*  if  */
        hface = hface->next;
      }   /*  while  */
    hgraph = hgraph->next;
  }  /*  while  */
return;
}  /*  erhoehe  */





void kuerze()   /*  Hilfsprocedur von finde_laengen.  */
/*  Versucht, Kanten zu verkuerzen, die durch schlechte Laengenzuweisung
    zu lang geraten sind.  */
{
int try();
int ermittle_facenummer();
TKANTENTRIPEL *liste;
int a,face;
liste = graph->flaeche->face;
while (liste != NULL)
  {
    a = 0;
    while ((a == 0)&&(liste != NULL))
      {
        face = ermittle_facenummer(liste->name,1,1);
        a = try(liste->name,liste->l,face,1);
        if (a == 0)
           liste = liste->next;
      }  /*  while  */
  }  /*  while  */
}  /*  kuerze */





void kuerze_laengengraph()
/*  Hilfsprocedur von finde_laengen  */
/*  Versucht, im Laengengraph Kanten zu verkuerzen, die im Graph
   ( rechteckig ) nicht zu kuerzen waren.  */
{
TZAHLLISTE *try_laengengraph();
int ermittle_facenummer();
int pruefe();
TGRAPH *hgraph;
TKANTENTRIPEL *liste;
TKNOTENMENGE *hknotenmenge;
TZAHLLISTE *kuerzliste,*freeliste;
int a,face,xwert,ywert,facenummer,gekuerzt,xmax,ymax;
kuerzliste = (TZAHLLISTE *) malloc(sizeof(TZAHLLISTE));
gekuerzt = 1;
while (gekuerzt == 1)
  {
    gekuerzt = 0;
    hgraph = laengengraph;
    while (hgraph != NULL)
      {
        facenummer = hgraph->flaeche->nummer;
        liste = hgraph->flaeche->face;
        while (liste != NULL)
          {
             a = 0;
             kuerzliste->zahl = 0;
             kuerzliste->next = NULL;
             face = ermittle_facenummer(liste->name,facenummer,2);
             kuerzliste = try_laengengraph(liste,face,facenummer);
             freeliste = kuerzliste;
             if (kuerzliste->zahl == 1)
                {
                   kuerzliste = kuerzliste->next;
                   a = pruefe(kuerzliste);
                }
               else a = 0;
             while (freeliste != NULL)
               {
                  /*  free(freeliste);  */
                  freeliste = freeliste->next;
               }
             if (a == 0)
                liste = liste->next;
               else gekuerzt = 1;
           }  /*  while  */
           hgraph = hgraph->next;
       }  /*  while  */
  }  /*  while gekuerzt  */
xwert = 0;
ywert = 0;
hknotenmenge = knotenmenge;
while (hknotenmenge != NULL)
  {
    if (hknotenmenge->xpos < xwert)
      xwert = hknotenmenge->xpos;
    if (hknotenmenge->ypos < ywert)
      ywert = hknotenmenge->ypos;
    hknotenmenge = hknotenmenge->next;
  }  /*  while  */
if ((xwert < 0)||(ywert < 0))
   {
     hknotenmenge = knotenmenge;
     while (hknotenmenge != NULL)
      {
        hknotenmenge->xpos += xwert;
        hknotenmenge->ypos += ywert;
        hknotenmenge = hknotenmenge->next;
      }
   }  /*  if  */
xmax = 0;
ymax = 0;
hknotenmenge = knotenmenge;
while (hknotenmenge != NULL)
  {
    if (hknotenmenge->xpos > xmax)
      xmax = hknotenmenge->xpos;
    if (hknotenmenge->ypos > ymax)
      ymax = hknotenmenge->ypos;
    hknotenmenge = hknotenmenge->next;
  }  /*  while */
if (xmax > ymax)
  Maxeinheit2 = xmax;
  else Maxeinheit2 = ymax;
}  /*  kuerze_laengengraph  */






int pruefe(kuerzliste)
/*  Hilfsprocedur von kuerze_laengengraph  */
/*  Ueberprueft, ob die vorgeschlagene Kuerzung moeglich ist,
    und fuehrt sie eventuell durch.   */
TZAHLLISTE *kuerzliste;
{
void mache_teilgraphen();
int mache_matrix();
TKANTENMENGE *hkantenmenge;
TKNOTENMENGE *hknotenmenge;
TZAHLLISTE *hkuerzliste;
TGRAPH *hgraph;
TKANTENTRIPEL *hface;
int fehler,zaehler,kante,anfknoten,endknoten,x1,x2,y1,y2,posfehler;
hkantenmenge = kantenmenge;
while (hkantenmenge != NULL)
  {
    hkantenmenge->schnitt = 0;
    hkantenmenge = hkantenmenge->next;
  }
hknotenmenge = knotenmenge;
while (hknotenmenge != NULL)
  {
    hknotenmenge->teil = 2;
    hknotenmenge->merkx = 0;
    hknotenmenge->merky = 0;
    hknotenmenge = hknotenmenge->next;
  }  /*  while */
hkuerzliste = kuerzliste;
while (hkuerzliste != NULL)
  {
    hkantenmenge = kantenmenge;
    while (hkantenmenge->nummer != hkuerzliste->zahl)
     hkantenmenge = hkantenmenge->next;
    hkantenmenge->schnitt = 1;
    hkuerzliste = hkuerzliste->next;
  }  /*  while  */
kante = kuerzliste->zahl;
hkantenmenge = kantenmenge;
while (hkantenmenge->nummer != kante)
  hkantenmenge = hkantenmenge->next;
anfknoten = hkantenmenge->anfang;
endknoten = hkantenmenge->ende;
mache_teilgraphen(anfknoten);
hknotenmenge = knotenmenge;
while ((hknotenmenge->nummer != anfknoten)&&(hknotenmenge->nummer != endknoten))
  hknotenmenge = hknotenmenge->next;
if (hknotenmenge->nummer == anfknoten)
  {
     x1 = hknotenmenge->xpos;
     y1 = hknotenmenge->ypos;
  }
  else
  {
     x2 = hknotenmenge->xpos;
     y2 = hknotenmenge->ypos;
  }
hknotenmenge = hknotenmenge->next;
while ((hknotenmenge->nummer != anfknoten)&&(hknotenmenge->nummer != endknoten))
  hknotenmenge = hknotenmenge->next;
if (hknotenmenge->nummer == anfknoten)
  {
     x1 = hknotenmenge->xpos;
     y1 = hknotenmenge->ypos;
  }
  else
  {
     x2 = hknotenmenge->xpos;
     y2 = hknotenmenge->ypos;
  }
hknotenmenge = knotenmenge;
posfehler = 0;
if (x1 == x2)
  {
    if (y1 < y2)   /*  Fall c  */
     {
       while (hknotenmenge != NULL)
         {
           if (hknotenmenge->teil == 1)
             {
               hknotenmenge->ypos++;
               hknotenmenge->merky = 1;
             }
           hknotenmenge = hknotenmenge->next;
         }
     }
    else   /*  Fall d   */
     {
       while (hknotenmenge != NULL)
         {
           if (hknotenmenge->teil == 1)
             {
               if (hknotenmenge->ypos == 0)
                  posfehler = 2;
               hknotenmenge->ypos--;
               hknotenmenge->merky = -1;
             }
           hknotenmenge = hknotenmenge->next;
         }  /*  while  */
     }  /*  else  */
  }
  else
  {
    if ( x1 < x2 )  /*  Fall a  */
     {
       while (hknotenmenge != NULL)
         {
           if (hknotenmenge->teil == 1)
             {
               hknotenmenge->xpos++;
               hknotenmenge->merkx = 1;
             }
           hknotenmenge = hknotenmenge->next;
         }  /*  while  */
     }
     else  /*  Fall b  */
     {
       while (hknotenmenge != NULL)
         {
           if (hknotenmenge->teil == 1)
             {
               if (hknotenmenge->xpos == 0)
                  posfehler = 1;
               hknotenmenge->xpos--;
               hknotenmenge->merkx = -1;
             }
           hknotenmenge = hknotenmenge->next;
         }  /*  while  */
     }
  }  /*  else  */
if (posfehler != 0)
  {
     hknotenmenge = knotenmenge;
     while (hknotenmenge != NULL)
       {
         if (posfehler == 1)
            hknotenmenge->xpos++;
           else if (posfehler == 2)
            hknotenmenge->ypos++;
         hknotenmenge = hknotenmenge->next;
       }  /*  while  */
  }  /*  if  */
fehler = mache_matrix();
if (fehler == 1)
  {
    hknotenmenge = knotenmenge;
    while (hknotenmenge != NULL)
     {
        hknotenmenge->xpos -= hknotenmenge->merkx;
        hknotenmenge->ypos -= hknotenmenge->merky;
        hknotenmenge->merkx = 0;
        hknotenmenge->merky = 0;
        hknotenmenge = hknotenmenge->next;
     }   /*  while  */
  }  /*  if  */
 else   /*  Vermindere ...->l  */
  {
    hkuerzliste = kuerzliste;
    while (hkuerzliste != NULL)
     {
       zaehler = 0;
       hgraph = laengengraph;
       while ((hgraph != NULL)&&(zaehler < 2))
         {
           hface = hgraph->flaeche->face;
           while ((hface != NULL)&&(zaehler < 2))
             {
                if (hface->name == hkuerzliste->zahl)
                   {
                      zaehler++;
                      hface->l--;
                   }
                hface = hface->next;
             }  /*  while  */
           hgraph = hgraph->next;
         }   /*  while  */
       hkuerzliste = hkuerzliste->next;
     }  /*  while  */
  }
if (fehler == 1)
  return(0);
  else return(1);
}  /*  pruefe  */




void mache_teilgraphen(knoten)
/*  Hilfsprocedur von pruefe  */
/*  Durch Zerschneiden von zu kuerzenden Kanten wird der Graph in zwei
    separate Teilgraphen unterteilt. Die Punkte werden entsprechend in
    knotenmenge mit 1 bzw. 2 markiert.    */
{
TKNOTENMENGE *hknotenmenge;
TKANTENMENGE *hkantenmenge;
int hknoten;
hknotenmenge = knotenmenge;
while (hknotenmenge->nummer != knoten)
  hknotenmenge = hknotenmenge->next;
hknotenmenge->teil = 1;
hkantenmenge = kantenmenge;
while (hkantenmenge != NULL)
  {
   if (hkantenmenge->schnitt == 0)
    {
    if ((hkantenmenge->anfang == knoten)||(hkantenmenge->ende == knoten))
      {
        if (hkantenmenge->anfang == knoten)
          hknoten = hkantenmenge->ende;
          else hknoten = hkantenmenge->anfang;
        hknotenmenge = knotenmenge;
        while (hknotenmenge->nummer != hknoten)
          hknotenmenge = hknotenmenge->next;
        if (hknotenmenge->teil != 1)
          {
             mache_teilgraphen(hknoten);
          }  /*  if  */
      }  /*  if  */
    }  /*  if  */
    hkantenmenge = hkantenmenge->next;
  }  /*  while  */
}  /*  mache_teilgraphen  */






TZAHLLISTE *try_laengengraph(pointer,face,endnummer) 
    /*  Hilfsprocedur von kuerze_laengengraph */
/*  Es wird festgestellt, ob die Kante kante kuerzbar ist.
    Falls ja, so wird eine Liste der zu kuerzenden Kanten zurueckgegeben. 
    endnummer ist die Nummer des Segments, das wieder erreicht werden muss,
    haeufig das externe Segment.    */
TKANTENTRIPEL *pointer;
{
TZAHLLISTE *kuerzliste,*freeliste;
int ermittle_facenummer();
TKANTENTRIPEL *mache_liste_laengengraph();
TGRAPH *hgraph,*g;
int zaehler,facenummer,ergebnis,kante,laenge,richtung,gefunden;
TKANTENTRIPEL *liste,*hface,*p;
TZAHLLISTE *hilf;
  g = laengengraph;
  gefunden = 0;
  while ((g != NULL)&&(gefunden == 0))
    {
      p = g->flaeche->face;
      while ((p != NULL)&&(gefunden == 0))
       {
          if ((p->name == pointer->name)&&(p->dir == pointer->dir))
             gefunden = 1;
           else
          p = p->next;
       }
      g = g->next;
    }
  p->tried = 1;
  kante = pointer->name;
  laenge = pointer->l;
  richtung = pointer->dir;
  kuerzliste = NULL;
  if (laenge <= 1)
     ergebnis = 0;
     else if (face == endnummer)  /*  extern  */
         ergebnis = 1; else
    {
      liste = mache_liste_laengengraph(kante,face,richtung);
      ergebnis = 0;
      while ((ergebnis == 0)&&(liste != NULL))
        {
          facenummer = ermittle_facenummer(liste->name,face,2);
          kuerzliste = 
             try_laengengraph(liste,facenummer,endnummer);
          ergebnis = kuerzliste->zahl;
          /*  free(liste);  */
          liste = liste->next;
        }  /*  while  */
    }
  if (ergebnis == 1)
    {
      if (kuerzliste == NULL)
        {
          kuerzliste = (TZAHLLISTE*) malloc(sizeof(TZAHLLISTE));
          kuerzliste->zahl = 1;
          kuerzliste->next = NULL;
        }
      hilf = (TZAHLLISTE*) malloc(sizeof(TZAHLLISTE));
      hilf->zahl = kante;
      hilf->next = kuerzliste->next;
      kuerzliste->next = hilf;
      p->tried = 0;
    }
   else
    {
          kuerzliste = (TZAHLLISTE*) malloc(sizeof(TZAHLLISTE));
          kuerzliste->zahl = 0;
          kuerzliste->next = NULL;
    }
  return(kuerzliste);
}  /*  try_laengengraph  */






int ermittle_facenummer(kante,face,modus)  
         /*  Hilfsprocedur von kuerze und try und bestimme_bruecken */
/* Berechnet wird die Nummer des Faces <> face, das die Kante kante enthaelt. */
{
TGRAPH *hgraph;
TKANTENTRIPEL *hface;
int ergebnis;
int gefunden;
if (modus == 1)
   hgraph = graph;
  else hgraph = laengengraph;
gefunden = 0;
while ((gefunden == 0)&&(hgraph != NULL))
  {
    if (hgraph->flaeche->nummer == face)
       hgraph = hgraph->next;
    if (hgraph != NULL)
      {
        hface = hgraph->flaeche->face;
        while ((hface != NULL)&&(gefunden == 0))
           {
            if (hface->name == kante)
              {
                gefunden = 1;
                ergebnis = hgraph->flaeche->nummer;
              }
            hface = hface->next;
           }  /*  while  */
        hgraph = hgraph->next;
       }  /*  if  */
  }  /*  while  */
if (gefunden == 0)
  ergebnis = face;
return(ergebnis);
}   /*  ermittle_facenummer  */






int try(kante,laenge,face)  /*  Hilfsprocedur von kuerze */
/*  Es wird festgestellt, ob die Kante kante kuerzbar ist.
    Falls ja, so werden alle entsprechenden Laengen um 1 gekuerzt.  */
{
int ermittle_facenummer();
TKANTENTRIPEL *mache_liste();
TGRAPH *hgraph;
int zaehler,facenummer,ergebnis;
TKANTENTRIPEL *liste,*hface;
  if (laenge <= 1)
     ergebnis = 0;
     else if (face == 1)  /*  extern  */
         ergebnis = 1; else
    {
      liste = mache_liste(kante,face);
      ergebnis = 0;
      while ((ergebnis == 0)&&(liste != NULL))
        {
          facenummer = ermittle_facenummer(liste->name,face,1);
          ergebnis = try(liste->name,liste->l,facenummer);
          /*  free(liste);  */
          liste = liste->next;
        }  /*  while  */
    }
  if (ergebnis == 1)
    {
      hgraph = graph;
      hface = hgraph->flaeche->face;
      zaehler = 0;
      while (zaehler < 2)
        {
          if (hface->name == kante)
             {
                hface->l--;
                zaehler++;
             }
          if (hface->next != NULL)
             hface = hface->next;
           else 
             {
               hgraph = hgraph->next;
               if (hgraph != NULL)
                 hface = hgraph->flaeche->face;
             }
        }  /*  while  */
    }
  return(ergebnis);
}  /*  try  */




TKANTENTRIPEL *mache_liste(kante,face)  /*  Hilfsprocedur von try  */
/*  Erstellt eine Liste der der Kante kante im Face face 
    gegenueberliegenden Kanten.  */
{
TKANTENTRIPEL *liste;
TGRAPH *hgraph;
TKANTENTRIPEL *hface,*new;
int zaehler;
liste = NULL;
hgraph = graph;
while (hgraph->flaeche->nummer != face)
  hgraph = hgraph->next;
zaehler = 0;
hface = hgraph->flaeche->face;
while (hface->name != kante)
  hface = hface->next;
while (zaehler < 2 )
  {
    if(hface->a != 180)
      zaehler++;
    if (hface->next != NULL)
       hface = hface->next;
      else hface = hgraph->flaeche->face;
  }  /*  while  */
while (hface->a == 180)
  {
    new = (TKANTENTRIPEL *) malloc(sizeof(TKANTENTRIPEL));
    new->name = hface->name;
    new->l = hface->l;
    new->next = liste;
    liste = new;
    if (hface->next != NULL)
      hface = hface->next;
      else hface = hgraph->flaeche->face;
  }  /*  while  */
new = (TKANTENTRIPEL *) malloc(sizeof(TKANTENTRIPEL));
new->name = hface->name;
new->l = hface->l;
new->next = liste;
liste = new;
return(liste);
}  /*  mache_liste  */





TKANTENTRIPEL *mache_liste_laengengraph(kante,face,richtung)  
/*  Hilfsprocedur von try_laengengraph  */
/*  Erstellt eine Liste der der Kante kante im Face face 
    gegenueberliegenden Kanten.  */
{
TKANTENTRIPEL *liste;
TGRAPH *hgraph;
TKANTENTRIPEL *hface,*new;
int hrichtung;
liste = NULL;
hgraph = laengengraph;
while (hgraph->flaeche->nummer != face)
  hgraph = hgraph->next;
hface = hgraph->flaeche->face;
while (hface != NULL)
  {
    hrichtung = hface->dir;
    if ((hrichtung == richtung)&&(hface->bridge == 0)&&(hface->tried == 0))
      {
        new = (TKANTENTRIPEL *) malloc(sizeof(TKANTENTRIPEL));
        new->name = hface->name;
        new->l = hface->l;
        new->dir = hface->dir;
        new->bridge = hface->bridge;
        new->a = hface->a;
        new->tried = hface->tried;
        new->next = liste;
        liste = new;
      }  /*  if  */
    hface = hface->next;
  }  /*  while  */
return(liste);
}  /*  mache_liste_laengengraph   */






void bestimme_bruecken()
/*  Hilfsprocedur von finde_laengen  */
/*  In laengengraph wird fuerjede Kante eingetragen, ob sie Teil einer Bruecke
    ist oder nicht.   */
{
int ermittle_facenummer();
TGRAPH *hgraph;
TKANTENTRIPEL *hface;
int face1,face2,name;
hgraph = graph;
while (hgraph != NULL)
  {
    hface = hgraph->flaeche->face;
    face1 = hgraph->flaeche->nummer;
    while (hface != NULL)
      {
         name = hface->name;
         face2 = ermittle_facenummer(name,face1,1);
         if (face1 == face2)
           hface->bridge = 1;
          else hface->bridge = 0;
         hface = hface->next;
      }  /*  while  */
    hgraph = hgraph->next;
  }  /*  while  */
}  /*  bestimme_bruecken  */







void berechne_laengen()  /*  Hilfsprocedur von finde_laengen  */
/*  Aus den Daten des Laengengraphen werden die Laengen des Originalgraphen
   berechnet  */
{
TGRAPH *hilfsgraph;
TKANTENTRIPEL *hilfsface;
int name,laenge,herkunft;
void addierelaenge();
int suche_herkunft();
void dividiere();
hilfsgraph = graph;
while (hilfsgraph != NULL)
  {
    hilfsface = hilfsgraph->flaeche->face;
    while (hilfsface != NULL)
      {
        name = hilfsface->name;
        laenge = hilfsface->l;
        if (name <= originalkantenzahl)
          {
             addierelaenge(name,laenge);
          }
         else 
          {
             herkunft = suche_herkunft(name);
             while ((herkunft != 0)&&(herkunft > originalkantenzahl))
               herkunft = suche_herkunft(herkunft);
             if (herkunft > 0)
               {
                 addierelaenge(herkunft,laenge);
               }   /*  if  */
          }   /*  else  */
        hilfsface = hilfsface->next;
      }  /*  while  */
    hilfsgraph = hilfsgraph->next;
  }  /*  while  */
dividiere();
}  /*  berechne_laengen  */




void addierelaenge(name,laenge)   /*  Hilfsprocedur von berechne_laengen  */
/*  sucht in laengengraph die Kante name und addiert zu ihrer Laenge
   laenge  */
{
TGRAPH *hilfsgraph;
TKANTENTRIPEL *hilfsface;
int gefunden = 0;
hilfsgraph = laengengraph;
while ((hilfsgraph != NULL)&&(gefunden < 2))
  {
    hilfsface = hilfsgraph->flaeche->face;
    while ((hilfsface != NULL)&&(gefunden < 2))
      {
         if (hilfsface->name == name)
           {
              hilfsface->l += laenge;
              gefunden++;
           }  /*  if  */
         hilfsface = hilfsface->next;
      }  /*  while   */
    hilfsgraph = hilfsgraph->next;
  }  /*  while  */
return;
}  /*  addierelaenge  */



int suche_herkunft(name)   /*  hilfsprocedur von berechne_laengen  */
/*  sucht in hilfskantenmenge, ob die Kante name vorkommt.
   Wenn ja: herkunft wird zurueckgegeben.
   Wenn nein: 0 wird zurueckgegeben.  */
{
THILFSKANTENMENGE *hilfsmenge;
int gefunden = 0;
int ergebnis = 0;
hilfsmenge = hilfskantenmenge;
while ((hilfsmenge != NULL)&&(gefunden == 0))
  {
    if (hilfsmenge->name == name)
      {
       gefunden = 1;
       ergebnis = hilfsmenge->herkunft;
      }
    hilfsmenge = hilfsmenge->next;
  }  /*  while  */
return(ergebnis);
}  /*  suche_herkunft  */




void dividiere()   /*  Hilfsprocedur von berechne_laengen  */
/*  Teilt alle Laengen in laengengraph durch 2.  */
{
TGRAPH *hilfsgraph;
TKANTENTRIPEL *hilfsface;
hilfsgraph = laengengraph;
while (hilfsgraph != NULL)
  {
     hilfsface = hilfsgraph->flaeche->face;
     while (hilfsface != NULL)
       {
         hilfsface->l = hilfsface->l/2;
         hilfsface = hilfsface->next;
       }  /*  while  */
     hilfsgraph = hilfsgraph->next;
  }   /*  while  */
return;
}  /*  dividiere  */







void berechne_position()    /*  Hilfsprocedur von finde_laengen */
/*  Bestimmt zu jedem Knoten seine Position.  */
{
TGRAPH *hgraph,*hlaengengraph;
TKANTENTRIPEL *hface,*hface2,*erster;
TKNOTENMENGE *hknotenmenge;
int gefunden2,x,x1,x2,y,y1,y2,nameend,nameanf,fertig;
int anfang1,ende1,anfang2,ende2,minx,miny;
int gefunden = 0;
int richtung,gefunden1;
int zaehler = 1;
knotenmenge->xpos = 0;
knotenmenge->ypos = 0;
hlaengengraph = laengengraph;
gefunden = 0;
while ( zaehler < knotenzahl)
  {
    hface = hlaengengraph->flaeche->face;
    erster = hface;
    gefunden = 0;
    while ((hface != NULL)&&(gefunden == 0))
      {
        anfang1 = hface->anfang;
        ende1 = hface->ende;
        hknotenmenge = knotenmenge;
        gefunden2 = 0;
        while (gefunden2 < 2)
           {
             if (((hknotenmenge->nummer == anfang1)
                 ||(hknotenmenge->nummer == ende1))
               &&(gefunden2 == 0))
               {
                  x1 = hknotenmenge->xpos;
                  y1 = hknotenmenge->ypos;
                  gefunden2++;
               }
             else if (((hknotenmenge->nummer == anfang1)
                      ||(hknotenmenge->nummer == ende1))
                     &&(gefunden2 == 1))
               {
                   x2 = hknotenmenge->xpos;
                   y2 = hknotenmenge->ypos;
                   gefunden2++;
               }
             if (hknotenmenge != NULL)
               hknotenmenge = hknotenmenge->next;
           }  /*  while  */
        if (((x1 != -100)&&(x2 == -100))||((x1 == -100)&&(x2 != -100)))
          gefunden = 1;
         else hface = hface->next;
      }   /*  while  */
    if (gefunden == 1)
      {
        zaehler++;
        if ( x1 == -100 )
          x = x2;
          else x = x1;
        if (y1 == -100)
          y = y2;
          else y = y1;
        richtung = hface->dir;
        if (hface->next == NULL)
          hface2 = erster;
          else hface2 = hface->next;
        anfang2 = hface2->anfang;
        ende2 = hface2->ende;
        if (((ende1 == anfang2)||(ende1 == ende2))
           &&((anfang1 != anfang2)&&(anfang1 != ende2)))
          {
            nameanf = anfang1;
            nameend = ende1;
          }
          else if (((anfang1 == anfang2)||(anfang1 == ende2))
                  &&((ende1 != anfang2)&&(ende1 != ende2)))
          {
            nameanf = ende1;
            nameend = anfang1;
          }
          else   /*  Bruecke, hface->name = hface2->name!!!  */
            {
              if (hface2->next == NULL)
                hface2 = erster;
                else hface2 = hface2->next;
              anfang2 = hface2->anfang;
              ende2 = hface2->ende;
           if ((ende1 == anfang2)||(ende1 == ende2))
             {
               nameanf = ende1;
               nameend = anfang1;
             }
             else
             {
               nameanf = anfang1;
               nameend = ende1;
             }
            }
        hknotenmenge = knotenmenge;
        fertig = 0;
        while (fertig == 0)
          {
             if ((hknotenmenge->nummer == nameanf)&&
                (hknotenmenge->xpos == -100))
               {
                 fertig = 1;
                 if (richtung == 1)
                   {
                      hknotenmenge->ypos = y;
                      hknotenmenge->xpos = (x - hface->l);
                   }
                 else if (richtung == 2)
                   {
                      hknotenmenge->xpos = x;
                      hknotenmenge->ypos = (y - hface->l);
                   }
                 else if (richtung == 3)
                   {
                      hknotenmenge->ypos = y;
                      hknotenmenge->xpos = (x + hface->l);
                   }
                 else /*  richtung == 4  */
                   {
                      hknotenmenge->xpos = x;
                      hknotenmenge->ypos = (y + hface->l);
                   }
               }
             else 
             if ((hknotenmenge->nummer == nameend)&&
                (hknotenmenge->xpos == -100))
               {
                 fertig = 1;
                 if (richtung == 1)
                   {
                      hknotenmenge->ypos = y;
                      hknotenmenge->xpos = (x + hface->l);
                   }
                 else if (richtung == 2)
                   {
                      hknotenmenge->xpos = x;
                      hknotenmenge->ypos = (y + hface->l);
                   }
                 else if (richtung == 3)
                   {
                      hknotenmenge->ypos = y;
                      hknotenmenge->xpos = (x - hface->l);
                   }
                 else /*  richtung == 4  */
                   {
                      hknotenmenge->xpos = x;
                      hknotenmenge->ypos = (y - hface->l);
                   }
               }
             else hknotenmenge = hknotenmenge->next;
          }  /*  while  */
      }   /*  gefunden == 1  */
    if (hlaengengraph->next == NULL)
      hlaengengraph = laengengraph;
      else hlaengengraph = hlaengengraph->next;
  }  /*  while  */
minx = 0;
miny = 0;
hknotenmenge = knotenmenge;
while (hknotenmenge != NULL)
  {
     if (hknotenmenge->xpos < minx)
       minx = hknotenmenge->xpos;
     if (hknotenmenge->ypos < miny)
       miny = hknotenmenge->ypos;
     hknotenmenge = hknotenmenge->next;
  }  /*  while  */
hknotenmenge = knotenmenge;
while (hknotenmenge != NULL)
  {
     hknotenmenge->xpos -= minx;
     hknotenmenge->ypos -= miny;
     hknotenmenge = hknotenmenge->next;
  }  /*  while  */
}  /*  berechne_position  */






int mache_matrix()
/*  Hilfsprocedur von pruefe   */
/*  Es wird die Koordinatenmatrix der ersten Loesung erstellt.  */
{
TKNOTENMENGE *hknotenmenge;
TKANTENMENGE *hkantenmenge;
int x,y,x1,x2,xx,y1,y2,yy,anf,ende,zaehler,fehler,i,j;
hknotenmenge = knotenmenge;
fehler = 0;
for (i=0;i<max1;i++)
  for (j=0;j<max2;j++)
    matrix[i][j] = 0;
while (hknotenmenge != NULL)
  {
    x = hknotenmenge->xpos;
    y = hknotenmenge->ypos;
    if (matrix[x][y] != 0)
      fehler = 1;
    matrix[x][y] = hknotenmenge->nummer;
    hknotenmenge = hknotenmenge->next;
  }  /*  while  */
hkantenmenge = kantenmenge;
while (hkantenmenge != NULL)
  {
     anf = hkantenmenge->anfang;
     ende = hkantenmenge->ende;
     hknotenmenge = knotenmenge;
     zaehler = 0;
     while (zaehler < 2)
       {
          if ((hknotenmenge->nummer == anf)||(hknotenmenge->nummer == ende))
             {
                zaehler++;
                if (hknotenmenge->nummer == anf)
                   {
                     x1 = hknotenmenge->xpos;
                     y1 = hknotenmenge->ypos;
                   }
                 else
                   {
                     x2 = hknotenmenge->xpos;
                     y2 = hknotenmenge->ypos;
                   }
             }   /*  if  */
          if (hknotenmenge != NULL)
             hknotenmenge = hknotenmenge->next;
       }   /*  while  */
     if (x1 == x2)
       {
         if (y1 < y2)
           {
             for (yy = (y1 + 1);yy < y2;yy++)
               {
                  if (matrix[x1][yy] != 0)
                     fehler = 1;
                  matrix[x1][yy] = -1;
               }
           }  /*  if  */
            else
           {
             for (yy = (y2 + 1);yy < y1;yy++)
              {
               if (matrix[x1][yy] != 0)
                  fehler = 1;
               matrix[x1][yy] = -1;
              }
           }
       }
      else
       {
          if (x1 < x2)
            {
             for (xx = (x1 + 1);xx < x2;xx++)
              {
               if (matrix[xx][y1] != 0)
                 fehler = 1;
               matrix[xx][y1] = -1;
              }
            }
           else
            {
             for (xx = (x2 + 1);xx < x1;xx++)
              {
               if (matrix[xx][y1] != 0)
                  fehler = 1;
               matrix[xx][y1] = -1;
              }
            }
       }
     hkantenmenge = hkantenmenge->next;
  }  /*  while  */
return(fehler);
}  /*  mache_matrix  */






void berechne_knicke()    /*  Hilfsprocedur von finde_laengen  */
/*  Statt der Dummyknoten werden Kantenzuege mit Knicken eingefuehrt.  */
{
int bestimme_vorgaenger();
int bestimme_nachfolger();
TKNICKLISTE *bestimme_knickliste();
void neukante();
TKNOTENMENGE *hknotenmenge;
TKNICKLISTE *knickliste1,*knickliste2,*hknickliste,*hilf;
int name,vorgaenger,nachfolger;
hknotenmenge = knotenmenge;
while (hknotenmenge != NULL)
  {
     if (hknotenmenge->echt == 0)
       {
         name = hknotenmenge->nummer;
         vorgaenger = bestimme_vorgaenger(name);
         nachfolger = bestimme_nachfolger(name);
         knickliste1 = bestimme_knickliste(vorgaenger,name);
         knickliste2 = bestimme_knickliste(name,nachfolger);
         hknickliste = (TKNICKLISTE*) malloc(sizeof(TKNICKLISTE));
         hknickliste->xpos = hknotenmenge->xpos;
         hknickliste->ypos = hknotenmenge->ypos;
         hknickliste->next = knickliste2;
         if (knickliste1 == NULL)
            knickliste1 = hknickliste;
           else
            {
              hilf = knickliste1;
              while (hilf->next != NULL)
                 hilf = hilf->next;
              hilf->next = hknickliste;
            }
         neukante(vorgaenger,name,nachfolger,knickliste1);
       }  /*  if  */
     hknotenmenge = hknotenmenge->next;
  }  /*  while  */
}  /*  berechne_knicke  */




int bestimme_vorgaenger(name)  /*  Hilfsprocedur von berechne_knicke  */
/*  Es gibt nur eine Kante x -> name. x wird geliefert.  */
{
TKANTENMENGE *hkantenmenge;
int gefunden = 0;
hkantenmenge = kantenmenge;
while (gefunden == 0)
  {
    if (hkantenmenge->ende == name)
      gefunden = 1;
      else hkantenmenge = hkantenmenge->next;
  } /*  while  */
return(hkantenmenge->anfang);
}  /*  bestimme_vorgaenger  */



int bestimme_nachfolger(name)  /*  Hilfsprocedur von berechne_knicke  */
/*  Es gibt nur eine Kante name -> x. x wird geliefert.  */
{
TKANTENMENGE *hkantenmenge;
int gefunden = 0;
hkantenmenge = kantenmenge;
while (gefunden == 0)
  {
    if (hkantenmenge->anfang == name)
      gefunden = 1;
      else hkantenmenge = hkantenmenge->next;
  } /*  while  */
return(hkantenmenge->ende);
}  /*  bestimme_nachfolger  */



TKNICKLISTE *bestimme_knickliste(anfang,ende) 
     /*  Hilfsprocedur von berechne_knicke  */
/*  Es gibt nur eine Kante anfang -> ende. Deren Knickliste wird ermittelt. */
{
TKANTENMENGE *hkantenmenge;
int gefunden = 0;
hkantenmenge = kantenmenge;
while (gefunden == 0)
  {
    if ((hkantenmenge->anfang == anfang)&&(hkantenmenge->ende == ende))
     gefunden = 1;
    else hkantenmenge = hkantenmenge->next;
  } /*  while  */
return(hkantenmenge->knicke);
}  /*  bestimme_knickliste */






void neukante(vor,name,nach,liste)  /*  Hilfsprocedur von berechne_knicke  */
/*  Die Kante vor -> name wird gesucht, ihr ende auf nach geaendert,
    und bei ihr bei knicke liste eingetragen.  Vorher wird die Kante
   name -> nach geloescht.   */
TKNICKLISTE *liste;
{
TKANTENMENGE *hkantenmenge,*freemenge;
char kantenname[10];
int gefunden = 0;
strcpy(kantenname,"");
hkantenmenge = kantenmenge;
if ((hkantenmenge->anfang == name)&&(hkantenmenge->ende == nach))
  {
    strcpy(kantenname,hkantenmenge->name);
    freemenge = kantenmenge;
    kantenmenge = kantenmenge->next;
  }
 else
while (gefunden == 0)
  {
    if ((hkantenmenge->next->anfang == name)&&
       (hkantenmenge->next->ende == nach))
      {
        gefunden = 1;
        strcpy(kantenname,hkantenmenge->next->name);
        freemenge = hkantenmenge->next;
        hkantenmenge->next = hkantenmenge->next->next;
      }
    else hkantenmenge = hkantenmenge->next;
  }  /*  while  */
/* free(freemenge); */
hkantenmenge = kantenmenge;
gefunden = 0;
while (gefunden == 0)
  {
    if ((hkantenmenge->anfang == vor)&&(hkantenmenge->ende == name))
      gefunden = 1;
     else hkantenmenge = hkantenmenge->next;
  }  /*  while  */
hkantenmenge->ende = nach;
if (strcmp(kantenname,"") != 0)
  strcpy(hkantenmenge->name,kantenname);
hkantenmenge->knicke = liste;
return;
}  /*  neukante  */








/*****************************************************************************/
/*****************************************************************************/


extern Snode sedge_real_source();
extern Snode sedge_real_target();




void ausgeben()   
/*  Die Koordinaten fuer Knoten und Knicke werden eingetragen.  */
{

Sgraph heingabe;
Snode hknoten,knotensource,knotentarget;
Sedge hkante;
Edgeline edgeline,hedgeline,ed;
int berechne_einheit();
TKNOTENMENGE *hilfknotenmenge,*sknotenmenge,*hknotenmenge,*tknotenmenge;
TKANTENMENGE *hilfkantenmenge;
TKNICKLISTE *hknicke;
char name[10];
int xkor,ykor,xkorziel,ykorziel,gefunden,x,y,einheit,knotenanf,edknotenanf;
int knotenend,edknotenend,knickzahl,i,j,zaehler;

heingabe = seingabe;
if (Einheit <= 0)
  Einheit = berechne_einheit();
einheit = Einheit;
for_all_nodes(heingabe,hknoten)
  {
    hilfknotenmenge = knotenmenge;
    while (hknoten->nr != hilfknotenmenge->edname)
          hilfknotenmenge = hilfknotenmenge->next;
    xkor = (Maxlinks + (hilfknotenmenge->xpos * einheit));
    ykor = (Maxoben + (hilfknotenmenge->ypos * einheit));
    hknoten->x = xkor;
    hknoten->y = ykor;
    for_sourcelist(hknoten,hkante)
      {
        knotensource = sedge_real_source(hkante);
        knotentarget = sedge_real_target(hkante);
        edknotenanf = knotensource->nr;
        hknotenmenge = knotenmenge;
        while (hknotenmenge->edname != edknotenanf)
          hknotenmenge = hknotenmenge->next;
        knotenanf = hknotenmenge->nummer;
        edknotenend = knotentarget->nr;
        hknotenmenge = knotenmenge;
        while (hknotenmenge->edname != edknotenend)
          hknotenmenge = hknotenmenge->next;
        knotenend = hknotenmenge->nummer;
        zaehler = 0;
        if (edknotenanf == edknotenend)
          {
            hedgeline = (Edgeline) edge_get(graphed_edge(hkante),EDGE_LINE);
            for_edgeline(hedgeline,ed)
              {
                 zaehler++;
              }
            end_for_edgeline(hedgeline,ed);
          }
        if (zaehler != 2)
          {
            hilfkantenmenge = kantenmenge;
            if (hkante->label != NULL)
              strcpy(name,(char*) edge_get(graphed_edge(hkante),EDGE_LABEL));
             else strcpy (name,"");
            if (seingabe->directed == 1)
              {
                while ((hilfkantenmenge->anfang != knotenanf)||
                       (hilfkantenmenge->ende != knotenend)||
                       (hilfkantenmenge->sbeh > 0))
                  hilfkantenmenge = hilfkantenmenge->next;
              }  /*  directed = 1  */
             else  /*  directed = 0  */
              {
                while ((((hilfkantenmenge->anfang != knotenanf)||
                        (hilfkantenmenge->ende != knotenend)||
                        (hilfkantenmenge->sbeh > 1))&&

                        ((hilfkantenmenge->anfang != knotenend)||
                        (hilfkantenmenge->ende != knotenanf)||
                        (hilfkantenmenge->sbeh > 1)))||

                        ((hilfkantenmenge->sbeh == 1)&&
                         (hilfkantenmenge->anfang != hilfkantenmenge->ende)&&
                         (hilfkantenmenge->behsknoten == hknoten->nr))||

                         (strcmp(hilfkantenmenge->name,name) != 0 ))
                  hilfkantenmenge = hilfkantenmenge->next;
              }  /*  directed = 0  */
            hilfkantenmenge->sbeh++;
            hilfkantenmenge->behsknoten = hknoten->nr;
            sknotenmenge = knotenmenge;
            gefunden = 0;
            while (gefunden == 0)
              {
                if (sknotenmenge->nummer == knotenanf)
                  gefunden = 1;
                 else sknotenmenge = sknotenmenge->next;
              }
            tknotenmenge = knotenmenge;
            gefunden = 0;
            while (gefunden == 0)
              {
                if (tknotenmenge->nummer == knotenend)
                  gefunden = 1;
                 else tknotenmenge = tknotenmenge->next;
              }
            if (seingabe->directed == 0)
              {
                 xkor = (Maxlinks + (sknotenmenge->xpos * einheit));
                 ykor = (Maxoben + (sknotenmenge->ypos * einheit));
                 xkorziel = (Maxlinks + (tknotenmenge->xpos * einheit));
                 ykorziel = (Maxoben + (tknotenmenge->ypos * einheit));
                 edgeline = new_edgeline(xkor,ykor);
                 add_to_edgeline(edgeline,xkorziel,ykorziel);
              }  /*  directed == 0  */
             else  /*  directed == 1  */
              {
                edgeline = new_edgeline(ykor,xkor);
                xkorziel = (Maxlinks + (tknotenmenge->xpos * einheit));
                ykorziel = (Maxoben + (tknotenmenge->ypos * einheit));
                add_to_edgeline(edgeline,xkorziel,ykorziel);
              }  /*  directed == 1  */
            hknicke = hilfkantenmenge->knicke;
            knickzahl = 0;
            while (hknicke != NULL)
              {
                 knickzahl++;
                 hknicke = hknicke->next;
              }
            if ((seingabe->directed == 1)||
                (hilfkantenmenge->anfang == knotenanf))
              for (i=0;i<knickzahl;i++)
                {
                  hknicke = hilfkantenmenge->knicke;
                  for (j=(knickzahl -1);j>i;j--)
                    {
                      hknicke = hknicke->next;
                    }  /*  for  j  */
                  x = (Maxlinks + (hknicke->xpos * einheit));
                  y = (Maxoben + (hknicke->ypos * einheit));
                  add_to_edgeline(edgeline,x,y);
                  hknicke = hknicke->next;
                }  /*  for i   */
             else  /*  directed == 0  */
              for (i=0;i<knickzahl;i++)
                {
                  hknicke = hilfkantenmenge->knicke;
                  for (j=0;j<i;j++)
                    {
                      hknicke = hknicke->next;
                    }  /*  for  j  */
                  x = (Maxlinks + (hknicke->xpos * einheit));
                  y = (Maxoben + (hknicke->ypos * einheit));
                  add_to_edgeline(edgeline,x,y);
                  hknicke = hknicke->next;
                }  /*  for i   */
            edge_set(graphed_edge(hkante),EDGE_LINE, edgeline,
                     0);
            /*  free_edgeline(edgeline);  */
        }  /*  zaehler != 2  */
      }
    end_for_sourcelist(hknoten,hkante);

    for_targetlist(hknoten,hkante)
      {
        knotensource = sedge_real_source(hkante);
        knotentarget = sedge_real_target(hkante);
        edknotenanf = knotensource->nr;
        hknotenmenge = knotenmenge;
        while (hknotenmenge->edname != edknotenanf)
          hknotenmenge = hknotenmenge->next;
        knotenanf = hknotenmenge->nummer;
        edknotenend = knotentarget->nr;
        hknotenmenge = knotenmenge;
        while (hknotenmenge->edname != edknotenend)
          hknotenmenge = hknotenmenge->next;
        knotenend = hknotenmenge->nummer;
        zaehler = 0;
        if (edknotenanf == edknotenend)
          {
            hedgeline = (Edgeline) edge_get(graphed_edge(hkante),EDGE_LINE);
            for_edgeline(hedgeline,ed)
              {
                 zaehler++;
              }
            end_for_edgeline(hedgeline,ed);
          }
        if (zaehler != 2)
          {
            hilfkantenmenge = kantenmenge;
            while ((hilfkantenmenge->anfang != knotenanf)||
                   (hilfkantenmenge->ende != knotenend)||
                   (hilfkantenmenge->tbeh == 1))
              hilfkantenmenge = hilfkantenmenge->next;
            hilfkantenmenge->tbeh = 1;
            sknotenmenge = knotenmenge;
            gefunden = 0;
            while (gefunden == 0)
              {
                if (sknotenmenge->nummer == hilfkantenmenge->ende)
                  gefunden = 1;
                 else sknotenmenge = sknotenmenge->next;
              }
                xkorziel = (Maxlinks + (sknotenmenge->xpos * einheit));
                ykorziel = (Maxoben + (sknotenmenge->ypos * einheit));
            edgeline = new_edgeline(xkorziel,ykorziel);
            add_to_edgeline(edgeline,xkor,ykor);
            hknicke = hilfkantenmenge->knicke;
            knickzahl = 0;
            while (hknicke != NULL)
              {
                 knickzahl++;
                 hknicke = hknicke->next;
              }
            for (i=0;i<knickzahl;i++)
              {
                hknicke = hilfkantenmenge->knicke;
                for (j=(knickzahl -1);j>i;j--)
                  {
                    hknicke = hknicke->next;
                  }  /*  for  j  */
                x = (Maxlinks + (hknicke->xpos * einheit));
                y = (Maxoben + (hknicke->ypos * einheit));
                add_to_edgeline(edgeline,x,y);
                hknicke = hknicke->next;
              }  /*  for i   */
            edge_set(graphed_edge(hkante),EDGE_LINE, edgeline,
                     0);
            /*  free_edgeline(edgeline);  */
        }  /*  zaehler  !=  2  */
      }
    end_for_targetlist(hknoten,hkante);
  }
end_for_all_nodes(heingabe,hknoten);
return;
}  /* ausgeben  */




int berechne_einheit() 
     /*  Hilfsprocedur von ausgeben */
/*   Die Laenge einer Einheitskante wird bestimmt.  */
{
TKANTENTRIPEL *hilf;
int ergebnis;
ergebnis = (Maxeinheit/Maxeinheit2);
return(ergebnis);
}  /*  berechne_einheit  */




void freigeben()
/*  Die Speicherplaetze werden freigegeben.   */
{
TGRAPH *hgraph;
TFACE *hflaeche;
TKANTENTRIPEL *hface;
TKNOTENMENGE *hilfvdach,*hknotenmenge;
TBOGENMENGE *hilfbogenmenge;
TNETZKNOTENMENGE *hilfnetz;
TKANTENMENGE *hkantenmenge;
TFACEMENGE *hfacemenge;
THILFSKANTENMENGE *hhilfskantenmenge;
TEDGRAPH *hedgraph;
TKANTENLISTE *hkanten;
hgraph = laengengraph;
while (hgraph != NULL)
  {
    hflaeche = hgraph->flaeche;
    hface = hflaeche->face;
    while (hface != NULL)
     {
       free(hface);
       hface = hface->next;
     }  /*  while  */
    free(hflaeche);
    free(hgraph);
    hgraph = hgraph->next;
  }  /* while  */
hgraph = graph;
while (hgraph != NULL)
  {
    hflaeche = hgraph->flaeche;
    hface = hflaeche->face;
    while (hface != NULL)
     {
       free(hface);
       hface = hface->next;
     }  /*  while  */
    free(hflaeche);
    free(hgraph);
    hgraph = hgraph->next;
  }  /* while  */
hilfvdach = Vdachmenge;
while (hilfvdach != NULL)
  {
    free(hilfvdach);
    hilfvdach = hilfvdach->next;
  }  /*  while  */
hilfbogenmenge = bogenmenge;
while (hilfbogenmenge != NULL)
  {
    free(hilfbogenmenge);
    hilfbogenmenge = hilfbogenmenge->next;
  }  /*  while  */
hilfnetz = netzknotenmenge;
while (hilfnetz != NULL)
  {
   free(hilfnetz);
   hilfnetz = hilfnetz->next;
  }  /* while  */
hknotenmenge = knotenmenge;
while (hknotenmenge != NULL)
  {
    free(hknotenmenge);
    hknotenmenge = hknotenmenge->next;
  }  /*  while  */
hkantenmenge = kantenmenge;
while (hkantenmenge != NULL)
  {
    free(hkantenmenge);
    hkantenmenge = hkantenmenge->next;
  }  /*  while  */
hfacemenge = facemenge;
while (hfacemenge != NULL)
  {
    free(hfacemenge);
    hfacemenge = hfacemenge->next;
  }  /*  while  */
hhilfskantenmenge = hilfskantenmenge;
while (hhilfskantenmenge != NULL)
  {
    free(hhilfskantenmenge);
    hhilfskantenmenge = hhilfskantenmenge->next;
  }  /*  while  */
hedgraph = edgraph;
while (hedgraph != NULL)
  {
     free(hedgraph);
     hedgraph = hedgraph->next;
  }
}  /*  freigeben  */





