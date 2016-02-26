/* (C) Universitaet Passau 1986-1991 */
/* 'embed.h' muss in alle Dateien eingefuegt werden,
   die den Einbettungsalgorithmus nutzen wollen.      */

/* Letzte Aenderung: 05.02.91 */
   
#ifndef RESULT_TYPE_ALREADY_DEFINED   /* Verhindert Doppeldefinition */
                                      /* des Rueckgabetyps           */
#define RESULT_TYPE_ALREADY_DEFINED

/* Rueckgabewert der Funktion 'embed' */

typedef
  enum Result {
               SUCCESS,        /* Prozedur erfolgreich - Graph planar */
                  /* Fehlermeldungen: Algorithmus bricht ab     */
                  /*                  ohne 'slist' zu erzeugen. */
               NONPLANAR,      /* Nichtplanaritaet entdeckt */
               SELF_LOOP,      /* Schleifen entdeckt */
               MULTIPLE_EDGE,  /* Mehrfachkante entdeckt */
               NO_MEM          /* Nicht genuegend Speicherplatz */
              } 
       RESULT;

#endif

/* Nach Aufruf von 'embed(sgraph)' wird im 'attrs'-Feld der 
   'Snode'-Struktur eine Liste vom Typ 'Slist' abgelegt, in
   der die Kanten um den jeweiligen Knoten - unabhaengig von
   einer eventuellen Orientierung bei gerichteten Graphen -
   als Zeiger vom Typ 'Sedge' im Uhrzeigersinn eingetragen 
   sind.
   Rueckgebewert ist eine Meldung vom Typ 'RESULT'.          */
   
/* ACHTUNG: Fuer das Loeschen der 'Slist' ist der Benutzer 
            selbst verantwortlich !                          */
 
extern RESULT embed(/* Sgraph */);

