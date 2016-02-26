/* (C) Universitaet Passau 1986-1991 */
/* 'planar.h' muss in alle Dateien eingefuegt werden,
   die den Planaritaetstestalgorithmus nutzen wollen. */

/* Letzte Aenderung: 08.02.91 */
   
#ifndef RESULT_TYPE_ALREADY_DEFINED   /* Verhindert Doppeldefinition */
                                      /* des Rueckgabetyps           */
#define RESULT_TYPE_ALREADY_DEFINED

/* Rueckgabewert der Funktion 'planarity' */

typedef
  enum Result {
               SUCCESS,        /* Prozedur erfolgreich - Graph planar */
               NONPLANAR,      /* Nichtplanaritaet entdeckt */
               SELF_LOOP,      /* Schleifen entdeckt */
               MULTIPLE_EDGE,  /* Mehrfachkante entdeckt */
               NO_MEM          /* Nicht genuegend Speicherplatz */
              } 
       RESULT;

#endif

/* Fuehrt Planaritaetstest fuer 'Sgraph' durch;
   Rueckgebewert ist eine Meldung vom Typ 'RESULT'. */

extern RESULT planarity(/* Sgraph */);
