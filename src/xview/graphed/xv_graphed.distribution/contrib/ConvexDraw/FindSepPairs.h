/* (C) Universitaet Passau 1986-1991 */
/*1990-91 von Stefan Jockenhoevel und Gerd Nienhaus.*/

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE FindSepPairs.h                                             **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

#ifndef FINDSEPPAIRS_HEADER
#define FINDSEPPAIRS_HEADER


/*******************************************************************************/
/*  MIN berechnet das Minimum der Nummern von 'FirstNode' und 'SecondNode'     */
/*      und gibt den Zeiger auf den Knoten mit dem kleineren Wert zurueck.     */
/*******************************************************************************/

#define MIN(FirstNode,SecondNode) ((Number(FirstNode) < Number(SecondNode)) ? FirstNode : SecondNode)

extern void FindSeperationPairs(/* Sgraph TheGraph */);

/*******************************************************************************/
/*  Die Prozedur ueberfuehrt den Graphen 'TheGraph' in einen 'palm tree' und   */
/*  berechnet alle noetigen Werte, um mit Hilfe des Lemma 13 (siehe Artikel:   */
/*  'DIVIDING A GRAPH INTO TRICONNECTED COMPONENTS)' die Spaltungspaare des    */
/*  Graphen zu bestimmen.                                                      */
/*******************************************************************************/

extern void RemoveSplitAttrs(/* Sgraph  TheGraph */);

/*******************************************************************************/
/*  Die Prozedur loescht die fuer das Zerlegen des Graphen benoetigten         */
/*  Knoten- bzw. Kantenattribute des Graphen 'TheGraph'.                       */
/*******************************************************************************/


       
#endif
