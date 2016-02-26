/* (C) Universitaet Passau 1986-1991 */
/*1990-91 von Stefan Jockenhoevel und Gerd Nienhaus.*/
/* Stand : 13.12.1990 */

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE PossibleConvexDraw.h                                       **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

#ifndef POSSIBLECONVEXDRAW_HEADER
#define POSSIBLECONVEXDRAW_HEADER

/********************************************************************************/

#define OK1  "The graph is already drawn convex."
#define OK2  "First test OK."
#define NOK  "The graph cannot be drawn convex."
#define TYP1  "Reason : there is at least one multiple edge."
#define TYP2a "Reason : there is at least one self-loop."

/********************************************************************************/

extern bool DrawConvexPossible(/* Sgraph TheGraph */);

/***********************************************************************************/
/*  Die Prozedur ueberprueft die Grundvoraussetzungen zum convexen Zeichen :       */
/*  Gibt es keine Mehrfachkanten und keine Kante von einem Knoten auf sich selbst? */
/***********************************************************************************/

#define EDGEpointer(TheSlist) (attr_data_of_type(TheSlist,Sedge))

/*******************************************************************************/
/*  EDGEpointer(TheSlist) ist der Zeiger auf die, im Listenelement 'TheSlist'  */
/*       abgespeicherte Kante.                                                 */
/*******************************************************************************/

#endif
