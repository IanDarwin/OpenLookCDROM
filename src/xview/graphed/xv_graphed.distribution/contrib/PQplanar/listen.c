/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : listen.c                                       */
/*        AUTOR : Uwe Schnieders                                  */
/*        UPDATE: 23.07.90                                         */
/*****************************************************************/
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*                                                                 */
/*****************************************************************/



/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "modula.h"
#include "listen.h"



ADRlist concatlist();



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  ADRlist neueselement( element )
  char    *element;
  BEGIN
    ADRlist hilf;
    NEW( hilf );
    hilf->elem = element;
    hilf->next = NIL;
    return( hilf);
  END ;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  int lengthlist(list)
    ADRlist list;
    BEGIN
      IF list == NIL THEN
        return(0)
      ELSE
        return(1+lengthlist(list->next));
      END;
    END;



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  ADRlist queuelist ( nl,n)
  ADRlist nl;
  char*n;
  BEGIN
    return( concatlist( nl , neueselement(n) ) );
  END ;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  char *poplist ( nl)
  ADRlist *nl;
  BEGIN
    ADRlist alt;
    char *n;
    n = NIL;
    IF nl != NIL THEN
      n = (*nl)->elem;
      alt = (*nl);
      (*nl) = (*nl)->next;
      free( (char *)alt );
    END /* if nl */;
    return ( n );
  END ;



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  char *first_element ( nl)
          ADRlist nl;
  BEGIN
          char *n;

    n = NIL;
    IF nl != NIL THEN
      n = (nl)->elem;
    ENDIF ;
    return ( n );
  END ;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  char *last_element ( nl)
          ADRlist nl;
  BEGIN
          char    *n;
          ADRlist alt;

    n = NIL;
    alt = nl;
    IF alt != NIL THEN
      WHILE alt->next != NIL DO
        alt=alt->next;
      ENDDO;
      n = alt->elem;
    ENDIF ;
    return ( n );
  END ;



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  ADRlist pushlist ( nl,n )
  ADRlist nl;
  char    *n;
  BEGIN
    ADRlist hilf;
    hilf = neueselement(n);
    hilf->next = nl;
    return( hilf);
  END ;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  ADRlist createlist ()
  BEGIN
    return((ADRlist)NIL);
  END ;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  void releaselist ( nl )
  ADRlist *nl;
  BEGIN
    IF (*nl) != NIL THEN
      releaselist( &(*nl)->next );
      free( (char*)(*nl) );
      (*nl) = NIL;
    END /* if nl */
  END ;

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  void releaselist_and_elements ( nl )
  ADRlist *nl;
  BEGIN
    IF (*nl) != NIL THEN
      releaselist( &(*nl)->next );
      free( (*nl)->elem );
      free( (char*)(*nl) );
      (*nl) = NIL;
    END /* if nl */
  END ;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  BOOLEAN emptylist ( nl)
  ADRlist nl;
  BEGIN
    return ( nl == NIL );
  END ;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  ADRlist copylist ( klis)
  ADRlist klis;
  BEGIN
  ADRlist hilf;
    hilf = NIL;
    IF klis != NIL THEN
      NEW( hilf );
      hilf->elem = klis->elem;
      hilf->next = copylist( klis->next );
    END /* if klis */;
    return ( hilf );
  END ;


 /*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

 ADRlist concatlist ( list1,list2)
  ADRlist list1,list2;
  BEGIN
    IF list1 == NIL THEN
      list1 = list2;
     ELSE
      list1->next = concatlist( list1->next, list2 );
    END /* if list1 */;
    return( list1);
  END;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  ADRlist reverselist ( klis)
  ADRlist klis;
  BEGIN
  ADRlist hilf,merke ;
    hilf = NIL;
    WHILE klis != NIL DO
      merke = klis;
      klis = klis->next;
      merke->next = hilf;
      hilf = merke;
    END /* while klis */;
    return ( hilf );
  END;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  char *searchlist ( klis,k,equal)
  ADRlist klis;
  char *k;
  BOOLEAN  (*equal)();
  BEGIN
  char *hilf;
    hilf = NIL;
    IF klis == NIL THEN
      RETURN ( hilf );
     ELSE
      IF equal( klis->elem, k ) THEN
         RETURN ( klis->elem );
       ELSE
         RETURN ( searchlist( klis->next, k, equal ) );
      END /* if equal */;
    END /* if klis */;
  END ;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  BOOLEAN memberlist ( klis,k,equal)
  ADRlist klis;
  char *k;
  BOOLEAN  (*equal)();
  BEGIN
    RETURN ( searchlist( klis, k, equal ) != NIL );
  END ;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  ADRlist deleteElement ( klis,k,equal)
  ADRlist klis;
  char *k;
  BOOLEAN  (*equal)();
  BEGIN
  ADRlist hilf;
    IF klis != NIL THEN
      klis->next = deleteElement( klis->next, k, equal );
      IF equal( klis->elem, k ) THEN
        hilf = klis;
        klis = klis->next;
        free((char*) hilf );
      END;
    END ;
    return( klis);
  END ;



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  ADRlist insertSortlist ( klis,k,less)
  ADRlist klis;
  char *k;
  BOOLEAN  (*less)();
  BEGIN
    ADRlist hilf;
    IF klis == NIL THEN
      klis = neueselement(  k );
     ELSE
      IF less( klis->elem, k ) THEN
        klis->next = insertSortlist( klis->next, k, less );
       ELSE
        hilf = neueselement(  k );
        hilf->next = klis;
        klis = hilf;
      END ;
    END ;
    return( klis);
  END ;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  char *searchSortlist ( klis,k,less,equal)
  ADRlist klis;
  char *k;
  BOOLEAN  (*less )();
  BOOLEAN  (*equal)();
  BEGIN
  char *hilf;
    hilf = NIL;
    IF klis == NIL THEN
      RETURN ( hilf );
     ELSE
      IF equal( klis->elem, k ) THEN
         RETURN ( klis->elem );
       ELSE
         IF less( klis->elem, k ) THEN
           RETURN ( searchSortlist( klis->next, k, less, equal ) );
         ELSE
          RETURN ( hilf );
        END ;
      END ;
    END ;
  END ;



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  BOOLEAN memberSortlist ( klis,k,less,equal)
  ADRlist klis;
  char *k;
  BOOLEAN  (*less )();
  BOOLEAN  (*equal)();
  BEGIN
    RETURN ( searchSortlist( klis, k, less, equal ) != NIL );
  END ;

