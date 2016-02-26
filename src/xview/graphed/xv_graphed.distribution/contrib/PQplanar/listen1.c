/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : listen1.c                                      */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/
/*        Listenmodule fuer doppelt verkettete Listen mit        */
/*        Strucktur :                                            */
/*                                                               */
/*        typedef struct nodlirec BEGIN                              */
/*                char                *elem;                     */
/*                struct nodlirec        *pre,*suc;              */
/*                END        Nodlirec;                             */
/*                                                               */
/*        typedef Nodlirec  *ADDRlist;                           */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI

#include "listen1.h"
#include "modula.h"


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/


ADDRlist        dneueselement( element )
char        *element;
BEGIN
ADDRlist hilf;
hilf = (ADDRlist)malloc (sizeof (struct nodlirec));
hilf->elem = element;
hilf->pre  = hilf;
hilf->suc  = hilf;
return(hilf);
END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  int dlengthlist(list)
    ADDRlist list;
    BEGIN
      ADDRlist hilf;
      INTEGER  zaehler;

      IF list == NIL THEN
        return(0)
      ELSE
        zaehler = 1;
        hilf = list->suc;
        WHILE hilf != list DO
          hilf = hilf ->suc;
          zaehler = zaehler +1 ;
        END;
        return(zaehler);
      END;
    END;


/*****************************************************************/
/*                                                                 */
/*  FUNCTION :                                                       */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/


ADDRlist        dqueuelist( nl,n)
        ADDRlist        nl;
        char           *n;
  BEGIN
        ADDRlist        hilf;

    hilf = dneueselement(n);
    IF nl == NIL THEN 
      nl = hilf;
     ELSE 
      hilf->suc = nl;
      hilf->pre = nl->pre;
      nl->pre->suc = hilf;
      nl->pre = hilf;
    ENDIF;
    RETURN(nl);
  ENDPROC

/*****************************************************************/
/*                                                                 */
/*  FUNCTION :                                                       */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/


char        *dpoplist(nl)
ADDRlist *nl;
BEGIN
ADDRlist alt;
char     *n;
n = NIL;
IF (*nl) != (ADDRlist)NIL THEN
        n =( (*nl)->elem );
        alt = (*nl);
        IF (*nl)->suc == (*nl) THEN
           (*nl) = (ADDRlist)NIL;
        ELSE
           (*nl)->suc->pre = (*nl)->pre;
           (*nl)->pre->suc = (*nl)->suc;
           (*nl) = (*nl)->suc;
        ENDIF; 
        free ( (char *)alt);
ENDIF;
return(n);
END

/*****************************************************************/
/*                                                                 */
/*  FUNCTION :                                                       */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/


ADDRlist        dpushlist(nl,n)
ADDRlist        nl;
char                *n;
BEGIN
ADDRlist        hilf;
hilf = dneueselement(n);
if(nl != (ADDRlist)NIL)
        BEGIN
        hilf->suc = nl;
        hilf->pre = nl->pre;
        hilf->pre->suc = hilf;
        hilf->suc->pre = hilf;
        END
return(hilf);
END

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/


ADDRlist        dcreatelist()
BEGIN
return((ADDRlist)NIL);
END


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void        dreleaselist(nl)
        ADDRlist        *nl;
  BEGIN
        ADDRlist alt;

    IF (*nl) != NIL THEN
      (*nl)->pre->suc = NIL;
      WHILE (*nl) != NIL DO
        alt = (*nl);
        (*nl) = (*nl)->suc;
        free( (char *)alt);
      ENDDO;
    ENDIF;
    (*nl) = NIL;
  ENDPROC


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void        dreleaselist_and_elements(nl)
        ADDRlist        *nl;
  BEGIN
        ADDRlist alt;

    IF (*nl) != NIL)
        BEGIN
        (*nl)->pre->suc = NIL;
        WHILE (*nl) != NIL DO
                alt = (*nl);
                (*nl) = (*nl)->suc;
                free( alt->elem);
                free( (char *)alt);
                END
        END
(*nl) = NIL;
END

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/


int demptylist (nl)
ADDRlist  nl;
BEGIN
return ( nl == NIL);
END

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

ADDRlist dsearchlist2( klis,k,equal,anfang)
  ADDRlist klis;
  char *k;
  BOOLEAN  (*equal)();
  ADDRlist anfang;
  BEGIN
    ADDRlist hilf;

    hilf = NIL;
    IF klis == NIL THEN
      RETURN ( hilf );
     ELSE
      IF equal( klis->elem, k ) THEN
         RETURN( klis );
       ELSE
         IF klis->suc == anfang THEN
           RETURN ( hilf );
          ELSE
           RETURN( dsearchlist2( klis->suc, k, equal ,anfang) );
         END;
      ENDIF /* if equal */;
    ENDIF /* if klis */;
  END 

/*************************************************************/
/*                                                                 */
/*  FUNCTION :                                                       */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/

ADDRlist dsearchlist( klis,k,equal)
  ADDRlist klis;
  char *k;
  BOOLEAN  (*equal)();
  BEGIN
    RETURN(dsearchlist2( klis,k,equal,klis) );
  END

/*************************************************************/
/*                                                                 */
/*  FUNCTION :                                                       */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/

ADDRlist dreverselist(klis)
  ADDRlist klis;
  BEGIN
    char        *elem;
    ADDRlist        Hilf;

    DINIT_LIST(Hilf);
    dfor_all_elements(klis,elem)
      DQUEUE(Hilf,elem);
    dend_for_all_elements(klis);
    RETURN(Hilf);
  END

/*************************************************************/
/*                                                                 */
/*  FUNCTION :                                                       */
/*                                                                */
/*   INPUT   :                                                     */
/*                                                                */
/*   OUTPUT  :                                                    */
/*                                                                */
/*****************************************************************/

  ADDRlist dcopylist2 ( klis,vorher,anfang)
  ADDRlist klis,vorher,anfang;
  BEGIN
    ADDRlist hilf;

    hilf = NIL;
    IF klis != NIL THEN
      NEW( hilf );
      hilf->elem = klis->elem;
      hilf->pre  = vorher;
      IF klis->suc != anfang THEN
        hilf->suc = dcopylist2( klis->suc,hilf,anfang );
       ELSE
        hilf->suc    = anfang;
        anfang->pre  = hilf;
      END;
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

  ADDRlist dcopylist ( klis)
  ADDRlist klis;
  BEGIN
    ADDRlist first,hilf,vorher,neu,erstes;

    IF klis == NIL THEN
      RETURN((ADDRlist)(NIL));
     ELSE
      first = klis;
      vorher = NIL;
      hilf = klis;

      REPEAT
        NEW(neu);
        neu->elem = hilf->elem;
        neu->pre = vorher;
        IF vorher != NIL THEN 
          vorher->suc = neu;
         ELSE
          erstes = neu;
        END;
        vorher = neu;
        hilf = hilf->suc ;
      UNTIL(hilf == first);

      erstes->pre = neu;  
      neu -> suc = erstes;

      RETURN(erstes); 
    END;
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

ADDRlist goto_nth(List,N)
        ADDRlist List;
        INTEGER  N;
  BEGIN
    ADDRlist result;

    IF N <= 0 THEN
      result = (NIL);
     ELSE
      IF N == 1 THEN
        result = (List);
       ELSE
        result = (goto_nth(List->suc,N-1));
      ENDIF;
    ENDIF;
    RETURN( result );
  ENDPROC;



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

char *dnth_element(List,N)
        ADDRlist List;
        INTEGER  N;
  BEGIN
    ADDRlist nth;

    nth = goto_nth(List,N);
    IF nth != NIL THEN
       RETURN(nth->elem);
      ELSE
       RETURN(NIL);
    ENDIF
  ENDPROC;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void dset_nth_element(List,N,element)
        ADDRlist List;
        INTEGER  N;
        char     *element;
  BEGIN
    ADDRlist nth;

    nth = goto_nth(List,N);
    IF nth != NIL THEN
       nth->elem = element;
    ENDIF
  ENDPROC;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void     delete_nth_element(List,N)
        ADDRlist List;
        INTEGER  N;
  BEGIN
    ADDRlist nth;
    char      *dummy;

    nth = goto_nth(List,N);
    IF nth != NIL THEN
       dummy = dpoplist(&nth);
    ENDIF
  ENDPROC;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

ADDRlist insert_after(List,element)
        ADDRlist List;
        char     *element;
  BEGIN
    RETURN(dpushlist(List->suc,element));
  ENDPROC;



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

ADDRlist insert_before(List,element)
        ADDRlist List;
        char     *element;
  BEGIN
    RETURN(dpushlist(List,element));
  ENDPROC;




/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

  ADDRlist dinsertSortlist ( klis,k,less)
         ADDRlist klis;
         char *k;
         BOOLEAN  (*less)();
  BEGIN
    IF klis == NIL THEN
      klis = dneueselement(  k );
     ELSE
      IF less( klis->elem, k ) THEN
        klis->suc = dinsertSortlist( klis->suc, k, less );
       ELSE
        klis = dpushlist(klis,k);
      ENDIF ;
    ENDIF ;
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

void vertausche_elem(k1,k2)
         ADDRlist *k1;
         ADDRlist *k2;
  BEGIN
        char *hilf;
    hilf = (*k1)->elem;
    (*k1)->elem = (*k2)->elem;
    (*k2)->elem = hilf;
  END

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

ADDRlist dsort_list ( klis,less)
         ADDRlist *klis;
         BOOLEAN  (*less)();
  BEGIN
         ADDRlist k1,k2;

    IF (*klis) != NIL THEN
      k1 = (*klis);
      WHILE k1->suc != (*klis) DO
        k2 = k1->suc;
        WHILE k2 != (*klis) DO
          IF less(k1->elem,k2->elem) THEN
            vertausche_elem(&k1,&k2);
          ENDIF;
          k2 = k2->suc;
        ENDDO;
        k1 = k1->suc;
      ENDDO;
    ENDIF;
  ENDPROC;



   
