/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*        FILE : listen1.h                                       */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 24.05.89                                       */
/*****************************************************************/
/*        Doppelt verkettete Listen mit allen notwendigen und    */
/*        hilfreichen DEFINES .                                  */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*****************************************************************/

/*****************************************************************/
/*       TYPEDEF                                                 */
/*****************************************************************/

typedef struct nodlirec {
        char                *elem;
        struct nodlirec     *pre,*suc;
        } Nodlirec;

typedef Nodlirec  *ADDRlist;



/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define empty_list        ((ADDRlist)NULL)

#define dfor_all_elements(list, element) \
        { ADDRlist Forhilf; \
        Forhilf = list; \
        if ( list != empty_list) do { element = (char*)Forhilf ;
#define dend_for_all_elements(list) \
        } while ((Forhilf = Forhilf->suc) != list ) ; }


#define dreverse_for_all_elements(list, element) \
        { ADDRlist DForhilf; \
        DForhilf = list; \
        if ( list != empty_list) do { element = (char*)DForhilf ;
#define dreverse_end_for_all_elements(list) \
        } while ((DForhilf = DForhilf->pre) != list ) ; }


#define ADDRLIST_SUC(list) ((list)->suc)

#define ADDRLIST_PRE(list) ((list)->pre)


#define DLIST                            ADDRlist
#define DINIT_LIST(li)                   (li) = dcreatelist()
#define DCLEAR_LIST(li)                  dreleaselist( &(li) )
#define DCLEAR_LIST_AND_ELEMENTS(li)     dreleaselist_and_elements( &(li) )
#define DIS_EMPTY_LIST(li)               demptylist( li )
#define DPOP_ELEMENT(li,el,type)         (el) = (type)dpoplist( &(li) )
#define DPUSH(li,el)                     (li) = dpushlist( (li), (char *)(el) )
#define DQUEUE(li,el)                    (li) = dqueuelist( (li), (char *)(el) )
#define DREVERSE_LIST(li)                (li) = dreverselist(li)
#define DCOPYLIST(li)                    dcopylist (li)


#define DPOP_NODE(li,el)        (el) = (Snode)dpoplist( &(li) )
#define DPOP_EDGE(li,el)        (el) = (Sedge)dpoplist( &(li) )
#define DPUSH_NODE(li,el)       (li) = dpushlist( (li), (char *)(el) )
#define DQUEUE_NODE(li,el)      (li) = dqueuelist( (li), (char *)(el) )


/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/


extern  ADDRlist        dqueuelist   ();
extern  char            *dpoplist    ();
extern  ADDRlist        dpushlist    ();
extern  ADDRlist        dcreatelist  ();
extern  void            dreleaselist ();
extern  int             demptylist   ();
extern  ADDRlist        dsearchlist  ();
extern  ADDRlist        dreverselist ();
extern  ADDRlist        dcopylist    ();
extern  char            *dnth_element();
extern  void            dset_nth_element();
extern  void            delete_nth_element();
extern  ADDRlist        insert_after();
extern  ADDRlist        insert_before();
extern  ADDRlist        dinsertSortlist ();
extern  ADDRlist        dsort_list ();

