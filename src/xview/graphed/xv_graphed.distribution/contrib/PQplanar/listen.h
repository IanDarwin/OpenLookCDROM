/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : listen.h                                       */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/
/*        Einfach verkettete Listen mit allen erforderlichen und */
/*        nuetzlichen DEFINES .                                  */
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


typedef struct nolirec {
                         char *elem ;
                         struct nolirec *next;
               } Nolirec;

typedef Nolirec *ADRlist;


/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/


#define        for_all_elements(list, element)                \
        { ADRlist Forhilf;  \
          Forhilf = list;   \
          while (Forhilf) { \
            (char*)element = Forhilf->elem ; \
            Forhilf        = Forhilf->next ;
            
#define        end_for_all_elements   } ; }

#define LAST_ELEMENT        (Forhilf == NIL)

#define LIST                         ADRlist
#define INIT_LIST(li)               (li) = createlist()
#define CLEAR_LIST(li)              releaselist( &(li) )
#define CLEAR_LIST_AND_ELEMENTS(li) releaselist_and_elements( &(li) )
#define IS_EMPTY_LIST(li)           emptylist( (li) )
#define POP_LIST(li,el)             (el) = (Snode)poplist( &(li) )
#define PUSH_LIST(li,el)            (li) = pushlist ( (li), (char *)(el) )
#define QUEUE_LIST(li,el)           (li) = queuelist( (li), (char *)(el) )
#define INSERT_SORT(li,el,less)     (li) = insertSortlist( (li), (char *)(el),less)     
/*****************************************************************/
/*       EXTERN                                                  */
/*****************************************************************/


extern        int        lengthlist ();
extern        ADRlist        queuelist ();
extern        char        *poplist ();
extern        char        *first_element ();
extern        char        *last_element ();
extern        ADRlist        pushlist ();
extern        ADRlist        createlist ();
extern        void        releaselist ();
extern        int        emptylist ();
extern        ADRlist        copylist ();
extern        ADRlist        concatlist ();
extern        ADRlist        reverselist ();
extern        char        *searchlist ();
extern        int        memberlist ();
extern        ADRlist        deleteElement ();
extern        ADRlist        insertSortlist ();
extern        char        *searchSortlist ();
extern        int        memberSortlist ();

          
