/* (C) Universitaet Passau 1986-1991 */
/**************************************************/
/*                                                */
/*                  LISTEN.H                      */
/*                                                */
/*  (Typdeklarationen fuer die Listen )           */
/*                                                */
/**************************************************/

typedef struct nodlirec {
	char	        *elem;
	struct nodlirec	*pre,*suc;
	}	Nodlirec;

typedef Nodlirec  *ADDRlist;

extern 	ADDRlist	dqueuelist   ();
extern	char		*dpoplist    ();
extern	ADDRlist	dpushlist    ();
extern	ADDRlist	dcreatelist  ();
extern	void		dreleaselist ();
extern	int		demptylist   ();
extern  ADDRlist	dsearchlist  ();
extern  ADDRlist	dreverselist ();
extern  ADDRlist	dreverselist ();
extern  Snode;
extern  Sedge;

#define empty_list	((ADDRlist)NULL)
#define dfor_all_elements(list, element) \
	{ ADDRlist Forhilf; \
	Forhilf = list; \
	if ( list != empty_list) do { element = Forhilf->elem ;
#define dend_for_all_elements(list) \
	} while ((Forhilf = Forhilf->suc) != list ) ; }

#define dreverse_for_all_elements(list, element) \
        { ADDRlist Forhilf; \
        Forhilf = list; \
        if ( list != empty_list) do { element = Forhilf->elem ;
#define dreverse_end_for_all_elements(list) \
        } while ((Forhilf = Forhilf->pre) != list ) ; }


#define	DLIST			ADDRlist
#define DINIT_LIST(li)   	(li) = dcreatelist()
#define DCLEAR_LIST(li)  	dreleaselist( &(li) )
#define DIS_EMPTY_LIST(li)	demptylist( li )
#define DPOP_NODE(li,el)	(el) = (Snode)dpoplist( &(li) )
#define DPOP_EDGE(li,el)	(el) = (Sedge)dpoplist( &(li) )
#define DPUSH(li,el)		(li) = dpushlist( (li), (char *)(el) )
#define DQUEUE(li,el)		(li) = dqueuelist( (li), (char *)(el) )
#define DPUSH_NODE(li,el)	(li) = dpushlist( (li), (char *)(el) )
#define DQUEUE_NODE(li,el)	(li) = dqueuelist( (li), (char *)(el) )
#define DREVERSE_LIST(li)   	(li) = dreverselist(li)
#define	LIST			ADDRlist
#define INIT_LIST(li)   	(li) = dcreatelist()
#define CLEAR_LIST(li)  	dreleaselist( &(li) )
#define IS_EMPTY_LIST(li)	demptylist( li )
#define POP_NODE(li,el)	(el) = (Snode)dpoplist( &(li) )
#define POP_EDGE(li,el)	(el) = (Sedge)dpoplist( &(li) )
#define PUSH(li,el)		(li) = dpushlist( (li), (char *)(el) )
#define QUEUE(li,el)		(li) = dqueuelist( (li), (char *)(el) )
#define PUSH_NODE(li,el)	(li) = dpushlist( (li), (char *)(el) )
#define QUEUE_NODE(li,el)	(li) = dqueuelist( (li), (char *)(el) )
#define REVERSE_LIST(li)   	(li) = dreverselist(li)


#define POP_XY(li,el)		(el) = (X_Y)dpoplist( &(li) )
