/********************************************************************************/
/* batch.h --									*/
/*										*/
/* Author : A.J.Doherty								*/
/* Date   : 01/11/93								*/
/********************************************************************************/

#ifndef _FSPtool_BATCH_H_
#define _FSPtool_BATCH_H_ 1

/********************************************************************************/

typedef struct
    {
    char *hostname,			/* -- name of host file is found on	*/
	 *hostport,			/* -- port id of host file is found on	*/
	 *filename;			/* -- FULL name of file from /		*/
    int   filesize;			/* -- size of file (bytes)		*/
    int	  filetype;			/* -- type of file (needed for sizing)	*/
    }
  BatchItem;

/********************************************************************************/

extern void do_batch_transfer(void);
extern void do_selected_batch_transfer(void);
extern void add_to_batch_list(void);
extern void batch_write_text(void);
extern void post_batch_tidy(void);
extern void batch_transfer_proc(void);

extern void delete_batch_item(int);

extern void write_text_callback(Panel_item,Event*);

extern int  batch_list_notify_proc(Panel_item,
					char*,Xv_opaque,Panel_list_op,Event*,int);

extern Notify_value read_batch_stream(Notify_client,int);

/********************************************************************************/
#endif

