/* code for "mgr sharing" stuff */

#ifdef MOVIE
#include <stdio.h>
#include "bitmap.h"
#include <signal.h>
#include "share.h"

/* stuff for bitmap id's */

BITMAP *bit_maps[MAX_MAPS];			/* pointers to used bitmaps */
unsigned short freed_ids[MAX_MAPS];	/* list of freed id's */
unsigned short next_id = 1;			/* next availiable ID (0 is not used) */
int next_free = 0;						/* next free id from free list */
int no_initial = 0;						/* dont output initial image */
struct share_msg _m;						/* place to hold the message */
struct share_msg do_m;					/* place to hold compressed message */
static FILE *share_file;				/* file to dump output to */
int do_save=0;								/* saving flag */

/* send a bit-blt message: binary version */

/* write blit log */

static int
Write(buff,cnt)
char *buff;				/* what to write */
int cnt;				/* how many to write */
	{
	if (share_file && cnt!=fwrite(buff,1,cnt,share_file)) {	/* kill sharing */
		fprintf(stderr,"Error: Closing MGR log\n");
		do_save=0;
		share_file=NULL;
		}
	}

int
DO_MSG(msg,data)
struct share_msg msg;				/* message to send */
char *data;								/* pointer to data */
	{
	switch (msg.type&TYPE_MASK) {
		case T_SCREEN:
			Write(&msg,10);
			if (msg.stuff[3] != 0)
				Write(data,BIT_Size(msg.stuff[1],msg.stuff[2],1));
			break;
		case T_BLIT:
			Write(&msg,18);
			break;
		case T_WRITE:
			Write(&msg,12);
			break;
		case T_LINE:
			Write(&msg,12);
			break;
		case T_DATA:
			Write(&msg,10);
			if (msg.stuff[3] != 0)
				Write(data,BIT_Size(msg.stuff[1],msg.stuff[2],1));
			break;
		case T_NOP:
			Write(&msg,2);
			if (msg.type&0xF > 0)
				Write(data,msg.type&0xF);
		case T_POINT:
			Write(&msg,8);
			break;
		case T_TIME:
			Write(&msg,6);
			break;
		case T_KILL:
			Write(&msg,4);
			break;
		}
	}

/* Send a timestamp for sequencing of replay */

SEND_TIME()
	{
	if (do_save) {
		register unsigned int time = timestamp();
		_m.type = T_TIME;
		_m.stuff[0] = time>>16;
		_m.stuff[1] = time&0xffff;
		DO_MSG(_m,0);
		fflush(share_file);
		}
	}

/* start a session by sending current bitmap state */

send_sync()
	{
	BITMAP *map;
	register int i;
	
	for(i=1;i<next_id;i++) {
		/* fprintf(stderr,"sunc %d/%d",i,next_id); */
		if ((map=bit_maps[i]) && IS_SCREEN(map) && IS_PRIMARY(map)) {
			SEND_screen(i,BIT_WIDE(map),BIT_HIGH(map),0);
			if (no_initial) {
				no_initial = 0;
/*
				fprintf(stderr,"NOT Setting %d (%d x %d) to dirty SCREEN \n",i,
					BIT_WIDE(map), BIT_HIGH(map));
*/
				}
			else {
				map->primary->type |= _DIRTY;
				fprintf(stderr,"Setting %d (%d x %d) to dirty SCREEN \n",i,
					BIT_WIDE(map), BIT_HIGH(map));
				}
			}
		else if (map && IS_PRIMARY(map) && map->primary->id == i) {
			map->primary->type |= _DIRTY;
/*
			fprintf(stderr,"Setting %d (%d x %d) to dirty\n",i,
					BIT_WIDE(map), BIT_HIGH(map));
*/
			}
		else if (map) {
			fprintf(stderr,"Bitmap %d is corrupted\n",i);
			}
		else {
			fprintf(stderr,"Bitmap %d is not there!!!\n",i);
			}
		}
	}

/* begin logging, send output to 'file' */

int
start_log(file)
FILE *file;
	{
	if (do_save==0 && file) {
		share_file = file;
		do_save = 1;
		/* fprintf(stderr,"Starting log %x\n",share_file); */
		send_sync();
		return(1);	
		}
	else
		return(0);
	}

/* end logging - let application close the log file */
	
int
end_log()
	{
	if (do_save) {
		do_save=0;
		SEND_KILL(0);
		/* fprintf(stderr,"Closing log\n"); */
		return(1);
		}
	else
		return(0);
	}

/* do the logging for bitmaps */

log_blit(dst_map,xd,yd,w,h,op,src_map,xs,ys)
register BITMAP *src_map, *dst_map;		/* source adnd destination bitmaps */
int xd,yd;					/* destination origin */
int w,h;						/* bitmap size */
int xs,ys;					/* source origin */
	{
	
	check_map(dst_map);
	check_map(src_map);

	/* log xaction iff turned on */

	if (do_save) {
		if (src_map) {
			src_map->primary->type &= ~_DIRTY;
			SEND_SRC(dst_map->id,xd,yd,w,h,op,src_map->id,xs,ys);
			}
		else {
			SEND_DST(dst_map->id,xd,yd,w,h,op);
			}
		dst_map->primary->type &= ~_DIRTY;
		}
	}

/* log lines and points */

log_line(dst,x0,y0,x1,y1,op)
BITMAP *dst;
int x0,y0,x1,y1;
int op;
	{
	check_map(dst);
	if (do_save) {
		SEND_LINE(dst->id,x0,y0,x1,y1,op);
		}
	}

log_point(dst,x,y,op)
BITMAP *dst;
int x,y;
int op;
	{
	check_map(dst);
	if (do_save) {
		SEND_PNT(dst->id,x,y,op);
		}
	}

int
sig_share(n)
int n;			/* signal # */
	{
	if (n==SIGUSR1)
		start_log();
	else
		end_log();	
	}

/* check bitmap, register and/or download if needed  */

check_map(map)
register BITMAP *map;
	{
	if (map && map->primary->id == 0) { 		/* static bitmap */
		map->primary->id = get_mid();
		reg_map(map->primary);
		map->primary->type |= _DIRTY;
		}

	/* (no_initial stuff -  this is broken, deleted  (sau 2/91) */

	if (map && do_save && map->primary->type & _DIRTY) {
		SEND_data(map->id,map->primary->wide,
						map->primary->high,map->data);	/* log the data */
		map->primary->type &= ~_DIRTY;
		/* fprintf(stderr,"Sending data for %d\n",map->id); */
 		return(1);
		}
   return(0);
	}

#endif	/* MOVIE */
