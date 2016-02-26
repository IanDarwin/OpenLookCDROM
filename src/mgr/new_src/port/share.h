/* routines to "share" bitmaps */

#ifdef MOVIE
#include <stdio.h>

/* types */

#define OP_MASK		0xF			/* opcode part of type */
#define TYPE_MASK		0xF0			

#define T_NOP			0X00			/* do a nnop */
#define T_BLIT			0X10			/* do a bit-blt */
#define T_WRITE		0X20			/* do a bit-blt */
#define T_LINE			0X30			/* do a line */
#define T_POINT		0X40			/* do a point */
#define T_DATA			0X50			/* send some data */
#define T_KILL			0X60			/* destroy a bitmap */
#define T_SCREEN		0X70			/* define the screen */
#define T_TIME			0X80			/* current time (100'th of a second) */
#define T_BLIT2		0x90			/* compressed bit-blit for chars */
#define T_MARK       0xa0      	/* network marks */

struct share_msg {
	unsigned short type;						/* message type */
	unsigned short stuff[8];				/* other stuff */
	};

/* stuff for getting bitmap id's */

#define MAX_MAPS	4000		/* max # bitmap id's */

/* get an unused bitmap id */

#define get_mid()	\
	(next_free>0 ? freed_ids[--next_free] : next_id++)

/* free a bitmap id */

#define free_mid(n) \
	(next_id-1 == (n) ? next_id-- : (freed_ids[next_free++] = (n)))

/* register/unregister  a bitmap */

#ifndef FOO

#define reg_map(map) \
	bit_maps[map->id] = map

#define unreg_map(map) \
	bit_maps[map->id] = NULL

#else

#define reg_map(map) {\
	bit_maps[map->id] = map; \
	fprintf(stderr,"  reg %d %d,%d %dx%d %s\n", \
		map->id,map->x0,map->y0,map->wide,map->high, \
		map==map->primary?"primary:""); \
	}

#define unreg_map(map) {\
	fprintf(stderr,"  un-reg %d %d,%d %dx%d %x=%x\n", \
		map->id,map->x0,map->y0,map->wide,map->high,map,map->primary); \
	bit_maps[map->id] = NULL; \
	}

#endif

/* macros for sending data */

#define SEND_data(id,wide,high,data) { \
	_m.type = T_DATA; \
	_m.stuff[0] = id; \
	_m.stuff[1] = wide; \
	_m.stuff[2] = high; \
	_m.stuff[3] = (data ? 1 : 0); \
	DO_MSG(_m,(char *)data); \
	}
#define SEND_screen(id,wide,high,data) { \
	_m.type = T_SCREEN; \
	_m.stuff[0] = id; \
	_m.stuff[1] = wide; \
	_m.stuff[2] = high; \
	_m.stuff[3] = (data ? 1 : 0); \
	DO_MSG(_m,(char *)data); \
	}
#define SEND_SRC(dst_id,dx,dy,w,h,op,src_id,sx,sy) { \
	_m.type = (op&0xF) + T_BLIT; \
	_m.stuff[0] = dst_id; \
	_m.stuff[1] = src_id; \
	_m.stuff[2] = dx; \
	_m.stuff[3] = dy; \
	_m.stuff[4] = w; \
	_m.stuff[5] = h; \
	_m.stuff[6] = sx; \
	_m.stuff[7] = sy; \
	DO_MSG(_m,0); \
	}
#define SEND_DST(id,x,y,w,h,op) { \
	_m.type = (op&0xF) + T_WRITE; \
	_m.stuff[0] = id; \
	_m.stuff[1] = x; \
	_m.stuff[2] = y; \
	_m.stuff[3] = w; \
	_m.stuff[4] = h; \
	DO_MSG(_m,0); \
	}
#define SEND_LINE(id,x0,y0,x1,y1,op) { \
	_m.type = (op&0xF) + T_LINE; \
	_m.stuff[0] = id; \
	_m.stuff[1] = x0; \
	_m.stuff[2] = y0; \
	_m.stuff[3] = x1; \
	_m.stuff[4] = y1; \
	DO_MSG(_m,0); \
	}
#define SEND_PNT(id,x,y,op) { \
	_m.type = (op&0xF) + T_POINT; \
	_m.stuff[0] = id; \
	_m.stuff[1] = x; \
	_m.stuff[2] = y; \
	DO_MSG(_m,0); \
	}
#define SEND_KILL(id) { \
	_m.type = T_KILL; \
	_m.stuff[0] = id; \
	DO_MSG(_m,0); \
	}

extern unsigned short freed_ids[];		/* list of freed id's */
extern unsigned short next_id;			/* nex unused ID */
extern int next_free;						/* next free id from free list */
extern struct share_msg _m;				/* place to hold the message */
extern int do_save;							/* true iif spitting out data */
extern BITMAP *bit_maps[];					/* pointers to used bitmaps */
#endif	/* MOVIE */
