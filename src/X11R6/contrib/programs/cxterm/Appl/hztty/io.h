/* $Id: io.h,v 1.1 1992/07/18 08:45:21 ygz Exp $ */

#define	DEF_ISTREAM	"gb2hz"
#define	DEF_OSTREAM	"hz2gb"

#define	MAX_MODULE	8	/* maximun number of module in a stream */

extern int in_stream_setup(), out_stream_setup();
extern int stream_read(), stream_write(); 

/*
 * All io functions must take the following signatures:
 *	char *xx2yy (char *s, int *plen, int inst)
 *	int xx2yy_init (char *arg)
 */

extern char *gb2hz(), *hz2gb();			/* hz2gb.c */
extern char *gb2big(), *big2gb();		/* b2g.c */
extern char *io_log();				/* log.c */

extern int gb2hz_init(), hz2gb_init();		/* hz2gb.c */
extern int gb2big_init(), big2gb_init();	/* b2g.c */
extern int log_init();				/* log.c */
