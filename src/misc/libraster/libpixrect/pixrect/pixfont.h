#ifndef	_PIXFONT_H_
#define	_PIXFONT_H_

/* Definitions. */

struct pixfont
    {
    struct pr_size pf_defaultsize;
    caddr_t data;	/* this is different from Sun's pixfont */
    };
typedef struct pixfont Pixfont;
#define	PIXFONT	Pixfont

#define pf_text(where, op, pf, text) \
    pr_text((where).pr, (where).pos.x, (where).pos.y, op, pf, text)
#define pf_ttext(where, op, pf, text) \
    pr_ttext((where).pr, (where).pos.x, (where).pos.y, op, pf, text)
#define prs_text(where, op, pf, text) \
    pr_text((where).pr, (where).pos.x, (where).pos.y, op, pf, text)
#define prs_ttext(where, op, pf, text) \
    pr_ttext((where).pr, (where).pos.x, (where).pos.y, op, pf, text)


/* Routines. */

extern struct pixfont* pf_open ARGS(( char* name ));
extern struct pixfont* pf_open_private ARGS(( char* name ));
extern struct pixfont* pf_default ARGS(( void ));
extern int pf_close ARGS(( struct pixfont* pf ));
extern int pr_text ARGS(( struct pixrect* pr, int x, int y, int op, struct pixfont* pf, char* text ));
extern int pr_ttext ARGS(( struct pixrect* pr, int x, int y, int op, struct pixfont* pf, char* text));
extern struct pr_size pf_textwidth ARGS(( int len, struct pixfont* pf, char* tex));
extern int pf_textbound ARGS(( struct pr_subregion* bound, int len, struct pixfont* pf, char* text ));

#endif /*_PIXFONT_H_*/
