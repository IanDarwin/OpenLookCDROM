#ifndef _PIXRECT_H_
#define _PIXRECT_H_

#include <sys/types.h>

/* Definitions. */

/* ANSI prototype conditionalizer. */
#ifndef ARGS
#if __STDC__
#define ARGS(alist) alist
#else /*__STDC__*/
#define ARGS(alist) ()
#endif /*__STDC__*/
#endif /*ARGS*/

struct pr_size
    {
    int x, y;
    };

struct pixrect
    {
    caddr_t pr_ops;	/* not used */
    struct pr_size pr_size;
    int pr_depth;
    caddr_t pr_data;
    };
#define pr_width pr_size.x
#define pr_height pr_size.y

typedef struct pixrect Pixrect;

struct pr_pos
    {
    int x, y;
    };

struct pr_prpos
    {
    struct pixrect* pr;
    struct pr_pos pos;
    };

struct pr_subregion
    {
    struct pixrect* pr;
    struct pr_pos pos;
    struct pr_size size;
    };

struct singlecolor
    {
    unsigned char red, green, blue;
    };

#define PIX_ERR -1

#define PIX_SRC (0xC << 1)
#define PIX_DST (0xA << 1)
#define PIX_NOT(op) ((0xf<<1)&(~(op)))		/* clean ~op */
#define PIX_CLR (PIX_SRC&PIX_NOT(PIX_SRC))	/* background */
#define PIX_SET (PIX_SRC|PIX_NOT(PIX_SRC))	/* foreground */
#define PIX_COLOR(c) ((c)<<5)
#define PIX_OPCOLOR(op) ((op)>>5)

#define PIXOP_NEEDS_DST(op) ((((op)>>1)^(op)) & PIX_NOT(PIX_DST))
#define PIXOP_NEEDS_SRC(op) ((((op)>>2)^(op)) & PIX_NOT(PIX_SRC))

#define PIX_DONTCLIP 0x1
#define PIX_CLIP 0x0

#define prs_rop(dstreg, op, srcprpos) \
    pr_rop((dstreg).pr, (dstreg).pos.x, (dstreg).pos.y, \
    (dstreg).size.x, (dstreg).size.y, op, \
    (srcprpos).pr, (srcprpos).pos.x, (srcprpos).pos.y)
#define prs_stencil(dstreg, op, stenprpos, srcprpos) \
    pr_stencil((dstreg).pr, (dstreg).pos.x, (dstreg).pos.y, \
    (dstreg).size.x, (dstreg).size.y, op, \
    (stenprpos).pr, (stenprpos).pos.x, (stenprpos).pos.y, \
    (srcprpos).pr, (srcprpos).pos.x, (srcprpos).pos.y)
#define prs_batchrop(dstprpos, op, items, n) \
    pr_batchrop((dstprpos).pr, (dstprpos).pos.x, (dstprpos).pos.y, \
    op, items, n)
#define prs_destroy(pr) \
    pr_destroy(pr)
#define prs_get(srcprpos) \
    pr_get((srcprpos).pr, (srcprpos).pos.x, (srcprpos).pos.y)
#define prs_put(dstprpos, val) \
    pr_get((dstprpos).pr, (dstprpos).pos.x, (dstprpos).pos.y)
#define prs_vector(pr, pos0, pos1, op, color) \
    pr_vector(pr, (pos0).x, (pos0).y, (pos1).x, (pos1).y, op, color)
#define prs_region(dstreg) \
    pr_region((dstreg).pr, (dstreg).pos.x, (dstreg).pos.y, \
    (dstreg).size.x, (dstreg).size.y)
#define prs_putcolormap(pr, ind, cnt, red, grn, blu) \
    pr_putcolormap(pr, ind, cnt, red, grn, blu)
#define prs_getcolormap(pr, ind, cnt, red, grn, blu) \
    pr_getcolormap(pr, ind, cnt, red, grn, blu)

#define prs_replrop(dstreg, op, srcprpos) \
    pr_replrop((dstreg).pr, (dstreg).pos.x, (dstreg).pos.y, \
    (dstreg).size.x, (dstreg).size.y, op, \
    (srcprpos).pr, (srcprpos).pos.x , (srcprpos).pos.y )

#define pr_close(pr) \
    pr_destroy(pr)


/* Routines. */

extern struct pixrect* pr_open ARGS(( char* fbname ));
extern struct pixrect* pr_region ARGS(( struct pixrect* pr, int x, int y, int w, int h ));
extern void pr_destroy ARGS(( struct pixrect* pr ));
extern int pr_rop ARGS(( struct pixrect* dpr, int dx, int dy, int w, int h, int op, struct pixrect* spr, int sx, int sy ));
extern int pr_get ARGS(( struct pixrect* pr, int x, int y ));
extern int pr_put ARGS(( struct pixrect* pr, int x, int y, int value ));
extern int pr_stencil ARGS(( struct pixrect* dpr, int dx, int dy, int w, int h, int op, struct pixrect* stpr, int stx, int sty, struct pixrect* spr, int sx, int sy ));
extern int pr_replrop ARGS(( struct pixrect* dpr, int dx, int dy, int dw, int dh, int op, struct pixrect* spr, int sx, int sy ));
extern int pr_batchrop ARGS(( struct pixrect* dpr, int dx, int dy, int op, struct pr_prpos* sprs, int n ));
extern int pr_vector ARGS(( struct pixrect* pr, int x0, int y0, int x1, int y1, int op, int value ));
extern int pr_polypoint ARGS(( struct pixrect* dpr, int dx, int dy, int npts, struct pr_pos* ptlist, int op ));
extern int pr_getcolormap ARGS(( struct pixrect* pr, int ind, int cnt, short* red, short* grn, short* blu ));
extern int pr_putcolormap ARGS(( struct pixrect* pr, int ind, int cnt, short* red, short* grn, short* blu ));
extern void pr_flip ARGS(( struct pixrect* pr ));

#endif /*_PIXRECT_H_*/
