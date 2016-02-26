/* @(#) $Header: myfbdevice.h,v 1.8 91/10/03 17:45:24 leres Exp $ (LBL) */

struct fbtype {
	int	fb_type;	/* as defined above */
	int	fb_height;	/* in pixels */
	int	fb_width;	/* in pixels */
	int	fb_depth;	/* bits per pixel */
	int	fb_cmsize;	/* size of color map (entries) */
	int	fb_size;	/* total size in bytes */
};

struct fbdevice {
	u_int fb_bits;			/* see defines below */
	int fb_ringing;			/* bell currently ringing */
	int fb_belldepth;		/* audible bell depth */
	int fb_scroll;			/* stupid sun scroll mode */

	struct fbtype fb_type;		/* what is says */

	int fb_p0;			/* escape sequence parameter 0 */
	int fb_p1;			/* escape sequence parameter 1 */

	int *fb_row;			/* emulator row */
	int *fb_col;			/* emulator column */

	int fb_maxrow;			/* actual height of screen */
	int fb_maxcol;			/* actual width of screen */

	int fb_emuwidth;		/* emulator screen width  */
	int fb_emuheight;		/* emulator screen height */

	int fb_xorigin;			/* x origin of first column */
	int fb_yorigin;			/* y origin of first row */

	struct raster *fb_sp;		/* frame buffer raster */
	struct raster *fb_cursor;	/* optional cursor */
	int fb_ras_blank;		/* current screen blank raster op */

	struct raster_font *fb_font;	/* font and related info */
	int fb_font_ascent;		/* distance from font to char origin */
};

#define FB_INESC	0x001		/* processing an escape sequence */
#define FB_STANDOUT	0x002		/* standout mode */
/* #define FB_BOLD	0x?		/* boldface mode */
#define FB_INVERT	0x008		/* white on black mode */
#define FB_VISBELL	0x010		/* visual bell */
#define FB_CURSOR	0x020		/* cursor is visible */
#define FB_P0_DEFAULT	0x100		/* param 0 is defaulted */
#define FB_P1_DEFAULT	0x200		/* param 1 is defaulted */
#define FB_P0		0x400		/* working on param 0 */
#define FB_P1		0x800		/* working on param 1 */
