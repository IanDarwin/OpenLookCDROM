/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*	Implementation dependend Constants and Macros			*/
/*									*/
/************************************************************************/

/* #define SUNOS403 */
/* #define SUNOS41  */
#define SUNOS411

#ifdef SUNOS411
#define SUNOS41
#endif


#define	GRAPHED


#ifndef MAXINT
#define MAXINT ((1<<31)-1)	/* maximum integer, machine dependent	*/
#endif

#define MAX_FONTS     255	/* Font table		*/
#define MAX_NODETYPE  255	/* Nodetype table	*/
#define MAX_EDGETYPE  255	/* Edgetype table	*/

#define DEFAULT_CURSOR_WIDTH  16	/* SunView Cursor		*/
#define DEFAULT_CURSOR_HEIGHT 16
#define DEFAULT_CURSOR_DEPTH  1
#define DEFAULT_ICON_WIDTH    64	/* SunView Icon			*/
#define DEFAULT_ICON_HEIGHT   64
#define DEFAULT_ICON_DEPTH    1

#define DEFAULT_PANEL_VALUE_DISPLAY_LENGTH 40	/* SunView PANEL_TEXT	*/

#define FILENAMESIZE MAXPATHLEN			/* -> sys/param.h	*/
#define FONT_ID_SIZE 40				/* -> font.c		*/

#define GRAPHED_INITIALISATION_FILE ".graphed"

/* The following Macros are just for hysterical reasons	*/
#define GRAPHED_DEFAULT_INITIALISATION_FILE graphed_default_initialisation_file  
#define	GRAPHED_DEFAULT_TYPES_DIRECTORY graphed_default_types_directory     
#define	GRAPHED_DEFAULT_FONTS_DIRECTORY	graphed_default_fonts_directory     
#define GRAPHED_LIBRARY_DIRECTORY       graphed_library_directory 

/* Some full-path filenames	*/
extern	char	graphed_default_initialisation_file [];
extern	char	graphed_default_types_directory     [];
extern	char	graphed_default_fonts_directory     [];
extern	char	graphed_library_directory           [];


extern	char	graphed_postscript_header[];	/* Head and tail templates */
extern	char	graphed_postscript_tail[];	/* for postscript files    */


/* Some common control characters...	*/

#define ESC        033		/* Escape		*/	
#define DEL        0177		/* Delete		*/
#define CR         015		/* Carriage Return	*/

/* From ascii(8) ...	*/

#define CTRL_A     1
#define	CTRL_B     2
#define	CTRL_C     3
#define CTRL_D     4
#define CTRL_E     5
#define	CTRL_F     6
#define	CTRL_G     7
#define CTRL_H     8
#define CTRL_I     9
#define	CTRL_J     10
#define	CTRL_K     11
#define CTRL_L     12
#define CTRL_M     13
#define	CTRL_N     14
#define	CTRL_O     15
#define CTRL_P     16
#define CTRL_Q     17
#define	CTRL_R     18
#define	CTRL_S     19
#define CTRL_T     20
#define CTRL_U     21
#define	CTRL_V     22
#define	CTRL_W     23
#define CTRL_X     24
#define	CTRL_Y     25
#define CTRL_Z     26

#define META_A     (CTRL_A + 128 + 96)
#define	META_B     (CTRL_B + 128 + 96)
#define	META_C     (CTRL_C + 128 + 96)
#define META_D     (CTRL_D + 128 + 96)
#define META_E     (CTRL_E + 128 + 96)
#define	META_F     (CTRL_F + 128 + 96)
#define	META_G     (CTRL_G + 128 + 96)
#define META_H     (CTRL_H + 128 + 96)
#define META_I     (CTRL_I + 128 + 96)
#define	META_J     (CTRL_J + 128 + 96)
#define	META_K     (CTRL_K + 128 + 96)
#define META_L     (CTRL_L + 128 + 96)
#define META_M     (CTRL_M + 128 + 96)
#define	META_N     (CTRL_N + 128 + 96)
#define	META_O     (CTRL_O + 128 + 96)
#define META_P     (CTRL_P + 128 + 96)
#define META_Q     (CTRL_Q + 128 + 96)
#define	META_R     (CTRL_R + 128 + 96)
#define	META_S     (CTRL_S + 128 + 96)
#define META_T     (CTRL_T + 128 + 96)
#define META_U     (CTRL_U + 128 + 96)
#define	META_V     (CTRL_V + 128 + 96)
#define	META_W     (CTRL_W + 128 + 96)
#define META_X     (CTRL_X + 128 + 96)
#define	META_Y     (CTRL_Y + 128 + 96)
#define META_Z     (CTRL_Z + 128 + 96)

#define SHIFT_META_A     (CTRL_A + 128 + 64)
#define	SHIFT_META_B     (CTRL_B + 128 + 64)
#define	SHIFT_META_C     (CTRL_C + 128 + 64)
#define SHIFT_META_D     (CTRL_D + 128 + 64)
#define SHIFT_META_E     (CTRL_E + 128 + 64)
#define	SHIFT_META_F     (CTRL_F + 128 + 64)
#define	SHIFT_META_G     (CTRL_G + 128 + 64)
#define SHIFT_META_H     (CTRL_H + 128 + 64)
#define SHIFT_META_I     (CTRL_I + 128 + 64)
#define	SHIFT_META_J     (CTRL_J + 128 + 64)
#define	SHIFT_META_K     (CTRL_K + 128 + 64)
#define SHIFT_META_L     (CTRL_L + 128 + 64)
#define SHIFT_META_M     (CTRL_M + 128 + 64)
#define	SHIFT_META_N     (CTRL_N + 128 + 64)
#define	SHIFT_META_O     (CTRL_O + 128 + 64)
#define SHIFT_META_P     (CTRL_P + 128 + 64)
#define SHIFT_META_Q     (CTRL_Q + 128 + 64)
#define	SHIFT_META_R     (CTRL_R + 128 + 64)
#define	SHIFT_META_S     (CTRL_S + 128 + 64)
#define SHIFT_META_T     (CTRL_T + 128 + 64)
#define SHIFT_META_U     (CTRL_U + 128 + 64)
#define	SHIFT_META_V     (CTRL_V + 128 + 64)
#define	SHIFT_META_W     (CTRL_W + 128 + 64)
#define SHIFT_META_X     (CTRL_X + 128 + 64)
#define	SHIFT_META_Y     (CTRL_Y + 128 + 64)
#define SHIFT_META_Z     (CTRL_Z + 128 + 64)

#define CTRL_META_A     (CTRL_A + 128)
#define	CTRL_META_B     (CTRL_B + 128)
#define	CTRL_META_C     (CTRL_C + 128)
#define CTRL_META_D     (CTRL_D + 128)
#define CTRL_META_E     (CTRL_E + 128)
#define	CTRL_META_F     (CTRL_F + 128)
#define	CTRL_META_G     (CTRL_G + 128)
#define CTRL_META_H     (CTRL_H + 128)
#define CTRL_META_I     (CTRL_I + 128)
#define	CTRL_META_J     (CTRL_J + 128)
#define	CTRL_META_K     (CTRL_K + 128)
#define CTRL_META_L     (CTRL_L + 128)
#define CTRL_META_M     (CTRL_M + 128)
#define	CTRL_META_N     (CTRL_N + 128)
#define	CTRL_META_O     (CTRL_O + 128)
#define CTRL_META_P     (CTRL_P + 128)
#define CTRL_META_Q     (CTRL_Q + 128)
#define	CTRL_META_R     (CTRL_R + 128)
#define	CTRL_META_S     (CTRL_S + 128)
#define CTRL_META_T     (CTRL_T + 128)
#define CTRL_META_U     (CTRL_U + 128)
#define	CTRL_META_V     (CTRL_V + 128)
#define	CTRL_META_W     (CTRL_W + 128)
#define CTRL_META_X     (CTRL_X + 128)
#define	CTRL_META_Y     (CTRL_Y + 128)
#define CTRL_META_Z     (CTRL_Z + 128)

#define	ESC_STRING "\033"	/* Escape as string	*/
#define CR_STRING  "\015"	/* CR as String		*/


#define	TABSIZE    8		/* Tabulators <-> Blanks		*/

#define	ESCAPE_CHARACTER '\\'	/* UNIX-Standard	*/

#define	SYSTEM_TYPES_IDENTIFICATION_CHARACTER '#'	/* ->type.c	*/


/*	Rect and Rectlist Macros					*/
/*									*/
/*	Rect		*r;						*/
/*	Rectlist	*rl;						*/
/*	Rectnode	*rn;						*/

#define rect_left(r)   ((r)->r_left)
#define rect_top(r)    ((r)->r_top)
#define rect_width(r)  ((r)->r_width)
#define rect_height(r) ((r)->r_height)

#define rectlist_head(rl) ((rl)->rl_head)
#define rectlist_tail(rl) ((rl)->rl_tail)
#define rectnode_next(rn) ((rn)->rn_next)
#define rectnode_rect(rn) ((rn)->rn_rect)


#define	BEGIN_GRAPH_INTERNALS_STRING "{$"	/* -> store.c,		*/
#define	END_GRAPH_INTERNALS_STRING   "$}"	/*   scanner.c		*/



#define DOTSIZE 2		/* Pixel size of a grid point		*/
#define	MARKER_SQUARE_SIZE 6	/* Pixel size of node/edge/group marker	*/


/* Multi-Klick								*/
/* Maximum distance for Multi-click : Time, Space			*/

#define	DEFAULT_MULTI_CLICK_SPACE	2	/* Pixel		*/
#define	DEFAULT_MULTI_CLICK_TIMEOUT	500	/* Milliseconds		*/


#define	DRAG_GROUP_TRESHHOLD 25
	/* Drag fast if a group is over DRAGH_GROUP_TRESHHOLD nodes	*/

#define GRAPHED2
#define	GRAPHED_VERSION "3.0aPL1"

#define	GRAPHED_COLORMAP_NAME "graphed-colormap"

#define	NODELABEL_GAP 2

/* Configuration procedure	*/

extern	void	init_config ();
