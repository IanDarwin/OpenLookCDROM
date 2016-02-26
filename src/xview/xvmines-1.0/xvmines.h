#define HORIZ_CELLS  10
#define VERT_CELLS   10
#define NUM_MINES    15
#define IC_WIDTH   32
#define IC_HEIGHT  32

#define EMPTY   0
#define ONE     1
#define TWO     2
#define THREE   3
#define FOUR    4
#define FIVE 	5
#define SIX	6
#define SEVEN	7
#define EIGHT   8
#define CLOSED  9
#define MARKED  10
#define MINE    11
#define QUEST   12
#define NUM_ICONS 13


#define FALSE      0
#define TRUE       1

#define CLEAR_YES  0
#define CLEAR_NO   1

#define QUANTUM   5

#define KEY       22

#define MAX_WIDTH   35
#define MAX_HEIGHT  24
#define MSG_CANVAS_HEIGHT 25
#define TIME_X 7
#define TIME_Y 17
#define RESULT_X 150
#define RESULT_Y 17

typedef struct _cell {
	int             in_state;	/* core state */
	int             out_state;	/* displayed state */
}               cell;

#define PLACE_ICON(icon,wherey,wherex) XCopyPlane(display,icon,pmap,def_gc,0,0, \
                                          IC_WIDTH,IC_HEIGHT,wherex,wherey,1L)

/* function prototypes */
#ifdef ANSI
int             get_cell(int, int, int *, int *);
int             place_tiles(int, int, int);
int             open_tile(int, int);
int             mark_tile(int, int);
int             auto_open(int, int);
int             init(void);
int             show_placements(int);
void            finish(char *);
#else
int             get_cell();
int             place_tiles();
int             open_tile();
int             mark_tile();
int             auto_open();
int             init();
int             show_placements();
void            finish();
#endif
