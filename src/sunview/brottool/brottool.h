/* Number of points to plot before updating the screen */
#define STEP		5000

/* Default seed values */
#define DEF_A		(-22938)
#define DEF_B		(-1638)
#define DEF_C		221459251

/* Index of default screen magnification factor (1, 2, 4, 8, 16, 32, 64, 128) */
#define DEF_FAC		6

/* Size of display area:
** X=(DEF_SIZE << (WIN_SHIFT+1)),
** Y=(DEF_SIZE << (WIN_SHIFT-1))
*/
#define DEF_SIZE	512
#define WIN_SHIFT	3

/* Default X and Y offsets into display area (NOT the same as scrolling) */
#define DEF_X_OFF	100
#define DEF_Y_OFF	120

/* For scrollbars, display size */
#define MAX_X_OFF	(DEF_SIZE << (WIN_SHIFT+1))
#define MAX_Y_OFF	(DEF_SIZE << (WIN_SHIFT-1))

/* Number of bits in fixed point calculations, probably shouldn't mess
** with this one.  14 seems to overflow, 12 gives poor (or poorer) results.
** This seems to be the best.
*/
#define SHIFT		13

