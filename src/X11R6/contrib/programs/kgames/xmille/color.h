/*
 * color.h
 *
 * definitions for standard colors
 */

# define BLACK_COLOR	0
# define WHITE_COLOR	1
# define RED_COLOR	2
# define GREEN_COLOR	3
# define GREY_COLOR	4
# define BLUE_COLOR	5
# define NUM_COLOR	6


struct color {
	char	*name;
	GC	gc;
	int	pixel;
};

extern struct color	colorMap[NUM_COLOR];
