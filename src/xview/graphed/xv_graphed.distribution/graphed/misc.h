/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef	MISC_HEADER
#define	MISC_HEADER


#include <stdio.h>
#include <string.h>
#ifndef MALLOC_HEADER
#define MALLOC_HEADER
#include <malloc.h>
#endif
#include <math.h>
#include <xview/base.h>
#include <xview/rect.h>
#include <pixrect/pixrect_hs.h>
#include <sys/param.h>
#include <assert.h>
#ifdef __GNUC__
#include <varargs.h>
#else
#include <varargs.h>
#endif

#include "error.h"
#include "config.h"

extern	char	*getenv(); /* Don't know where it is defined	*/

/************************************************************************/
/*									*/
/*			Naming Conventions (excerpt)			*/
/*									*/
/*	- All type Names start with capital letter			*/
/*	- _string(s) are string representations, mostly for enumeration	*/
/*	  types								*/
/*									*/
/************************************************************************/

 
/************************************************************************/
/*			FONTS						*/
/************************************************************************/


typedef	struct	graphed_font
{
	struct	pixfont	*font;
	char		*id;
	char		*filename;
	int		used;
}
	*Graphed_font;


/************************************************************************/
/*			NODETYPES					*/
/************************************************************************/


typedef	struct	nodetypeimage
{
	int			sx,sy;
	Pixrect		*pr;
	int			used;
	struct	nodetypeimage	*pre, *suc;
}
	*Nodetypeimage;

typedef	struct	nodetype
{
	Nodetypeimage	images;
	Pixrect	*pr;
	char		*filename;
	int		used;
	int		is_system;
	void		(*adjust_func)();	/* Valid only if	*/
	void		(*pr_paint_func)();	/* is_system = TRUE	*/
	void		(*laser_paint_func)();	/*			*/
}
	*Nodetype;


/************************************************************************/
/*		Edgetypes						*/
/************************************************************************/


typedef	struct	edgetype
{
	Pixrect		*pr;
	struct pr_texture  	*texture;
	char			*filename;
	int			used;
}
	*Edgetype;



/************************************************************************/
/*			SCALING						*/
/************************************************************************/


typedef	enum {
	SCALE_16_16,
	SCALE_32_32,
	SCALE_64_64,
	SCALE_96_96,
	SCALE_128_128,
	SCALE_192_192,
	SCALE_256_256,
	SCALE_384_384,
	SCALE_512_512,
	SCALE_IDENTITY,
	SCALE_DOWN_XY,
	SCALE_DOWN_X,
	SCALE_DOWN_Y,
	SCALE_UP_XY,
	SCALE_UP_X,
	SCALE_UP_Y,
	SCALE_SQUARE_X,
	SCALE_SQUARE_Y,
	NUMBER_OF_SCALINGS
}
	Scaling;
	
extern	char	*scaling_strings[];
extern	char	*scaling_strings_for_cycle[];
extern	void	scale ();
extern	Scaling	size_to_scale ();


/************************************************************************/
/*			DIVERSA (MISC)					*/
/************************************************************************/


#define iif(b,e1,e2) ( (b) ? (e1) : (e2) )
#define	maximum(x,y) iif( (x) > (y), (x), (y))
#define	minimum(x,y) iif( (x) < (y), (x), (y))
#define	sgn(x) iif((x)>0, 1, iif((x)==0, 0, -1))
#define	dist_2(x1,y1,x2,y2) (((x1)-(x2))*((x1)-(x2)) + ((y1)-(y2))*((y1)-(y2)))
				/* Square of euclidian distance	*/

typedef enum { NODE,   EDGE   }  Node_or_edge;
typedef enum { SOURCE, TARGET }  Source_or_target;
typedef enum { LOAD,   STORE  }  Load_or_store;

extern	char	**split_string();
extern	void	free_lines ();

extern	int	find_min_distance_between_pointclusters ();
extern	void	write_quoted_text                       ();
extern	char	*remove_escape_characters_from_text     ();
extern	char	*remove_control_chars_from_string       ();

extern	void	display_files                           ();
extern	int	check_file_is_single                    ();
extern	int	file_exists                             ();
extern	char	*file_exists_somewhere                  ();

extern	char	*mymalloc                               ();
extern	char	*mycalloc                               ();

extern	void	constrain_8				();
extern	void	constrain_to_grid			();
extern	int	rad_to_deg                              ();
extern	float	deg_to_rad                              ();
extern	char	*int_to_ascii                           ();
extern	char	*float_to_ascii                         ();

extern	int	ticks		();

extern	char	*strsave ();	/* Deklariert in sgraph/std.h	*/


/*	Transformation bool --> 0,1     TRUE --> 1,  FALSE --> 0	*/
/*	und zurueck							*/
/*	sowie Liste aus Werten x,y gemaess Ordnung von b1,b2		*/


#define	bool_to_int(b)	iif ((b) == TRUE, 1, 0)
#define int_to_bool(n)	iif ((n) == 1, TRUE, FALSE)

#define	boolean_ordered_list(x,b1, y,b2)             \
	iif (bool_to_int(b1) < bool_to_int(b2), (x), (y)),  \
	iif (bool_to_int(b1) < bool_to_int(b2), (y), (x))

#endif
