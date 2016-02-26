/*
 * @(#)gcm.h	1.13 90/05/27 Copyright 1989 Sun Microsystems
 *
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/*
 * GUIDE colormap segment support functions.
 */

#ifndef guide_gcm_DEFINED
#define guide_gcm_DEFINED

#include	<c_varieties.h>

EXTERN_FUNCTION( char	*gcm_colormap_name,	(_VOID_) );
EXTERN_FUNCTION( void	gcm_initialize_colors,	(Xv_opaque, char *, char *) );
EXTERN_FUNCTION( int	gcm_color_index,	(char *) );

#define GUIDE_COLOR_LIST	\
	"Aquamarine", "Black", "Blue", "Blue Violet", "Brown", "Cadet Blue", \
	"Coral", "Cornflower Blue", "Cyan", "Dark Green", "Dark Olive Green", \
	"Dark Orchid", "Dark Slate Blue", "Dark Slate Gray", "Dark Turquoise", \
	"Dim Gray", "Firebrick", "Forest Green", "Gold", "Goldenrod", "Gray", \
	"Green", "Green Yellow", "Indian Red", "Khaki", "Light Blue", \
	"Light Gray", "Light Steel Blue", "Lime Green", "Magenta", "Maroon", \
	"Medium Aquamarine", "Medium Blue", "Medium Orchid", \
	"Medium Sea Green", "Medium Slate Blue", "Medium Spring Green", \
	"Medium Turquoise", "Medium Violet Red", "Midnight Blue", "Navy", \
	"Navy Blue", "Olive Drab", "Orange", "Orange Red", "Orchid", \
	"Pale Green", "Pink", "Plum", "Red", "Salmon", "Sea Green", "Sienna", \
	"Sky Blue", "Slate Blue", "Spring Green", "Steel Blue", "Tan", \
	"Thistle", "Turquoise", "Violet", "Violet Red", "Wheat", "White", \
	"Yellow", "Yellow Green"

#endif ~guide_gcm_DEFINED
