/*
 *	Copyright (c) 1990 by Columbia University.
 */

#ifndef __ICE_DEFINES__
#define __ICE_DEFINES__

#include <rasterfile.h>
#include <pixrect/pixrect_hs.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <local/lxt.h>

#include "Path.h"
#include "Grobj.h"
#include "Composite.h"
#include "Pathdir.h"

#define MAX_FILENMLEN		(255)
#define MAX_GEOMLEN		(35)
#define MAX_ERRMSGLEN		(255)
#define MAX_FONTFAMILYLEN	(255)

#define PI			((double) 3.1415926536)

#define PSEUDOCOLOR_MAPSZ	(256)
#define STATICCOLOR_FREESZ	(16)
#define STATICCOLOR_CUBESZ	(PSEUDOCOLOR_MAPSZ-STATICCOLOR_FREESZ)

#define INPUT_BUFSZ		(1024)

#define FATAL			(0)
#define NONFATAL		(1)

#define STDIN_EMPTY		(0)
#define STDIN_ICE		(1)
#define STDIN_POSTSCRIPT	(2)
#define STDIN_RASTER		(3)

#define FONT			"8x13"
#define	GEOMETRY		"=600x600+100+100"
#define	BORDER_WIDTH		(2)

#define	WMPR_ATOMS		(2)

#define PAGE_FRAMEWIDTH		(400)
#define PAGE_FRAMEHEIGHT	(300)
#define PAGE_WIDTH		(600)
#define PAGE_HEIGHT		(500)

#define ATTR_FRAMEWIDTH		(350)
#define ATTR_FRAMEHEIGHT	(150)

#define PG_PIXELS		(0)
#define PG_POINTS		(1)
#define PG_INCHES		(2)
#define PG_USER			(3)

#define PG_AUTOMATIC		(0)
#define PG_MANUAL		(1)

#define PG_CURSORLOC		(0)
#define PG_TEXTLOC		(1)

#define PG_DISPLAYLOC		(0)
#define PG_NODISPLAYLOC		(1)

#define PG_NOORIGINHLT		(0)
#define PG_ORIGINHLT		(1)

#define PG_WHITEBG		(0)
#define PG_BLACKBG		(1)
#define PG_OTHERBG		(2)

#define GDF_BLACKFG		(0)
#define GDF_WHITEFG		(1)
#define GDF_OTHERFG		(2)

#define GDF_WHITEBG		(0)
#define GDF_BLACKBG		(1)
#define GDF_OTHERBG		(2)

#define GDF_SQUARE		(0)
#define GDF_TRIANGLE		(1)
#define GDF_CIRCLE		(2)
#define GDF_CROSS		(3)

#define GDF_BLACKBND		(0)
#define GDF_WHITEBND		(1)
#define GDF_OTHERBND		(2)

#define GDF_WHITEFILL		(0)
#define GDF_BLACKFILL		(1)
#define GDF_OTHERFILL		(2)

#define GDF_WHITEDTK		(0)
#define GDF_BLACKDTK		(1)
#define GDF_OTHERDTK		(2)

#define INS_DEFAULTS		(0)
#define INS_LASTEDIT		(1)

#define RECT_CURSORDIM		(0)
#define RECT_TEXTDIM		(1)

#define CMP_NOSCALEATTR		(0)
#define CMP_SCALEATTR		(1)

#define PTH_USERINPUT		(0)
#define PTH_FILEINPUT		(1)

#define DMPIPS_ICE		(0x1)
#define DMPIPS_PS		(0x2)

#define DMPIPS_RASEXCLUDE	(0)
#define DMPIPS_RASINCLUDE	(1)

#define INSICE_CURRPG		(0)
#define INSICE_NEWPG		(1)

#define INSICE_CURRGDF		(0)
#define INSICE_NEWGDF		(1)

#define INSICE_CURROBJCURRGDF	(0)
#define INSICE_CURROBJNEWGDF	(1)

#define INSICE_NEWOBJNEWGDF	(0)
#define INSICE_NEWOBJCURRGDF	(1)

#define SEL_ATOMIC		(0)
#define SEL_ROOTPARENT		(1)

/* user ops */
#define NULL_OP			(0)
#define MAIN_MENU		(1)
#define SEL_ATTR		(2)
#define SEL_DELETE		(3)
#define SEL_TRANSLATE		(4)
#define SEL_COPY		(5)
#define SEL_DUMP		(6)
#define PAGE_ATTR		(7)
#define GDF_ATTR		(8)
#define INS_ATTR		(9)
#define PSD_INSERTATTR		(10)
#define PSD_INSERTLOC		(11)
#define PSD_ATTR		(12)
#define PSD_TRANSLATE		(13)
#define PSD_COPY		(14)
#define RAS_INSERTATTR		(15)
#define RAS_INSERTLOC		(16)
#define RAS_ATTR		(17)
#define RAS_TRANSLATE		(18)
#define RAS_COPY		(19)
#define TEXT_INSERTATTR		(20)
#define TEXT_INSERTLOC		(21)
#define TEXT_ATTR		(22)
#define TEXT_TRANSLATE		(23)
#define TEXT_COPY		(24)
#define VEC_INSERTATTR		(25)
#define VEC_INSERTLOC1		(26)
#define VEC_INSERTLOC2		(27)
#define VEC_ATTR		(28)
#define VEC_TRANSLATE		(29)
#define VEC_COPY		(30)
#define VEC_MVLOC1		(31)
#define VEC_MVLOC2		(32)
#define CRV_INSERTATTR		(33)
#define CRV_INSERTLOC1		(34)
#define CRV_INSERTLOC2		(35)
#define CRV_INSERTCNT1		(36)
#define CRV_INSERTCNT2		(37)
#define CRV_ATTR		(38)
#define CRV_TRANSLATE		(39)
#define CRV_COPY		(40)
#define CRV_MVLOC1		(41)
#define CRV_MVLOC2		(42)
#define CRV_MVCNT1		(43)
#define CRV_MVCNT2		(44)
#define MRK_INSERTATTR		(45)
#define MRK_INSERTLOC		(46)
#define MRK_ATTR		(47)
#define MRK_TRANSLATE		(48)
#define MRK_COPY		(49)
#define RECT_INSERTATTR		(50)
#define RECT_INSERTLOC		(51)
#define RECT_INSERTROT		(52)
#define RECT_INSERTDIM		(53)
#define RECT_ATTR		(54)
#define RECT_TRANSLATE		(55)
#define RECT_COPY		(56)
#define POLY_INSERTATTR		(57)
#define POLY_INSERTLOC		(58)
#define POLY_ATTR		(59)
#define POLY_TRANSLATE		(60)
#define POLY_COPY		(61)
#define AXIS_INSERTATTR		(62)
#define AXIS_INSERTLOC1		(63)
#define AXIS_INSERTLOC2		(64)
#define AXIS_ATTR		(65)
#define AXIS_TRANSLATE		(66)
#define AXIS_COPY		(67)
#define AXIS_MVLOC1		(68)
#define AXIS_MVLOC2		(69)
#define CMP_BIND		(70)
#define CMP_ATTR		(71)
#define CMP_UNBIND		(72)
#define CMP_ADD			(73)
#define CMP_REMOVE		(74)
#define CMP_TRANSLATE		(75)
#define CMP_COPY		(76)
#define CMP_DELETE		(77)
#define PATH_INSERTATTR		(78)
#define PATH_INSERTLOC		(79)
#define PATH_ATTR		(80)
#define PATH_TRANSLATE		(81)
#define ICE_INSERT		(82)
#define DUMP_ALL		(83)
#define DUMP_OBJECT		(84)
#define DELETE_ALL		(85)

struct PSFontFamily {
	char *psff_name;
	int psff_count;
	Panel_item *psff_gdfpi;
	Panel_item *psff_textpi;
	Panel_item *psff_axispi;
	char **psff_fonts;
};

#endif __ICE_DEFINES__
