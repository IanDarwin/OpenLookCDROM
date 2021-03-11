/* File: hz2ps.h */

/***************************************************************************

  Copyright 1993  Fung F. Lee

***************************************************************************/

#define FALSE		0
#define TRUE		1

#define GB_SPACE	0xA1A1
#define GB_SQUARE	0xA1F5
#define BIG_SPACE	0xA140
#define BIG_SQUARE	0xA1BC

#define MAX_ROW		64	/* maximum height of an hanzi */
#define MAX_COL 	132	/* maximum number of columns on terminal */
#define MAX_S_LEN	80	/* maximum English string length */
#define MAX_FONT_NAME	256	/* maximum length of the font full path name */
#define PT_PER_IN	72	/* points per inch */

#define MAX(a, b)	((a) >= (b) ? (a) : (b))
#define MIN(a, b)	((a) >= (b) ? (b) : (a))
#define TO_CODE(hi,lo)	(((hi) & 0xFF) << 8 | (lo) & 0xFF)

typedef int		BOOL;
typedef unsigned int	UINT;
typedef unsigned char	UCHAR;
typedef enum {HH, VV}	PRINT_MODE;
typedef enum {GB, BIG5} CODE_SCHEME;

#ifdef USE_OLD
#define	FONT_DIR	"HZFONTDIR"
#define	FONT_EXT	".hbf"
#endif /* USE_OLD */

#define HZLIB		"HZLIB"

#ifdef msdos
#define	PATH_SEPARATOR	"\\"
#else
#define	PATH_SEPARATOR	"/"
#endif

#define	DEFAULT_HZ_FONT	"ccs24.hbf"
#define	DEFAULT_E_FONT	"Courier"
#define	PROLOG		"hz2ps.pro"
