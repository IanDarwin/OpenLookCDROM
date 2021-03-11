/*
 * Copyright 1993 by Ross Paterson
 *
 * - You can use this code for any purpose and without fee, except that
 *   you may distribute only verbatim copies of it.  Please send me any
 *   bug fixes, ports or improvements for inclusion in future versions.
 *
 * - The code is supplied with no warranty of any kind.
 *
 * Ross Paterson <rap@doc.ic.ac.uk>
 * 8 October 1993
 *
 * The following people have supplied bug fixes:
 *	Man-Chi Pong	<mcpong@cs.ust.hk>
 *	Fung Fung Lee	<lee@simd.stanford.edu>
 *	Steven Simpson	<simpson@math.psu.edu>
 *	Charles Wang	<charles.wang@infores.com>
 */

/*
 * Two C interfaces to HBF files.
 *
 * The multiple interfaces make this code rather messy; I intend
 * to clean it up as experience is gained on what is really needed.
 */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "hbf.h"

#ifdef __MSDOS__
#define msdos
#endif

/*
 * if the linker complains about an unresolved identifier '_strdup',
 * uncomment the following definition.
 */
/* #define NO_STRDUP */

#define	reg	register

typedef	int	bool;
#define	TRUE	1
#define	FALSE	0

#define	Bit(n)	(1<<(7 - (n)))

/*
 * Messy file system issues
 */

#ifdef unix
#define	PATH_DELIMITER ':'
#define	RelativeFileName(fn)	((fn)[0] != '/')
#define	LocalFileName(fn)	(strchr(fn, '/') == NULL)
#endif /* unix */
#ifdef msdos
#define	PATH_DELIMITER ';'
#define	HasDrive(fn)	(isalpha((fn)[0]) && (fn)[1] == ':')
#define	RelativeFileName(fn)	(! HasDrive(fn) && (fn)[0] != '\\')
#define	LocalFileName(fn)	(! HasDrive(fn) && strchr(fn, '\\') == NULL)
#define	READ_BINARY	"rb"
#endif /* msdos */
#ifdef vms
#define	PATH_DELIMITER ','
#define	RelativeFileName(fn)	(strchr(fn, ':') == NULL && ((fn)[0] != '[' || (fn)[1] == '.' || (fn)[1] == '-'))
#define	LocalFileName(fn)	(strchr(fn, ':') == NULL && strchr(fn, ']') == NULL)
#endif

#ifndef	RelativeFileName
#define	RelativeFileName(fn)	FALSE
#endif

#ifndef	LocalFileName
#define	LocalFileName(fn)	FALSE
#endif

#ifndef READ_BINARY
#define	READ_BINARY	"r"
#endif

#define	MAX_FILENAME	1024

/*
 *	Internal structures
 */

typedef	unsigned char Uchar;

#define PROPERTY	struct _PROPERTY
#define BM_FILE		struct _BM_FILE
#define B2_RANGE	struct _B2_RANGE
#define CODE_RANGE	struct _CODE_RANGE

PROPERTY {
	char		*prop_name;
	char		*prop_value;
	PROPERTY	*prop_next;
};

BM_FILE {
	char	*bmf_name;
	FILE	*bmf_file;
	BM_FILE	*bmf_next;
};

B2_RANGE {
	Uchar	b2r_start;
	Uchar	b2r_finish;
	B2_RANGE	*b2r_next;
};

CODE_RANGE {
	unsigned short	code_start;
	unsigned short	code_finish;
	BM_FILE		*code_bm_file;
	unsigned long	code_offset;
	unsigned int	code_pos;
	bool		code_sideways;
	CODE_RANGE	*code_next;
};

/*
 *	Extended internal version of HBF
 */

typedef struct {
	/* fields corresponding to the definition */
	HBF		public;
	/* plus internal stuff */
	char		*filename;
	unsigned char	*bitmap_buffer;
	unsigned int	b2_size;	/* number of legal byte-2's */
	PROPERTY	*property;
	B2_RANGE	*byte_2_range;
	CODE_RANGE	*code_range;
	BM_FILE		*bm_file;
} HBF_STRUCT;

#define	FirstByte(code)		((code)>>8)
#define	SecondByte(code)	((code)&0xff)
#define	MakeCode(byte1,byte2)	(((byte1)<<8)|(byte2))

#define	NEW(type)	((type *)malloc((unsigned)(sizeof(type))))

#define	QUOTE '"'

#define MAXLINE	1024

/* Error reporting */

int	hbfDebug;	/* set this for error reporting */

void
eprintf(fmt, x1, x2, x3, x4, x5, x6, x7, x8, x9)
	const	char	*fmt;
	int	x1, x2, x3, x4, x5, x6, x7, x8, x9;
{
	if (hbfDebug) {
		fprintf(stderr, "HBF: ");
		fprintf(stderr, fmt, x1, x2, x3, x4, x5, x6, x7, x8, x9);
		fprintf(stderr, "\n");
	}
}

static void
clear_bbox(bbox)
	HBF_BBOX *bbox;
{
	bbox->hbf_width = bbox->hbf_height = 0;
	bbox->hbf_xDisplacement = bbox->hbf_yDisplacement = 0;
}

static void
clear_record(hbf)
	HBF_STRUCT *hbf;
{
	clear_bbox(&(hbf->public.hbf_bitmap_bbox));
	clear_bbox(&(hbf->public.hbf_font_bbox));
	hbf->property = NULL;
	hbf->filename = NULL;
	hbf->bitmap_buffer = NULL;
	hbf->byte_2_range = NULL;
	hbf->code_range = NULL;
	hbf->bm_file = NULL;
}

/*
 *	Byte-2 ranges
 */

static void
add_b2r(last_b2r, start, finish)
reg	B2_RANGE **last_b2r;
	int	start;
	int	finish;
{
	B2_RANGE *b2r;

	b2r = NEW(B2_RANGE);
	while (*last_b2r != NULL && (*last_b2r)->b2r_start < start)
		last_b2r = &((*last_b2r)->b2r_next);
	b2r->b2r_next = *last_b2r;
	b2r->b2r_start = start;
	b2r->b2r_finish = finish;
	*last_b2r = b2r;
}

static int
b2_pos(hbf, code)
	HBF_STRUCT	*hbf;
	HBF_CHAR	code;
{
reg	B2_RANGE *b2r;
reg	unsigned c;
reg	int	pos;

	c = SecondByte(code);
	pos = 0;
	for (b2r = hbf->byte_2_range; b2r != NULL; b2r = b2r->b2r_next)
		if (b2r->b2r_start <= c && c <= b2r->b2r_finish)
			return pos + c - b2r->b2r_start;
		else
			pos += b2r->b2r_finish - b2r->b2r_start + 1;
	return -1;
}

static int
b2_size(b2r)
reg	B2_RANGE *b2r;
{
reg	int	size;

	size = 0;
	for ( ; b2r != NULL; b2r = b2r->b2r_next)
		size += b2r->b2r_finish - b2r->b2r_start + 1;
	return size;
}

/*
 *	String stuff
 */

static bool
match(lp, sp)
reg	const	char	*lp;
reg	const	char	*sp;
{
	while (*lp == *sp && *sp != '\0') {
		lp++;
		sp++;
	}
	return (*lp == '\0' || isspace(*lp)) && *sp == '\0';
}

#ifdef NO_STRDUP
char *
strdup(s)
	const	char	*s;
{
	char	*new_s;

	new_s = malloc((unsigned)strlen(s) + 1);
	strcpy(new_s, s);
	return new_s;
}
#endif

/*
 *	Properties
 */

static void
add_property(hbf, lp)
reg	HBF_STRUCT	*hbf;
reg	const char	*lp;
{
reg	PROPERTY	*prop;
	char	tmp[MAXLINE];
reg	char	*tp;

	prop = NEW(PROPERTY);

	tp = tmp;
	while (*lp != '\0' && ! isspace(*lp))
		*tp++ = *lp++;
	*tp = '\0';
	prop->prop_name = strdup(tmp);

	while (*lp != '\0' && isspace(*lp))
		lp++;

	tp = tmp;
	if (*lp == QUOTE) {
		lp++;
		while (*lp != '\0' && ! (*lp == QUOTE && *++lp != QUOTE))
			*tp++ = *lp++;
	}
	else
		for (;;) {
			while (*lp != '\0' && ! isspace(*lp))
				*tp++ = *lp++;
			while (isspace(*lp))
				lp++;
			if (*lp == '\0')
				break;
			*tp++ = ' ';
		}
	*tp = '\0';
	prop->prop_value = strdup(tmp);

	prop->prop_next = hbf->property;
	hbf->property = prop;
}

const char *
hbfProperty(hbfFile, propName)
	HBF		*hbfFile;
	const	char	*propName;
{
reg	HBF_STRUCT	*hbf;
reg	PROPERTY	*prop;

	hbf = (HBF_STRUCT *)hbfFile;
	for (prop = hbf->property; prop != NULL; prop = prop->prop_next)
		if (strcmp(prop->prop_name, propName) == 0)
			return prop->prop_value;
	return NULL;
}

/*
 *	Compatability routines
 */

const char *
HBF_GetProperty(handle, propertyName)
	HBF		*handle;
	const	char	*propertyName;
{
	return hbfProperty(handle, propertyName);
}

int
HBF_GetFontBoundingBox(handle, width, height, xDisplacement, yDisplacement)
	HBF_Handle	handle;
	unsigned int	*width;
	unsigned int	*height;
	int		*xDisplacement;
	int		*yDisplacement;
{
	if (width != NULL)
		*width = hbfFontBBox(handle)->hbf_width;
	if (height != NULL)
		*height = hbfFontBBox(handle)->hbf_height;
	if (xDisplacement != NULL)
		*xDisplacement = hbfFontBBox(handle)->hbf_xDisplacement;
	if (yDisplacement != NULL)
		*yDisplacement = hbfFontBBox(handle)->hbf_yDisplacement;
	return 0;
}

int
HBF_GetBitmapBoundingBox(handle, width, height, xDisplacement, yDisplacement)
	HBF_Handle	handle;
	unsigned int	*width;
	unsigned int	*height;
	int		*xDisplacement;
	int		*yDisplacement;
{
	if (width != NULL)
		*width = hbfBitmapBBox(handle)->hbf_width;
	if (height != NULL)
		*height = hbfBitmapBBox(handle)->hbf_height;
	if (xDisplacement != NULL)
		*xDisplacement = hbfBitmapBBox(handle)->hbf_xDisplacement;
	if (yDisplacement != NULL)
		*yDisplacement = hbfBitmapBBox(handle)->hbf_yDisplacement;
	return 0;
}

/*
 * Prepend a directory to a relative filename.
 */
static char *
concat(dir, dirlen, stem)
	const	char	*dir;	/* not necessarily null-terminated */
	int	dirlen;		/* number of significant chars in dir */
	const	char	*stem;	/* relative filename */
{
	char	*fullname;

	if (dirlen == 0)	/* null: current directory */
		return strdup(stem);
#ifdef unix
	fullname = malloc(dirlen + strlen(stem) + 2);
	(void)sprintf(fullname, "%.*s/%s", dirlen, dir, stem);
#else
#ifdef msdos
	fullname = malloc(dirlen + strlen(stem) + 2);
	(void)sprintf(fullname, "%.*s\\%s", dirlen, dir, stem);
#else
#ifdef vms
	if (dir[dirlen-1] == ']' && stem[0] == '[' && stem[1] == '-') {
		dirlen--;
		stem++;
		fullname = malloc(dirlen + strlen(stem) + 2);
		(void)sprintf(fullname, "%.*s.%s", dirlen, dir, stem);
	}
	else {
		if (dir[dirlen-1] == ']' && stem[0] == '[' && stem[1] == '.') {
			dirlen--;
			stem++;
		}
		fullname = malloc(dirlen + strlen(stem) + 1);
		(void)sprintf(fullname, "%.*s%s", dirlen, dir, stem);
	}
#else
	fullname = strdup(stem);
#endif /* vms */
#endif /* msdos */
#endif /* unix */
	return fullname;
}

/*
 *	Bitmap files
 *
 *	If the host operating system has a heirarchical file system and
 *	the bitmap file name is relative, it is relative to the directory
 *	containing the HBF file.
 */
static char *
expand_filename(name, hbf_name)
	const	char	*name;
	const	char	*hbf_name;
{
#ifdef unix
reg	char	*s;
reg	int	size;

	size = name[0] != '/' && (s = strrchr(hbf_name, '/')) != NULL ?
		s - hbf_name + 1 : 0;
	s = malloc((unsigned)size + strlen(name) + 1);
	(void)sprintf(s, "%.*s%s", size, hbf_name, name);
	return s;
#else
#ifdef msdos
reg	char	*s;
reg	int	size;

	size = HasDrive(name) ? 0 :
		name[0] == '\\' ? (HasDrive(hbf_name) ? 2 : 0) :
		(s = strrchr(hbf_name, '\\')) != NULL ?
			s - hbf_name + 1 : 0;
	s = malloc((unsigned)size + strlen(name) + 1);
	(void)sprintf(s, "%.*s%s", size, hbf_name, name);
	return s;
#else
#ifdef vms
reg	char	*s;
reg	const	char	*copyto;
reg	int	size;

	if ((s = strchr(hbf_name, ']')) != NULL && RelativeFileName(name))
		return concat(hbf_name, (s - hbf_name) + 1, name);

	copyto = hbf_name;
	if ((s = strstr(copyto, "::")) != NULL && strstr(name, "::") == NULL)
		copyto = s+2;
	if ((s = strchr(copyto, ':')) != NULL && strchr(name, ':') == NULL)
		copyto = s+1;
	size = copyto - hbf_name;
	s = malloc((unsigned)size + strlen(name) + 1);
	(void)sprintf(s, "%.*s%s", size, hbf_name, name);
	return s;
#else
	return strdup(s);
#endif /* vms */
#endif /* msdos */
#endif /* unix */
}

static BM_FILE *
find_file(hbf, filename)
	HBF_STRUCT *hbf;
	const char *filename;
{
	BM_FILE	**fp;
reg	BM_FILE	*file;

	for (fp = &(hbf->bm_file); *fp != NULL; fp = &((*fp)->bmf_next))
		if (strcmp((*fp)->bmf_name, filename) == 0)
			return *fp;

	*fp = file = NEW(BM_FILE);
	if (file == NULL) {
		eprintf("out of memory");
		return NULL;
	}
	file->bmf_name = expand_filename(filename, hbf->filename);
	if (file->bmf_name == NULL) {
		free((char *)file);
		return NULL;
	}
	if ((file->bmf_file = fopen(file->bmf_name, READ_BINARY)) == NULL) {
		eprintf("can't open bitmap file '%s'", file->bmf_name);
		free(file->bmf_name);
		free((char *)file);
		return NULL;
	}
	file->bmf_next = NULL;
	return file;
}

/*
 *	Code ranges
 */

static bool
add_code_range(hbf, line)
	HBF_STRUCT	*hbf;
	const char	*line;
{
	CODE_RANGE *cp;
	CODE_RANGE **cpp;
	long	start, finish;
	long	offset;
	char	filename[MAXLINE];
	char	orientation[MAXLINE];
	BM_FILE	*bmf;

	orientation[0] = '\0';
	if (sscanf(line, "HBF_CODE_RANGE %li-%li %s %li %s",
		   &start, &finish, filename, &offset, orientation) < 4) {
		eprintf("syntax error in HBF_CODE_RANGE");
		return FALSE;
	}

	if (start > finish) {
		eprintf("illegal code range 0x%04x-0x%04x", start, finish);
		return FALSE;
	}
	if ((bmf = find_file(hbf, filename)) == NULL)
		return FALSE;
	if ((cp = NEW(CODE_RANGE)) == NULL) {
		eprintf("out of memory");
		return FALSE;
	}
	cp->code_start = start;
	cp->code_finish = finish;
	cp->code_bm_file = bmf;
	cp->code_offset = offset;
	cp->code_sideways = orientation[0] == 's' || orientation[0] == 'S';
	/* insert it in order */
	for (cpp = &hbf->code_range;
	     *cpp != NULL && (*cpp)->code_start < start;
	     cpp = &((*cpp)->code_next))
		;
	cp->code_next = *cpp;
	*cpp = cp;
	return TRUE;
}

/*
 *	Reading and parsing of an HBF file
 */

static bool
parse_file(f, hbf)
	FILE	*f;
reg	HBF_STRUCT *hbf;
{
	char	line[MAXLINE];
	int	w, h, xd, yd;
	int	start, finish;

	while (fgets(line, MAXLINE, f) != NULL) {
		if (sscanf(line, "HBF_BITMAP_BOUNDING_BOX %i %i %i %i",
				&w, &h, &xd, &yd) == 4) {
			hbf->public.hbf_bitmap_bbox.hbf_width = w;
			hbf->public.hbf_bitmap_bbox.hbf_height = h;
			hbf->public.hbf_bitmap_bbox.hbf_xDisplacement = xd;
			hbf->public.hbf_bitmap_bbox.hbf_yDisplacement = yd;
		}
		else if (sscanf(line, "FONTBOUNDINGBOX %i %i %i %i",
				&w, &h, &xd, &yd) == 4) {
			hbf->public.hbf_font_bbox.hbf_width = w;
			hbf->public.hbf_font_bbox.hbf_height = h;
			hbf->public.hbf_font_bbox.hbf_xDisplacement = xd;
			hbf->public.hbf_font_bbox.hbf_yDisplacement = yd;
		}
		else if (match(line, "STARTPROPERTIES"))
			for (;;) {
				if (fgets(line, MAXLINE, f) == NULL)
					return FALSE;
				if (match(line, "ENDPROPERTIES"))
					break;
				if (! match(line, "COMMENT"))
					add_property(hbf, line);
			}
		else if (match(line, "HBF_START_BYTE_2_RANGES"))
			for (;;) {
				if (fgets(line, MAXLINE, f) == NULL)
					return FALSE;
				if (match(line, "HBF_END_BYTE_2_RANGES"))
					break;
				if (sscanf(line, "HBF_BYTE_2_RANGE %i-%i",
					   &start, &finish) == 2)
					add_b2r(&(hbf->byte_2_range),
						start, finish);
			}
		else if (match(line, "HBF_START_CODE_RANGES"))
			for (;;) {
				if (fgets(line, MAXLINE, f) == NULL)
					return FALSE;
				if (match(line, "HBF_END_CODE_RANGES"))
					break;
				if (! match(line, "COMMENT") &&
				    ! add_code_range(hbf, line))
					return FALSE;
		}
		else if (match(line, "HBF_END_FONT"))
			break;
		else if (! match(line, "COMMENT"))
			add_property(hbf, line);
	}

	if (hbf->byte_2_range == NULL)
		add_b2r(&(hbf->byte_2_range), 0, 0xff);
	if (hbf->public.hbf_bitmap_bbox.hbf_height == 0 ||
	    hbf->public.hbf_bitmap_bbox.hbf_width == 0) {
		eprintf("illegal dimensions %dx%d",
			hbf->public.hbf_bitmap_bbox.hbf_height,
			hbf->public.hbf_bitmap_bbox.hbf_width);
		return FALSE;
	}
	if (hbf->code_range == NULL)
		eprintf("no code ranges");
	return hbf->code_range != NULL;
}

static FILE *
path_open(path, filename, fullp)
	const	char	*path;
	const	char	*filename;
	char	**fullp;
{
	if (LocalFileName(filename) && path != NULL) {
#ifdef PATH_DELIMITER
		int	len;
		char	*fullname;
		FILE	*f;
		const	char	*p_next;

		len = strlen(filename);
		for (;;) {
			p_next = strchr(path, PATH_DELIMITER);
			if (p_next == NULL)
				p_next = path + strlen(path);
			fullname = concat(path, p_next - path, filename);
			if ((f = fopen(fullname, "r")) != NULL) {
				*fullp = fullname;
				return f;
			}
			free(fullname);
			if (*p_next == '\0')
				break;
			path = p_next + 1;
		}
#endif
		return NULL;
	}
	else {
		*fullp = strdup(filename);
		return fopen(*fullp, "r");
	}
}

static bool
real_open(filename, hbf)
	const	char	*filename;
reg	HBF_STRUCT *hbf;
{
	FILE	*f;
	CODE_RANGE *cp;
	int	pos;

	f = path_open(getenv("HBFPATH"), filename, &(hbf->filename));
	if (f == NULL) {
		eprintf("can't read file '%s'", filename);
		return FALSE;
	}
	if (! parse_file(f, hbf)) {
		fclose(f);
		return FALSE;
	}
	fclose(f);

	/* set derived fields */
	hbf->b2_size = b2_size(hbf->byte_2_range);
	for (cp = hbf->code_range; cp != NULL; cp = cp->code_next) {
		if ((pos = b2_pos(hbf, cp->code_start)) < 0) {
			eprintf("illegal start code 0x%04x",
				cp->code_start);
			return FALSE;
		}
		cp->code_pos = hbf->b2_size*FirstByte(cp->code_start) + pos;
		if (b2_pos(hbf, cp->code_finish) < 0) {
			eprintf("illegal finish code 0x%04x",
				cp->code_finish);
			return FALSE;
		}
	}
	return TRUE;
}

HBF *
hbfOpen(filename)
	const	char	*filename;
{
reg	HBF_STRUCT *hbf;

	if ((hbf = NEW(HBF_STRUCT)) == NULL) {
		eprintf("can't allocate HBF structure");
		return NULL;
	}
	clear_record(hbf);
	if (real_open(filename, hbf))
		return &(hbf->public);
	hbfClose(&(hbf->public));
	return NULL;
}

int
HBF_OpenFont(filename, ptrHandleStorage)
	const	char	*filename;
	HBF	**ptrHandleStorage;
{
	return (*ptrHandleStorage = hbfOpen(filename)) == NULL ? -1 : 0;
}

/*
 *	Close files, free everything associated with the HBF.
 */

int
HBF_CloseFont(hbfFile)
	HBF	*hbfFile;
{
reg	HBF_STRUCT	*hbf;
	PROPERTY	*prop_ptr, *prop_next;
	B2_RANGE	*b2r_ptr, *b2r_next;
	CODE_RANGE	*code_ptr, *code_next;
	BM_FILE		*bmf_ptr, *bmf_next;
	int		status;

	status = 0;
	hbf = (HBF_STRUCT *)hbfFile;

	if (hbf->filename != NULL)
		free(hbf->filename);
	if (hbf->bitmap_buffer != NULL)
		free(hbf->bitmap_buffer);

	for (prop_ptr = hbf->property;
	     prop_ptr != NULL;
	     prop_ptr = prop_next) {
		prop_next = prop_ptr->prop_next;
		free(prop_ptr->prop_name);
		free(prop_ptr->prop_value);
		free((char *)prop_ptr);
	}

	for (b2r_ptr = hbf->byte_2_range;
	     b2r_ptr != NULL;
	     b2r_ptr = b2r_next) {
		b2r_next = b2r_ptr->b2r_next;
		free((char *)b2r_ptr);
	}

	for (code_ptr = hbf->code_range;
	     code_ptr != NULL;
	     code_ptr = code_next) {
		code_next = code_ptr->code_next;
		free((char *)code_ptr);
	}

	for (bmf_ptr = hbf->bm_file;
	     bmf_ptr != NULL;
	     bmf_ptr = bmf_next) {
		bmf_next = bmf_ptr->bmf_next;
		if (fclose(bmf_ptr->bmf_file) < 0)
			status = -1;
		free(bmf_ptr->bmf_name);
		free((char *)bmf_ptr);
	}

	free((char *)hbf);

	return status;
}

void
hbfClose(hbfFile)
	HBF	*hbfFile;
{
	(void)HBF_CloseFont(hbfFile);
}

/*
 *	Fetch a bitmap
 */

static bool
get_transposed(hbf, f, bitmap)
	HBF	*hbf;
	FILE	*f;
reg	char	*bitmap;
{
reg	char	*pos;
reg	char	*bm_end;
	int	x;
	int	width;
reg	int	row_size;
reg	int	c;
reg	int	imask, omask;

	width = hbfBitmapBBox(hbf)->hbf_width;
	row_size = HBF_RowSize(hbf);
	bm_end = bitmap + HBF_BitmapSize(hbf);
	(void)memset(bitmap, '\0', HBF_BitmapSize(hbf));
	for (x = 0; x < width; x++) {
		pos = bitmap + x/8;
		omask = Bit(x%8);
		/* y = 0 */
		for (;;) {
			if ((c = getc(f)) == EOF)
				return FALSE;
			for (imask = Bit(0); imask != 0; imask >>= 1) {
				/*
				 * At this point,
				 *
				 *	imask == Bit(y%8)
				 *	pos == bitmap + y*row_size + x/8
				 *
				 * We examine bit y of row x of the input,
				 * setting bit x of row y of the output if
				 * required, by applying omask to *pos.
				 */
				if ((c & imask) != 0)
					*pos |= omask;
				/* if (++y > height) goto end_column */
				pos += row_size;
				if (pos >= bm_end)
					goto end_column;
			}
		}
end_column:
		;
	}
	return TRUE;
}

const unsigned char *
hbfGetBitmap(hbfFile, code)
	HBF		*hbfFile;
	HBF_CHAR	code;
{
reg	HBF_STRUCT	*hbf;

	hbf = (HBF_STRUCT *)hbfFile;
	if (hbf->bitmap_buffer == NULL &&
	    (hbf->bitmap_buffer = (unsigned char *)malloc(HBF_BitmapSize(hbfFile))) == NULL)
		return NULL;
	return HBF_GetBitmap(hbfFile, code, hbf->bitmap_buffer) == 0 ?
			hbf->bitmap_buffer : NULL;
}

int
HBF_GetBitmap(hbfFile, code, buffer)
	HBF		*hbfFile;
	HBF_HzCode	code;
	unsigned char	*buffer;
{
reg	HBF_STRUCT	*hbf;
	int	pos;
	int	b2pos;
reg	CODE_RANGE *cp;
reg	BM_FILE	*bmf;
	int	bm_size;
	int	retval;

	hbf = (HBF_STRUCT *)hbfFile;
	if ((b2pos = b2_pos(hbf, code)) < 0)
		return -1;
	pos = hbf->b2_size*FirstByte(code) + b2pos;
	for (cp = hbf->code_range; cp != NULL; cp = cp->code_next)
		if (cp->code_start <= code && code <= cp->code_finish) {
			bm_size = cp->code_sideways ?
				(hbfBitmapBBox(hbfFile)->hbf_height + 7)/8 *
					hbfBitmapBBox(hbfFile)->hbf_width :
				HBF_BitmapSize(hbfFile);
			bmf = cp->code_bm_file;
			if (fseek(bmf->bmf_file,
					cp->code_offset +
					(long)(pos - cp->code_pos) * bm_size,
					0) != 0) {
				eprintf("seek error on code 0x%04x", code);
				return -1;
			}
			retval = (cp->code_sideways ?
				get_transposed(hbfFile, bmf->bmf_file,
					(char *)buffer) :
				fread((char *)buffer,
					bm_size, 1, bmf->bmf_file) == 1
			       ) ? 0 : -1;
			if (retval < 0)
				eprintf("read error on code 0x%04x", code);
			return retval;
		}
	eprintf("code 0x%04x out of range", code);
	return -1;
}

/*
 * Call function on each valid code in ascending order.
 */
void
hbfForEach(hbfFile, func)
	HBF	*hbfFile;
	void	(*func)(
#ifdef __STDC__
			HBF	*sameHbfFile,
			HBF_CHAR code
#endif
		);
{
	HBF_STRUCT	*hbf;
	unsigned	byte1, byte2;
	unsigned	finish;
	CODE_RANGE	*cp;
	B2_RANGE	*b2r;

	hbf = (HBF_STRUCT *)hbfFile;
	for (cp = hbf->code_range; cp != NULL; cp = cp->code_next) {
		byte1 = FirstByte(cp->code_start);
		byte2 = SecondByte(cp->code_start);
		while (MakeCode(byte1, byte2) <= cp->code_finish) {
			for (b2r = hbf->byte_2_range;
			     b2r != NULL;
			     b2r = b2r->b2r_next) {
				if (byte2 < b2r->b2r_start)
					byte2 = b2r->b2r_start;
				finish = b2r->b2r_finish;
				if (byte1 == FirstByte(cp->code_finish) &&
				    finish > SecondByte(cp->code_finish))
					finish = SecondByte(cp->code_finish);
				while (byte2 <= finish) {
					(*func)(hbfFile,
						MakeCode(byte1, byte2));
					byte2++;
				}
			}
			byte1++;
			byte2 = 0;
		}
	}
}

const char *
hbfFileName(hbf)
	HBF	*hbf;
{
	return ((HBF_STRUCT *)hbf)->filename;
}

/*
 *	Functions also implemented as macros
 */

#ifdef hbfBitmapBBox
#undef hbfBitmapBBox
#endif

HBF_BBOX *
hbfBitmapBBox(hbf)
	HBF	*hbf;
{
	return &(hbf->hbf_bitmap_bbox);
}

#ifdef hbfFontBBox
#undef hbfFontBBox
#endif

HBF_BBOX *
hbfFontBBox(hbf)
	HBF	*hbf;
{
	return &(hbf->hbf_font_bbox);
}
