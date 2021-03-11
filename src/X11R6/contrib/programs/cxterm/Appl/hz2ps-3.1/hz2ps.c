/* File: hz2ps.c */

static char version[] = "hz2ps 3.1 (November 9, 1993)";

/***************************************************************************

  	hz2ps 3.1: convert a hanzi (GB/BIG5) File to PostScript
	-------------------------------------------------------

	By Fung F. Lee  (~{@n7c7e~})

	Copyright (C) 1989-1993  Fung F. Lee

	All rights reserved.
	Permission to copy and distribute verbatim copies of this
	document for non-commercial purposes is granted, but changing
	it is not allowed.
	There is absolutely no warranty for this program.

	Please send bug reports to the author:
	Fung F. Lee	lee@csl.stanford.edu


***************************************************************************/
  
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hbf.h"
#include "hz2ps.h"


char	*prog_name;
char	*input_file_name = NULL;
float	left_margin = 72;	/* 1 in from left edge */
float	right_margin = 540;	/* 7.5 in from left edge */
float	bottom_margin = 72;	/* 1 in from bottom edge */
float	top_margin = 720;	/* 10 in from bottom edge */
float	h_pos;			/* horizontal (x) position */
float	v_pos;			/* vertical (y) position */
UINT	hz_width;		/* width of an hanzi bitmap */
UINT	hz_height;		/* height of an hanzi bitmap */
UINT	hz_bm;			/* alias of hz_width */
UINT	hz_byte;		/* number of bytes per hanzi bitmap */
float	hz_point = 10;		/* point size of hanzi */
float	hz_spacing = 1;		/* horizontal inter-hanzi spacing */
float	hz_skip = 15;		/* inter-line spacing */
char	*e_font_name = DEFAULT_E_FONT;
float	e_point = 10;		/* point size of ASCII characters */
float	e_skip = 6;		/* inter-English character spacing */
float	e_descender = 2;	/* ASCII character descender */
float	e_vskip;
float	v_skip = 15;
float	h_skip = 15;
int	hanzi_per_tab = 4;
float	hz_tab;
float	pn_left = 300;
float	pn_bottom = 36;
int	page_no;
int	pn_offset = 0;
int	print_page_no = TRUE;
int	first_page = 1;
int	last_page = 9999;
int	print_page = TRUE;

CODE_SCHEME	encoding = GB;
PRINT_MODE	pmode = HH;

#ifdef __STDC__
void fput_newline(FILE *f);
void v_fput_newline(FILE *f);
#else
void fput_newline();
void v_fput_newline();
#endif


#ifdef __STDC__
void usage(void)
#else
void usage()
#endif
{
    FILE *f = stderr;

    fprintf(f, "%s\n", version);
    fprintf(f, "Copyright 1989-1993  Fung F. Lee\n\n");
    fprintf(f, "usage:\t%s ", prog_name);
    fprintf(f, "[-gb] [-big] [-h] [-v] \\\n");
    fprintf(f, "\t[-margin <left> <right> <bottom> <top>] \\\n" );
    fprintf(f, "\t[-ls <line-spacing>] [-htab <n>] \\\n");
    fprintf(f, "\t[-hf <hanzi-font> <point-size> <inter-hanzi-spacing>] \\\n");
    fprintf(f, "\t[-af <ascii-font> <point-size> <width> <descender>] \\\n");
    fprintf(f, "\t[-pf <page-number>] [-pl <page-number>] \\\n");
    fprintf(f, "\t[-pnp <left> <bottom>] [-pnoffset <number>] [-nopn] \\\n");
    fprintf(f, "\t[file]\n");
    exit(1);
}


#ifdef __STDC__
void fput_header(FILE *fin, FILE *f)
#else
void fput_header(fin, f)
FILE *fin, *f;
#endif
{
    int c;
    
    fprintf(f, "%%!PS-Adobe-3.0\n");
    fprintf(f, "%%%%Title: %s\n", input_file_name);
    fprintf(f, "%%%%Creator: (hz2ps 3.0)\n");
    fprintf(f, "%%%%Pages: (atend)\n");
    fprintf(f, "%%%%PageOrder: Ascend\n");
    fprintf(f, "%%%%Orientation: Portrait\n");
    fprintf(f, "%%%%BoundingBox: %d %d %d %d\n", (int)left_margin,
	    (int)bottom_margin, (int)right_margin, (int)top_margin);
    fprintf(f, "%%%%DocumentData: Clean7Bit\n");
    fprintf(f, "%%%%LanguageLevel: 1\n");
    fprintf(f, "%%%%EndComments\n");

    fprintf(f, "%%%%BeginProlog\n");
    while ((c = fgetc(fin)) != EOF) putc(c, f);
    fprintf(f, "%%%%EndProlog\n");

    fprintf(f, "/HZbm %d def\n", hz_bm);
    fprintf(f, "/HZpoint %6.2f def\n", hz_point);
    fprintf(f, "/HZspacing %6.2f def\n", hz_spacing);
    fprintf(f, "/HZskip %6.2f def\n", hz_skip);
    fprintf(f, "/Epoint %6.2f def\n", e_point);
    fprintf(f, "/Evskip %6.2f def\n", e_vskip);
    fprintf(f, "/Edescender %6.2f def\n", e_descender);
    fprintf(f, "/%s Epoint chooseFont\n", e_font_name);
    fprintf(f, "/pnLeft %6.2f def\n", pn_left);
    fprintf(f, "/pnBottom %6.2f def\n", pn_bottom);
    fprintf(f, "%%%%\n");
}



#ifdef __STDC__
void fput_newpage(FILE *f)
#else
void fput_newpage(f)
FILE *f;
#endif
{
    if (print_page)
    {
	if (print_page_no) fprintf(f, "(%d) prPageNo\n", page_no + pn_offset);
	fprintf(f, "endpage\n");
    }
    page_no++;
    print_page = (first_page <= page_no) && (page_no <= last_page);
    if (print_page)
    {
	fprintf(f, "%%%%Page: page%d %d\n", page_no, page_no - first_page + 1);
	fprintf(f, "beginpage\n");
    }
    h_pos = left_margin;
    v_pos = top_margin;
    fput_newline(f);
}



#ifdef __STDC__
void fput_newline(FILE *f)
#else
void fput_newline(f)
FILE *f;
#endif
{
    h_pos = left_margin;
    v_pos -= v_skip;
    if (v_pos < bottom_margin) fput_newpage(f);
    else if (print_page) fprintf(f, "%6.2f %6.2f moveto\n", h_pos, v_pos);
}



#ifdef __STDC__
void fput_hanzi(UCHAR *image, FILE *f)
#else
void fput_hanzi(image, f)
UCHAR *image; FILE *f;
#endif
{
    UCHAR	c;
    int		i;
    
    if (h_pos + hz_skip > right_margin) fput_newline(f);
    if (print_page)
    {
	fprintf(f, "<");
	for (i= 0; i < hz_byte; i++, image++)
	{
	    c = ~(*image);
	    fprintf(f, "%02x", c);
	}
	fprintf(f, "> HZshow\n");
    }
    h_pos += hz_skip;
}



#ifdef __STDC__
void fput_word(char *s, FILE *f)
#else
void fput_word(s, f)
char *s; FILE *f;
#endif
{
    if (h_pos + strlen(s) * e_skip > right_margin) fput_newline(f);
    if (print_page) fprintf(f, "(%s) Eshow\n", s);
    h_pos += strlen(s) * e_skip;
}



#ifdef __STDC__
void fput_dash(float scale, FILE *f)
#else
void fput_dash(scale, f)
float scale; FILE *f;
#endif
{
    if (h_pos + hz_skip > right_margin) fput_newline(f);
    if (print_page) fprintf(f, "%3f dash\n", scale);
    h_pos += hz_skip;
}



#ifdef __STDC__
void fput_tab(FILE *f)
#else
void fput_tab(f)
FILE *f;
#endif
{
    h_pos = ((h_pos-left_margin)/hz_tab + 1) * hz_tab + left_margin;
    if (h_pos > right_margin) fput_newline(f);
    else if (print_page) fprintf(f, "%6.2f %6.2f moveto\n", h_pos, v_pos);
}



#ifdef __STDC__
void fput_ascii(char c, FILE *f)
#else
void fput_ascii(c, f)
char c; FILE *f;
#endif
{
    if (c=='\n')
    {
	fput_newline(f);
	return;
    }
    if (c=='\f')
    {
	fput_newpage(f);
	return;
    }
    if (c=='\t')
    {
	fput_tab(f);
	return;
    }
    if (h_pos + e_skip > right_margin) fput_newline(f);
    if (c==' ')
    {
	if (print_page) fprintf(f, "( ) Eshow\n");
	h_pos += e_skip;
    }
    else if (c=='(' ||  c==')')
    {
	if (print_page) fprintf(f,  "(\\%c) Eshow\n", c);
	h_pos += e_skip;
    }
    else if (c=='\\')
    {
	if (print_page) fprintf(f,  "(\\\\) Eshow\n");
	h_pos += e_skip;
    }
    else if (isgraph(c))
    {
	if (print_page) fprintf(f, "(%c) Eshow\n", c);
	h_pos += e_skip;
    }
    /* ignore other non-graphical (unprintable) ascii characters */	
}



#ifdef __STDC__
void filter(FILE *f_in, FILE *f_out, HBF *hbf)
#else
void filter(f_in, f_out, hbf)
FILE *f_in, *f_out; HBF *hbf;
#endif
{
    int c1, c2, i;
    unsigned int code, default_code;
    unsigned char *image;
    char s[MAX_S_LEN];
    BOOL ok, special = FALSE;
    

    if ((image = (unsigned char *) calloc(hz_byte, 1)) == NULL)
    {
	fprintf(stderr, "Fatal error: can't allocate enough memory\n");
	exit(1);
    }
    page_no = 1;
    print_page = (first_page <= page_no) && (page_no <= last_page);
    if (print_page)
    {
	fprintf(f_out, "%%%%Page: page%d %d\n", page_no, page_no - first_page + 1);
	fprintf(f_out, "beginpage\n");
	fput_newline(f_out);
    }
    while ((c1 = fgetc(f_in)) != EOF && page_no <= last_page)
    {
	if (c1 & 0x80)
	{
	    c2 = fgetc(f_in);
	    code = TO_CODE(c1, c2);
	    if (encoding == GB)
	    {
		default_code = GB_SQUARE;
		special = TRUE;
		switch (code)
		{
		case 0xA1AA: fput_dash(0.5, f_out); break;
		case 0xA3DF: fput_dash(0.0, f_out); break;
		case 0xA3FE: fput_dash(1.0, f_out); break;
		default:     special = FALSE; break;
		}
	    }
	    else if (encoding == BIG5)
	    {
		default_code = BIG_SQUARE;
		special = TRUE;
		switch (code)
		{
		case 0xA277:
		case 0xA299:
		case 0xA156:
		case 0xA158: fput_dash(0.5, f_out); break;
		default:     special = FALSE; break;
		}
	    }
	    if (special) continue;	/* already taken care of */
	    ok = (HBF_GetBitmap(hbf, code, image) == 0);
	    if (!ok)
	    {
		ok = (HBF_GetBitmap(hbf, default_code, image) == 0);
	    }
	    if (ok) fput_hanzi(image, f_out);
	}
	else if (isalnum(c1))
	{
	    s[0] = c1;
	    i = 1;
	    while (i < MAX_S_LEN - 1 && ((c2 = fgetc(f_in)) != EOF)
		   && isalnum(c2))
	    {
		s[i++] = c2;
	    }
	    s[i] = '\0';
	    if (i < MAX_S_LEN - 1) ungetc(c2, f_in);
	    fput_word(s, f_out);
	}
	else
	{
	    fput_ascii(c1, f_out);
	}
    }
    if (print_page)
    {
	if (print_page_no)
	    fprintf(f_out, "(%d) prPageNo\n", page_no + pn_offset);
	fprintf(f_out, "endpage\n");
    }
    fprintf(f_out, "%%%%Trailer\n");
    fprintf(f_out, "%%%%Pages: %d\n", MIN(last_page, page_no) - first_page + 1);
    fprintf(f_out, "%%%%EOF\n");
}


/*  vertical printing functions */


#ifdef __STDC__
void v_fput_newpage(FILE *f)
#else
void v_fput_newpage(f)
FILE *f;
#endif
{
    if (print_page)
    {
	if (print_page_no) fprintf(f, "(%d) prPageNo\n", page_no + pn_offset);
	fprintf(f, "endpage\n");
    }
    page_no++;
    print_page = (first_page <= page_no) && (page_no <= last_page);
    if (print_page)
    {
	fprintf(f, "%%%%Page: page%d %d\n", page_no, page_no - first_page + 1);
	fprintf(f, "beginpage\n");
    }
    h_pos = right_margin;
    v_pos = top_margin;
    v_fput_newline(f);
}



#ifdef __STDC__
void v_fput_newline(FILE *f)
#else
void v_fput_newline(f)
FILE *f;
#endif
{
    v_pos = top_margin - hz_skip;
    h_pos -= h_skip;
    if (h_pos < left_margin) v_fput_newpage(f);
    else if (print_page) fprintf(f, "%6.2f %6.2f moveto\n", h_pos, v_pos);
}



#ifdef __STDC__
void v_fput_hanzi(UCHAR *image, FILE *f, char vmode)
#else
void v_fput_hanzi(image, f, vmode)
UCHAR *image; FILE *f; char vmode;
#endif
{
    UCHAR	c;
    int		i;
    
    if (v_pos < bottom_margin) v_fput_newline(f);
    if (print_page)
    {
	fprintf(f, "<");
	for (i=0; i<hz_byte; i++, image++)
	{
	    c = ~(*image);
	    fprintf(f, "%02x", c);
	}
	switch (vmode)
	{
	case 'R': fprintf(f, "> HZvshowR\n"); break;
	case 'Q': fprintf(f, "> HZvshowQ\n"); break;
	default:  fprintf(f, "> HZvshow\n"); break;
	}
    }
    v_pos -= hz_skip;
}



#ifdef __STDC__
void v_fput_word(char *s, FILE *f)
#else
void v_fput_word(s, f)
char *s; FILE *f;
#endif
{
    if (v_pos + hz_skip - strlen(s) * e_vskip < bottom_margin) 
	v_fput_newline(f);
    if (print_page) fprintf(f, "(%s) Evshow\n", s);
    v_pos -= strlen(s) * e_vskip;
}



#ifdef __STDC__
void v_fput_dash(float scale, FILE *f)
#else
void v_fput_dash(scale, f)
float scale; FILE *f;
#endif
{
    if (v_pos < bottom_margin) v_fput_newline(f);
    if (print_page) fprintf(f, "%3f vdash\n", scale);
    v_pos -= hz_skip;
}



#ifdef __STDC__
void v_fput_tab(FILE *f)
#else
void v_fput_tab(f)
FILE *f;
#endif
{
    v_pos = top_margin - ((top_margin - v_pos-hz_skip) / hz_tab + 1) * hz_tab
	- hz_skip;
    if (v_pos < bottom_margin) v_fput_newline(f);
    else if (print_page) fprintf(f, "%6.2f %6.2f moveto\n", h_pos, v_pos);
}



#ifdef __STDC__
void v_fput_ascii(char c, FILE *f)
#else
void v_fput_ascii(c, f)
char c; FILE *f;
#endif
{
    if (c=='-')	c = '|';
    else if (c=='|') c = '-';

    if (c=='\n')
    {
	v_fput_newline(f);
	return;
    }
    if (c=='\f')
    {
	v_fput_newpage(f);
	return;
    }
    if (c=='\t')
    {
	v_fput_tab(f);
	return;
    }
    if (v_pos + hz_skip - e_point - e_vskip < bottom_margin) v_fput_newline(f);
    if (c==' ')
    {
	if (print_page) fprintf(f, "( ) Evshow\n");
    }
    else if (c=='(' ||  c==')')
    {
	if (print_page) fprintf(f,  "(\\%c) Evshow\n", c);
	v_pos -= e_point;
    }
    else if (c=='\\')
    {
	if (print_page) fprintf(f,  "(\\\\) Evshow\n");
	v_pos -= e_point;
    }
    else if (isgraph(c))
    {
	if (print_page) fprintf(f, "(%c) Evshow\n", c);
	v_pos -= e_point;
    }
    /* ignore the unprintable ascii characters */	
}



#ifdef __STDC__
char v_test_gb(UINT code)
#else
char v_test_gb(code)
UINT code;
#endif
{
    char mode;
    
    switch (code)
    {
    case 0xA1AA:	/* middle-dash */
    case 0xA1AB:	/*  ~  */
    case 0xA1AD:	/* ... */
    case 0xA1B2:
    case 0xA1B3:
    case 0xA1B4:
    case 0xA1B5:
    case 0xA1B6:
    case 0xA1B7:
    case 0xA1B8:
    case 0xA1B9:
    case 0xA1BA:
    case 0xA1BB:
    case 0xA1BC:
    case 0xA1BD:
    case 0xA1BE:
    case 0xA1BF:
    case 0xA1FA:	/* --> */
    case 0xA1FB:	/* <-- */
    case 0xA1FC:	/* up-arrow */
    case 0xA1FD:	/* down-arrow */
    case 0xA3A8:	/* ( */
    case 0xA3A9:	/* ) */
    case 0xA3AD:	/* - */
    case 0xA3DB:	/* [ */
    case 0xA3DD:	/* ] */
    case 0xA3DF:	/* lower-dash */
    case 0xA3FB:	/* { */
    case 0xA3FC:	/* | */
    case 0xA3FD:	/* } */
    case 0xA3FE:	/* upper-dash */
	mode = 'R'; break;
    case 0xA1A2:	/* dun-hao */
    case 0xA1A3:	/* period */
    case 0xA3A1:	/* ! */
    case 0xA3AC:	/* , */
    case 0xA3AE:	/* . */
    case 0xA3BA:	/* : */
    case 0xA3BB:	/* ; */
    case 0xA3BF:	/* ? */
	mode = 'Q'; break;
    default:
	mode = 'V'; break;
    }
    return mode;
}



#ifdef __STDC__
UINT v_replace_gb(UINT code)
#else
UINT v_replace_gb(code)
UINT code;
#endif
{
    unsigned int out;
    switch (code)
    {
    case 0xA1AE: out = 0xA1B8; break;
    case 0xA1AF: out = 0xA1B9; break;
    case 0xA1B0: out = 0xA1BA; break;
    case 0xA1B1: out = 0xA1BB; break;
    default: out = code; break;
    }
    return out;
}



#ifdef __STDC__
char v_test_big(UINT code)
#else
char v_test_big(code)
UINT code;
#endif
{
    char mode;
    
    switch (code)
    {
    case 0xA156:	/* middle-dash */
    case 0xA158:
    case 0xA14B:
    case 0xA14C:
    case 0xA155:
    case 0xA157:
    case 0xA159:
    case 0xA15B:
    case 0xA15C:
    case 0xA1E3:
	mode = 'R'; break;
    case 0xA142:	/* dun-hao */
    case 0xA14E:
    case 0xA143:	/* period */
    case 0xA144:
    case 0xA145:
    case 0xA14F:
    case 0xA150:
    case 0xA141:	/* , */
    case 0xA14D:
	mode = 'Q'; break;
    default:
	mode = 'V'; break;
    }
    if (code >= 0xA15B && code <= 0xA1A4)
	mode = 'R';
    else if (code >= 0xA1F4 && code <= 0xA1FB)
	mode = 'R';
    return mode;
}



#ifdef __STDC__
UINT v_replace_big(UINT code)
#else
UINT v_replace_big(code)
UINT code;
#endif
{
    unsigned int out;

    switch (code)
    {
    case 0xA1A5: out = 0xA175; break;
    case 0xA1A6: out = 0xA176; break;
    case 0xA1A7: out = 0xA179; break;
    case 0xA1A8: out = 0xA17A; break;
    case 0xA1A9: out = 0xA179; break;
    case 0xA1AA: out = 0xA17A; break;
    default: out = code; break;
    }
    return out;
}



#ifdef __STDC__
void v_filter(FILE *f_in, FILE *f_out, HBF *hbf)
#else
void v_filter(f_in, f_out, hbf)
FILE *f_in, *f_out; HBF *hbf;
#endif
{
    int c1, c2, i;
    unsigned int code, default_code;
    unsigned char *image;
    char s[MAX_S_LEN];
    BOOL ok, special = FALSE;
    int vmode = FALSE;
    
    if ((image = (unsigned char *) calloc(hz_byte, 1)) == NULL)
    {
	fprintf(stderr, "Fatal error: can't allocate enough memory\n");
	exit(1);
    }
    page_no = 1;
    print_page = (first_page <= page_no) && (page_no <= last_page);
    if (print_page)
    {
	fprintf(f_out, "%%%%Page: page%d %d\n", page_no, page_no - first_page + 1);
	fprintf(f_out, "beginpage\n");
	v_fput_newline(f_out);
    }
    while ((c1=fgetc(f_in)) != EOF && page_no <= last_page)
    {
	if (c1 & 0x80)
	{
	    c2 = fgetc(f_in);
	    code = TO_CODE(c1, c2);
	    if (encoding == GB)
	    {
		default_code = GB_SQUARE;
		special = TRUE;
		switch (code)
		{
		case 0xA1AA: v_fput_dash(0.5, f_out); break;
		case 0xA3DF: v_fput_dash(0.0, f_out); break;
		case 0xA3FE: v_fput_dash(1.0, f_out); break;
		default:
		    code = v_replace_gb(code);
		    vmode = v_test_gb(code);
		    special = FALSE;
		    break;
		}
	    }
	    else if (encoding == BIG5)
	    {
		default_code = BIG_SQUARE;
		special = TRUE;
		switch (code)
		{
		case 0xA277:
		case 0xA299:
		case 0xA156:
		case 0xA158: v_fput_dash(0.5, f_out); break;
		case 0xA15A: v_fput_dash(0.0, f_out); break;
		default:
		    code = v_replace_big(code);
		    vmode = v_test_big(code);
		    special = FALSE;
		    break;
		}
	    }
	    if (special) continue;	/* already taken care of */
	    ok = (HBF_GetBitmap(hbf, code, image) == 0);
	    if (!ok)
	    {
		ok = (HBF_GetBitmap(hbf, default_code, image) == 0);
	    }
	    if (ok) v_fput_hanzi(image, f_out, vmode);
	}
	else if (isalnum(c1))
	{
	    s[0] = c1;
	    i = 1;
	    while (i < MAX_S_LEN - 1 && ((c2 = fgetc(f_in)) != EOF)
		   && isalnum(c2))
	    {
		s[i++] = c2;
	    }
	    s[i] = '\0';
	    if (i < MAX_S_LEN - 1) ungetc(c2, f_in);
	    v_fput_word(s, f_out);
	}
	else
	{
	    v_fput_ascii(c1, f_out);
	}
    }
    if (print_page)
    {
	if (print_page_no)
	    fprintf(f_out, "(%d) prPageNo\n", page_no + pn_offset);
	fprintf(f_out, "endpage\n");
    }
    fprintf(f_out, "%%%%Trailer\n");
    fprintf(f_out, "%%%%Pages: %d\n", MIN(last_page, page_no) - first_page + 1);
    fprintf(f_out, "%%%%EOF\n");
}



#ifdef __STDC__
int main(int argc, char *argv[])
#else
int main(argc, argv)
int argc; char *argv[];
#endif
{
    int		i;
    char	*font_dir;
    char	*font_name = DEFAULT_HZ_FONT;
    char	font_full_name[MAX_FONT_NAME];
    char	header_full_name[MAX_FONT_NAME];
    FILE	*f_in;
    HBF		*hbf;
    int		x_disp, y_disp;
    FILE	*header;
    float	ftmp;

    hbfDebug = TRUE;

    *font_full_name = '\0';
    *header_full_name = '\0';
    prog_name = argv[0];
#ifdef USE_OLD
    if ((font_dir = getenv(FONT_DIR)) != NULL)
    {
	strcpy(font_full_name, font_dir);
	strcat(font_full_name, PATH_SEPARATOR);
	strcpy(header_full_name, font_dir);
	strcat(header_full_name, PATH_SEPARATOR);
    }
    /* else assume the current directory as the FONT_DIR */
#endif
    if ((font_dir = getenv(HZLIB)) != NULL)
    {
	strcpy(header_full_name, font_dir);
	strcat(header_full_name, PATH_SEPARATOR);
    }
    /* else assume the current directory as the FONT_DIR */
    strcat(header_full_name, PROLOG);
    
    for (i = 1; i < argc; i++)
    {
	if (strcmp("-gb", argv[i]) == 0)
	    encoding = GB;
	else if (strcmp("-big", argv[i]) == 0)
	    encoding = BIG5;
	else if (strcmp("-h", argv[i]) == 0)
	    pmode = HH;
	else if (strcmp("-v", argv[i]) == 0)
	    pmode = VV;
	else if (strcmp("-margin", argv[i]) == 0)
	{
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &ftmp) != 1) usage();
	    left_margin = ftmp * PT_PER_IN;
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &ftmp) != 1) usage();
	    right_margin = ftmp * PT_PER_IN;
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &ftmp) != 1) usage();
	    bottom_margin = ftmp * PT_PER_IN;
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &ftmp) != 1) usage();
	    top_margin = ftmp * PT_PER_IN;
	}
	else if (strcmp("-ls", argv[i]) == 0)
	{
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &v_skip) != 1) usage();
	    h_skip = v_skip;
	}
	else if (strcmp("-htab", argv[i]) == 0)
	{
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%d", &hanzi_per_tab) != 1) usage();
	}
	else if (strcmp("-hf", argv[i]) == 0)
	{
	    if (++i >= argc) usage();
	    font_name = argv[i];
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &hz_point) != 1) usage();
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &hz_spacing) != 1) usage();
	}
	else if (strcmp("-af", argv[i]) == 0)
	{
	    if (++i >= argc) usage();
	    e_font_name = argv[i];
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &e_point) != 1) usage();
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &e_skip) != 1) usage();
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &e_descender) != 1) usage();
	}
	else if (strcmp("-pf", argv[i]) == 0)
	{
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%d", &first_page) != 1) usage();
	}
	else if (strcmp("-pl", argv[i]) == 0)
	{
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%d", &last_page) != 1) usage();
	}
	else if (strcmp("-pnp", argv[i]) == 0)
	{
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &ftmp) != 1) usage();
	    pn_left = ftmp * PT_PER_IN;
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%f", &ftmp) != 1) usage();
	    pn_bottom = ftmp * PT_PER_IN;
	}
	else if (strcmp("-pnoffset", argv[i]) == 0)
	{
	    if (++i >= argc) usage();
	    if (sscanf(argv[i], "%d", &pn_offset) != 1) usage();
	}
	else if (strcmp("-nopn", argv[i]) == 0)
	    print_page_no = FALSE;
	else if (argv[i][0] == '-')
	    usage();
	else
	    input_file_name = argv[i];
    }

    strcat(font_full_name, font_name);
#ifdef USE_OLD
    strcat(font_full_name, FONT_EXT);
#endif
    if (HBF_OpenFont(font_full_name, &hbf) != 0)
    {
#ifdef USE_OLD
	fprintf(stderr, "%s: can't open font '%s'\n",
		prog_name, font_full_name);
#endif
	exit(1);
    }
    HBF_GetBitmapBoundingBox(hbf, &hz_width, &hz_height, &x_disp, &y_disp);
    if (hz_width != hz_height)
    {
	fprintf(stderr, "Can't handle non-square bitmap yet.\n");
	exit(1);
    }
    hz_byte = (hz_width + 7) / 8 * hz_height;
    hz_bm = hz_width;
    hz_skip = hz_point + hz_spacing;
    e_vskip = e_point * 1.2;
    hz_tab = hanzi_per_tab * hz_skip;
    
    if (left_margin + MAX(hz_skip, h_skip) > right_margin)
    {
	fprintf(stderr,
		"Error: insufficient space between left and right margins.\n");
	exit(1);
    }
    if (bottom_margin + MAX(hz_skip, v_skip) > top_margin)
    {
	fprintf(stderr,
		"Error: insufficient space between top and bottom margins.\n");
	exit(1);
    }
    if (last_page < first_page)
    {
	fprintf(stderr,
		"Last page specified should be no smaller than first page.\n");
	exit(1);
    }

    if ((header = fopen(header_full_name, "r")) == NULL)
    {
	fprintf(stderr, "Cannot open prolog file %s\n", header_full_name);
	exit(1);
    }
    if (input_file_name == NULL) f_in = stdin;
    else if ((f_in = fopen(input_file_name, "r")) == NULL)
    {
	fprintf(stderr, "Cannot open input file %s\n", input_file_name);
	exit(1);
    }
    fput_header(header, stdout);
    fclose(header);
    if (pmode == HH) 
    {     
	h_pos = left_margin;
	v_pos = top_margin;
	filter(f_in, stdout, hbf);
    }  
    else if (pmode == VV)
    {
	h_pos = right_margin;
	v_pos = top_margin;
	v_filter(f_in, stdout, hbf);
    }
    else
    {
	fprintf(stderr, "Error: unknown printing mode %c\n", pmode);
	usage();
    }
    if (f_in != stdin) fclose(f_in);
    HBF_CloseFont(hbf);
    return 0;
}
