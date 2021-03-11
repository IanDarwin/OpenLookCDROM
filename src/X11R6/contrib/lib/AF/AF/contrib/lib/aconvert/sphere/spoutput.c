/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* LINTLIBRARY */

/** File: spoutput.c **/

#include <stdio.h>
#include <ctype.h>
#include "header.h"
#include "sysparam.h"
#include "sysfunc.h"
#include "sp.h"
#include "version.h"

#define NAMEWIDTH	30
/** Field names are printed to this width, left-justified, in */
/** sp_print_lines().                                         */


/******************************************************************/
/* Prints the specified header to stream fp in the standard       */
/* format that will allow "sp_open_header() to read it back       */
/* in later.                                                      */
/* More specifically:                                             */
/* writes "header_end" after the last field, then adds some       */
/* newlines (so pagers can be used to look at the file a          */
/* screenful at a time without seeing the samples themselves),    */
/* and then pads the header to a multiple of PAD_MULT bytes.      */
/* That constant is #define'd in sysparam.h, currently at 1024.   */
/* Padding is accomplished via ftell(), so it won't work on       */
/* systems where ftell() doesn't return byte offsets.             */
/******************************************************************/

FUNCTION int sp_write_header(fp,h,hbytes,databytes)
register FILE *fp;
struct header_t *h;
long *hbytes, *databytes;
{
register int c;
long d=0, hpos=0, p, pbytes;
static struct fileheader_fixed fh;

if (fp == FPNULL)	return -1;	/* check sanity of arguments */
if (h == HDRNULL)	return -1;
if (hbytes == LNULL)	return -1;
if (databytes == LNULL)	return -1;

clearerr(fp);
if (fseek(fp,(long)sizeof(struct fileheader_fixed),0) == -1)
	return -1;
if (sp_format_lines(h,fp) < 0)
	return -1;
(void) fprintf(fp,"%s\n",ENDSTR);
d = ftell(fp);

for (c=0; c < PAD_NEWLINES; c++)	/* pad with newlines always */
	(void) putc('\n',fp);
pbytes = PAD_MULT - (ftell(fp) % PAD_MULT);
for (p=0; p < pbytes; p++)		/* pad to multiple of PAD_MULT */
	(void) putc(PAD_CHAR,fp);
hpos = ftell(fp);
if (hpos % PAD_MULT != 0)
	return -1;

/** now that we know how big the header is, seek to fixed part and write */
/** NIST label and header size in bytes as first two lines. */
if (fseek(fp,0L,0) == -1) return -1;
sprintf((char *) &fh,"%*s\n%*ld\n",
	sizeof(fh.header_id)-1,NISTLABEL,
	sizeof(fh.header_size)-1,hpos);
(void) fwrite((char *) &fh,sizeof(struct fileheader_fixed),1,fp);
(void) fflush(fp);

if (fseek(fp,hpos,0) == -1) return -1;
if (ferror(fp)) return -1;

*hbytes = hpos;		/* on success, return #bytes in header */
*databytes = d;		/* and #bytes before padding           */
return 0;
}

/*********************************************************************/
/* Dumps header fields to stream fp in a readable form:              */
/*        fieldnumber: fieldtype fieldname "field_value"             */
/*   where fieldtype is either 's', 'i' or 'r'.                      */
/* Field numbers start at 0.                                         */
/* Beware printing headers with unprintable characters in data       */
/*   fields using this function.                                     */
/*********************************************************************/

FUNCTION int sp_print_lines(h,fp)
register FILE *fp;
struct header_t *h;
{
register int i, fc;
register struct field_t **fv;
int len, j;
char *p;

if (h == HDRNULL)	return -1;	/* check sanity of arguments */
if (fp == FPNULL)	return -1;

clearerr(fp);
fv = h->fv;
fc = h->fc;
for (i=0; i < fc; i++) {
	len = strlen(fv[i]->name);
	if (len <= NAMEWIDTH)
		(void) fprintf(fp,"%5d: %c %-*s ",i,spx_tp(fv[i]->type),NAMEWIDTH,fv[i]->name);
	else
		(void) fprintf(fp,"%5d: %c %s ",i,spx_tp(fv[i]->type),fv[i]->name);
	for (j=0, p=fv[i]->data; j < fv[i]->datalen; j++, p++)
		if (isprint(*p) || (*p == '\n') || (*p == '\t'))
			(void) putc(*p,fp);
		else
			(void) fprintf(fp,"\0%o",*p);
	(void) fprintf(fp,"\n");
}
if (ferror(fp)) {
	clearerr(fp);
	return -1;
}
return 0;
}

/********************************************************************/
/* Write header fields to stream fp in NIST SPHERE format.          */
/* Format is basically: name -type value.                           */
/* See documentation for the complete header grammar.               */
/********************************************************************/

FUNCTION int sp_format_lines(h,fp)
struct header_t *h;
register FILE *fp;
{
int i, j, fc;
char *p;
register struct field_t **fv;

if (h == HDRNULL)	return -1;	/* check sanity of arguments */
if (fp == FPNULL)	return -1;

clearerr(fp);
fv = h->fv;
fc = h->fc;
for (i=0; i < fc; i++) {
	(void) fprintf(fp,"%s -%c",fv[i]->name,spx_tp(fv[i]->type));
	if (fv[i]->type == T_STRING)
		(void) fprintf(fp,"%d",strlen(fv[i]->data));
	(void) fprintf(fp," ");
	for (j=0, p=fv[i]->data; j < fv[i]->datalen; j++, p++)
		(void) putc(*p,fp);
	(void) fprintf(fp,"\n");
}
if (ferror(fp)) {
	clearerr(fp);
	return -1;
}
return 0;
}

/************************************************************/
/** Returns a character that represents the type associated */
/** with the integer parameter, '?' for bad type.           */
/************************************************************/

int spx_tp(ftype)
register int ftype;
{
register int result;

switch (ftype) {
	case T_INTEGER:
		result = 'i'; break;
	case T_REAL:
		result = 'r'; break;
	case T_STRING:
		result = 's'; break;
	default:
		result = '?'; break;
}

return result;
}

/**************************************************************/
/* Copies stream fp to stream outfp through EOF.              */
/* Returns -1 if an error occurs (either reading or writing). */
/**************************************************************/

FUNCTION int sp_fpcopy(fp,outfp)
register FILE *fp, *outfp;
{
register int c;

while ((c = getc(fp)) != EOF)
	if (putc(c,outfp) == EOF)
		return -1;
return (ferror(fp) || ferror(outfp)) ? -1 : 0;
}
