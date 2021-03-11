/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* LINTLIBRARY */

/** File: spinput.c **/

#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include <sys/types.h>
#include "header.h"
#include "sysparam.h"
#include "sysfunc.h"
#include "sp.h"
#include "version.h"

int farray_fields;
struct field_t *farray[MAXFIELDS];
char *header = CNULL;

static int parse_header();
static char *parse_line();

/*******************************************************************/
/* Reads a NIST header from file pointer "fp" into a buffer, then  */
/*    calls parse_header() to parse the buffer if "parse_flag" is  */
/*    set.                                                         */
/* On failure, "*error" is set to a string describing the error.   */
/*******************************************************************/

int spx_read_header(fp,header_size,parse_flag,error)
FILE *fp;
char **error;
int *header_size, parse_flag;
{
char *p;
static struct fileheader_fixed fh;
int hsize, hfields;

if (fp == FPNULL) {
	*error = "File pointer is null";
	goto errexit;
}
if (ftell(fp) != 0L) {
	*error = "File pointer not at beginning of file";
	goto errexit;
}
if (fread((char *) &fh, sizeof fh, 1, fp) != 1) {
	*error = "Fread for fixed part of header failed";
	goto errexit;
}
if (fh.header_id[sizeof(fh.header_id) - 1] != '\n') {
	*error = "Bad header label line";
	goto errexit;
}
if (strncmp(fh.header_id,NISTLABEL,sizeof(NISTLABEL)-1) != 0) {
	*error = "Bad header label";
	goto errexit;
}
if (fh.header_size[sizeof(fh.header_size) - 1] != '\n') {
	*error = "Bad header size line";
	goto errexit;
}
p = fh.header_size;
while ((p < &fh.header_size[sizeof(fh.header_size)-1]) && (*p == ' '))
	p++;
if (! isdigit(*p)) {
	*error = "Bad header size specifier";
	goto errexit;
}
hsize = atoi(p);
if (hsize < sizeof fh) {
	*error = "Specified header size is too small";
	goto errexit;
}
header = spx_malloc((u_int) (hsize - sizeof fh + 1));
if (header == CNULL) {
	*error = "Malloc for header failed";
	goto errexit;
}
if (fread(header,hsize-sizeof fh,1,fp) != 1) {
	*error = "Can't read entire header into memory";
	goto errexit;
}
if (parse_flag && (parse_header(header,hsize,&hfields,error) < 0))
	goto errexit;

spx_free(header);
header = CNULL;
if (header_size != INULL)
	*header_size = hsize;
return 0;

errexit:
	if (header != CNULL) {
		spx_free(header);
		header = CNULL;
	}
	return -1;
}

/************************************************************/
/* Parses the bytes read from a speech file and inserts the */
/*    fields into "farray".                                 */
/* If the parsing finishes with success, the fields should  */
/*    then be copied into a header structure for the user.  */
/************************************************************/

static int parse_header(p,hsize,fields,error)
register char *p;
char **error;
int hsize, *fields;
{
register char *lim = p + (hsize - sizeof(struct fileheader_fixed));
int i, remaining;

farray_fields = 0;
for (i = 0; i < MAXFIELDS; i++)
	farray[i] = FNULL;

*lim = '\0';		/* by setting last character in buffer to NULL,   */
*fields = 0;		/* index() can be used at any point in buffer w/o */
			/* accessing potentially-illegal addresses        */

while (p < lim) {
	remaining = lim - p;
	if (remaining < sizeof(ENDSTR)-1) {
		*error = "Bad header end";
		return -1;
	}
	if (*p == COMMENT_CHAR) {
		while ((p < lim) && (*p != '\n'))
			p++;
		if (p < lim) p++;
	} else if (isalpha(*p)) {
		register char *t, *v;

		if ((strncmp(p,ENDSTR,sizeof(ENDSTR)-1) == 0) &&
			((remaining == sizeof(ENDSTR)-1) ||
			(*(p+sizeof(ENDSTR)-1) == ' ') ||
			(*(p+sizeof(ENDSTR)-1) == '\n')))
				return 0;
		t = index(p,' ');
		if (t == CNULL) {
			*error = "space expected after field name";
			return -1;
		}
		v = index(t+1,' ');
		if (v == CNULL) {
			*error = "space expected after type specifier";
			return -1;
		}
		p = parse_line(p,t,v,error);
		if (p == CNULL)
			return -1;
		++*fields;
	} else {
		*error = "Bad character at beginning of line";
		return -1;
	}
}
return 0;
}

/*********************************************************************/
/* Parses a line from a speech file.                                 */
/* The arguments to parse_line() point into a line in the header     */
/*    buffer as follows:                                             */
/*                                                                   */
/*	field type value[;.....]\n                                   */
/* 	^    ^    ^                                                  */
/*	h    t    v                                                  */
/*********************************************************************/

static char *parse_line(h,t,v,error)
char *h, *t, *v, **error;
{
struct field_t *f;
int vtype, vlen;
char *endofvalue = v + 1, *endoffieldname = h, *ptr;

if (farray_fields >= MAXFIELDS) {
	*error = "too many fields";
	return CNULL;
}
*t = '\0';
while (isalnum(*endoffieldname) || (*endoffieldname == '_'))
	endoffieldname++;
if (endoffieldname != t) {
	*error = "space expected after field name";
	return CNULL;
}
if (*(t+1) != '-') {
	*error = "dash expected in type specifier";
	return CNULL;
}
switch (*(t+2)) {
	case 'i':
		vtype = T_INTEGER;
		while (isdigit(*endofvalue) || (*endofvalue == '-'))
			++endofvalue;
		vlen = endofvalue - (v + 1);
		break;
	case 'r':
		vtype = T_REAL;
		while (isdigit(*endofvalue) ||
			(*endofvalue == '.') ||
			(*endofvalue == '-'))
				++endofvalue;
		vlen = endofvalue - (v + 1);
		break;
	case 's':
		vtype = T_STRING;
		vlen = 0;
		ptr = t + 3;
		while (isdigit(*ptr))
			vlen = 10 * vlen + (*ptr++ - '0');
		if (! vlen) {
			*error = "bad string length";
			return CNULL;
		}
		if (ptr != v) {
			*error = "space expected after type specifier";
			return CNULL;
		}
		endofvalue = v + vlen + 1;
		break;
	default:
		*error = "unknown type specifier";
		return CNULL;
}

{
/* Really only need the function call, but by null-terminating the     */
/* string at (v+1), looking at a stack trace in "dbx" (a BSD Unix      */
/* debugger) is easier. Otherwise, "dbx" will expect a null-terminator */
/* and print the rest of the header block.                             */

int ch = *(v + 1 + vlen);

*(v + 1 + vlen) = '\0';
f = spx_allocate_field_str(vtype,h,v+1,vlen);
*(v + 1 + vlen) = ch;
}

if (f == FNULL) {
	*error = "Malloc for triple failed";
	return CNULL;
}
farray[farray_fields++] = f;

switch (*endofvalue) {
	case COMMENT_CHAR:
	case '\n':
		return endofvalue + 1;
	case ' ':
		while (*endofvalue == ' ')
			++endofvalue;
		if (*endofvalue == '\n')
			return endofvalue + 1;
		if (*endofvalue == COMMENT_CHAR) {
			char *eoln = index(endofvalue,'\n');
			if (eoln != CNULL)
				return eoln + 1;
		}
		*error = "bad character after triple and space(s)";
		return CNULL;
}
*error = "bad character after triple";
return CNULL;
}

/********************************************************/

struct header_t *spx_allocate_header(fc,fv)
int fc;
struct field_t **fv;
{
register struct header_t *h;

h = (struct header_t *) spx_malloc((u_int) sizeof(struct header_t));
if (h != HDRNULL) {
	h->fc = fc;
	h->fv = fv;
}
return h;
}

/**********************************************************/
/* Deallocates a header by freeing the structure that     */
/*    represents it.                                      */
/**********************************************************/

int spx_deallocate_header(h)
struct header_t *h;
{
if (h == HDRNULL)	return -1;	/* check sanity of arguments */

spx_free((char *) h);
return 0;
}

/**********************************************************/
/* Allocates room for a field with name "fieldname", type */
/*    "type" represented at address "v" and comprising    */
/*    "vlen" bytes.                                       */
/**********************************************************/

struct field_t *spx_allocate_field(type,fieldname,v,vlen)
int type, vlen;
char *fieldname, *v;
{
static char buffer[1024];
	/* plenty big enough for storing ascii form of numbers */

if (fieldname == CNULL) return FNULL;
if (v == CNULL) return FNULL;

switch (type) {
	case T_INTEGER:
		sprintf(buffer,"%ld",*(long *)v);
		break;
	case T_REAL:
		sprintf(buffer,"%f",*(double *)v);
		break;
	case T_STRING:
		if (vlen <= 0)
			return FNULL;
		return spx_allocate_field_str(type,fieldname,v,vlen);
		break;
	default:
		return FNULL;
}
return spx_allocate_field_str(type,fieldname,buffer,strlen(buffer));
}

/***************************************************************/

struct field_t *spx_allocate_field_str(type,fieldname,value,vlen)
int type, vlen;
char *fieldname, *value;
{
register struct field_t *f;
char *p1, *p2;

if (vlen <= 0)
	return FNULL;

f = (struct field_t *) spx_malloc((u_int) sizeof(struct field_t));
if (f == FNULL)
	return FNULL;

p1 = spx_malloc((u_int) (strlen(fieldname) + 1));
if (p1 == CNULL) {
	spx_free((char *) f);
	return FNULL;
}

p2 = spx_malloc((u_int) (vlen + 1));
if (p2 == CNULL) {
	spx_free((char *) f);
	spx_free(p1);
	return FNULL;
}

f->type = type;
f->name = p1;
f->data = p2;
f->datalen = vlen;
(void) strcpy(p1,fieldname);
(void) bcopy(value,p2,vlen);
p2[vlen] = '\0';

return f;
}

/*******************************************************************/
/* Deallocates a field by freeing bytes used to store the field    */
/*    name and value, then freeing bytes that were allocated for   */
/*    the structure.                                               */
/*******************************************************************/

int spx_deallocate_field(fv)
struct field_t *fv;
{
if (fv == FNULL)	return -1;	/* check sanity of arguments */

spx_free(fv->name);
spx_free(fv->data);
spx_free((char *) fv);
return 0;
}

/******************************************************************/
/* Returns a pointer to a vector of field structures for the      */
/*    specified number of elements.                               */
/******************************************************************/

struct field_t **spx_get_field_vector(elements)
int elements;
{
if (elements <= 0)
	return FVNULL;
return (struct field_t **)
	spx_malloc((u_int) (elements * sizeof(struct field_t *)));
}

/**********************************************************/
/* Copies field vector "src_fv" to field vector "dst_fv". */
/* The number of fields in the source vector must be      */
/*    specified by "elements", a positive number.         */
/**********************************************************/

int spx_copy_field_vector(src_fv, dst_fv, elements)
struct field_t **src_fv, **dst_fv;
int elements;
{
int vbytes;

if (elements <= 0)
	return -1;
vbytes = elements * sizeof(struct field_t *);
(void) bcopy((char *) src_fv, (char *) dst_fv, vbytes);
return 0;
}
