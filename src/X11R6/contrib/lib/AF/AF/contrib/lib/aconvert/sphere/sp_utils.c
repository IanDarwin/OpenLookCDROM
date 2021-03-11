/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* LINTLIBRARY */

/** File: sp_utils.c **/

#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include "header.h"
#include "sysparam.h"
#include "sysfunc.h"
#include "sp.h"
#include "version.h"

extern int farray_fields;
extern struct field_t *farray[];

/***************************************************************/
/* Reads an existing header in from file pointer "fp".         */
/* The file pointer is assumed to be positioned at the         */
/*    beginning of a speech file with a header in NIST SPHERE  */
/*    format.                                                  */
/* On success, "fp" is positioned at the end of the header     */
/*    (ready to read samples) and a pointer to a header        */
/*    structure is returned.                                   */
/* On failure, argument "error" will point to a string         */
/*    describing the problem.                                  */
/* If "parse_flag" is false (zero), the fields in the header   */
/*    will not be parsed and inserted into the header          */
/*    structure; the structure will contain zero fields.       */
/*    This is useful for operations on files when the contents */
/*    of the header are not important, for example when        */
/*    stripping the header.                                    */
/***************************************************************/

FUNCTION struct header_t *sp_open_header(fp,parse_flag,error)
register FILE *fp;
char **error;
int parse_flag;
{
register struct header_t *h;
int header_size, i;
struct field_t **fv;

if (fp == FPNULL)	return HDRNULL;	/* check sanity of arguments */

if (ftell(fp) != 0L)	return HDRNULL; /* fp must be at beginning of file */

if (spx_read_header(fp,&header_size,parse_flag,error) < 0)
	return HDRNULL;

if ((! parse_flag) || (farray_fields == 0))
	fv = FVNULL;
else {
	fv = spx_get_field_vector(farray_fields);
	if (fv == FVNULL) {
		for (i=0; i<farray_fields; i++)
			(void) spx_deallocate_field(farray[i]);
		return HDRNULL;
	}
	(void) spx_copy_field_vector(farray, fv, farray_fields);
}

h = spx_allocate_header(farray_fields,fv);
if (h == HDRNULL)
	for (i=0; i<farray_fields; i++)
		(void) spx_deallocate_field(farray[i]);
return h;
}

/*******************************************************************/
/* Deletes all fields from the header pointed to by h.             */
/*******************************************************************/

FUNCTION int sp_clear_fields(h)
register struct header_t *h;
{
register int i, j, errors = 0;

if (h == HDRNULL)	return -1;	/* check sanity of arguments */

for (i=0, j = h->fc; i<j; i++) {
	if (spx_deallocate_field(h->fv[i]) < 0)
		errors++;
	h->fv[i] = FNULL;
}
if (h->fv != FVNULL)
	spx_free((char *) h->fv);
h->fv = FVNULL;
h->fc = 0;

return errors ? -1 : 0;
}

/***********************************************************************/
/* Reclaims the space allocated for the header structure pointed to    */
/* by h. First reclaims all space allocated for the header's fields,   */
/* if any exist.                                                       */
/***********************************************************************/

FUNCTION int sp_close_header(h)
register struct header_t *h;
{
(void) sp_clear_fields(h);
spx_free((char *) h);
return 0;
}

/*********************************************************************/
/* Returns the number of fields stored in the specified header.      */
/*********************************************************************/

FUNCTION int sp_get_nfields(h)
struct header_t *h;
{
if (h == HDRNULL)	return -1;	/* check sanity of arguments */

return h->fc;
}

/*********************************************************************/
/* Fills in an array of character pointers with addresses of the     */
/* fields in the specified header. No more than n pointers in the    */
/* array will be set.                                                */
/* Returns the number of pointers set.                               */
/*********************************************************************/

FUNCTION int sp_get_fieldnames(h,n,v)
struct header_t *h;
int n;
char *v[];
{
register struct field_t **fv;
int i, fc;

if (h == HDRNULL)
	return -1;	/* check sanity of arguments */
if (v == (char **) NULL)
	return -1;

fc = h->fc;
fv = h->fv;
for (i=0; i < fc && i < n; i++)
	v[i] = fv[i]->name;
return i;
}

/***********************************************************************/
/* Returns the type and size (in bytes) of the specified header field. */
/* Types are T_INTEGER, T_REAL, T_STRING (defined in header.h).        */
/* The size of a T_INTEGER field is sizeof(long).                      */
/* The size of a T_REAL field is sizeof(double).                       */
/* The size of a string is variable and does not includes a            */
/*    null-terminator byte (null bytes are allowed in a string).       */
/***********************************************************************/

FUNCTION int sp_get_field(h,name,type,size)
struct header_t *h;
char *name;
int *type, *size;
{
register int i, fc;
register struct field_t **fv;

if (h == HDRNULL)	return -1;	/* check sanity of arguments */
if (name == CNULL)	return -1;

fc = h->fc;
fv = h->fv;
for (i=0; i < fc ; i++, fv++)
	if (strcmp(name,(*fv)->name) == 0) {
		switch ((*fv)->type) {
			case T_INTEGER:
				*size = sizeof(long);
				break;
			case T_REAL:
				*size = sizeof(double);
				break;
			case T_STRING:
				*size = (*fv)->datalen;
				break;
			default:
				return -1;
		}
		*type = (*fv)->type;
		return 0;
	}
return -1;
}

/*********************************************************************/
/* Returns the type of the specified header field.                   */
/* Types are T_INTEGER, T_REAL, T_STRING (defined in header.h).      */
/*********************************************************************/

FUNCTION int sp_get_type(h,name)
struct header_t *h;
char *name;
{
register int i, fc;
register struct field_t **fv;

if (h == HDRNULL)	return -1;	/* check sanity of arguments */
if (name == CNULL)	return -1;

fc = h->fc;
fv = h->fv;
for (i=0; i < fc ; i++, fv++)
	if (strcmp(name,(*fv)->name) == 0)
		switch ((*fv)->type) {
			case T_INTEGER:
			case T_REAL:
			case T_STRING:
				return (*fv)->type;
			default:
				return -1;
		}
return -1;
}

/*********************************************************************/
/* Returns the size (in bytes) of the specified header field.        */
/* The size of a T_INTEGER field is sizeof(long).                    */
/* The size of a T_REAL field is sizeof(double).                     */
/* The size of a string is variable and does not includes a          */
/*    null-terminator byte (null bytes are allowed in a string).     */
/*********************************************************************/

FUNCTION int sp_get_size(h,name)
struct header_t *h;
char *name;
{
register int i, fc;
register struct field_t **fv;

if (h == HDRNULL)	return -1;	/* check sanity of arguments */
if (name == CNULL)	return -1;

fc = h->fc;
fv = h->fv;
for (i=0; i < fc ; i++, fv++)
	if (strcmp(name,(*fv)->name) == 0)
		switch ((*fv)->type) {
			case T_INTEGER:
				return sizeof(long);
			case T_REAL:
				return sizeof(double);
			case T_STRING:
				return (*fv)->datalen;
			default:
				return -1;
		}
return -1;
}

/***********************************************************************/
/* Returns the value of the specifed header field in "buf".            */
/* No more than "len" bytes are copied; "len" must be positive.        */
/* It really doesn't make much sense to ask for part of a long or      */
/*    double, but it's not illegal.                                    */
/* Remember that strings are not null-terminated.                      */
/***********************************************************************/

FUNCTION int sp_get_data(h,name,buf,len)
struct header_t *h;
char *name, *buf;
int *len;
{
register struct field_t **fv;
register int i, fc;
long n;
double x;

if (h == HDRNULL)	return -1;	/* check sanity of arguments */
if (name == CNULL)	return -1;
if (buf == CNULL)	return -1;
if (len == INULL)	return -1;
if (*len <= 0)		return -1;

fc = h->fc;
fv = h->fv;
for (i=0; i<fc; i++, fv++)
	if (strcmp(name,(*fv)->name) == 0) {
		switch ((*fv)->type) {
			case T_INTEGER:
				n = atol((*fv)->data);
				*len = MIN(*len,sizeof(long));
				(void) bcopy((char *) &n, buf, *len);
				break;
			case T_REAL:
				x = atof((*fv)->data);
				*len = MIN(*len,sizeof(double));
				(void) bcopy((char *) &x, buf, *len);
				break;
			case T_STRING:
				*len = MIN(*len,(*fv)->datalen);
				(void) bcopy((*fv)->data, buf, *len);
				break;
			default:
				return -1;
		}
		return 0;
	}
return -1;
}

/*******************************************************************/
/* Adds the field "name" to header specified by "h".               */
/* Argument "type" is T_INTEGER, T_REAL, or T_STRING.              */
/* Argument "p" is a pointer to a long integer, a double, or a     */
/*    character cast if necessary to a character pointer.          */
/* The specified field must not already exist in the header.       */
/*******************************************************************/

FUNCTION int sp_add_field(h,name,type,p)
struct header_t *h;
int type;
char *name, *p;
{
register struct field_t **fv, *nf;
int size, i, fc;

if (h == HDRNULL)		return -1;	/* check sanity of arguments */
if (h->fc < 0)			return -1;
if (name == CNULL)		return -1;
if (p == CNULL)			return -1;
if (spx_tp(type) == '?')	return -1;

fc = h->fc;
for (i=0; i < fc; i++)
	if (strcmp(name,h->fv[i]->name) == 0)
		return -1;

switch (type) {
	case T_INTEGER:
		size = sizeof(long);
		break;
	case T_REAL:
		size = sizeof(double);
		break;
	default:
		size = strlen(p);
		break;
}

nf = spx_allocate_field(type,name,p,size);
if (nf == FNULL)
	return -1;

fv = spx_get_field_vector(fc + 1);
if (fv == FVNULL) {
	(void) spx_deallocate_field(nf);
	return -1;
}

if (fc > 0) {
	(void) spx_copy_field_vector(h->fv, fv, fc);
	spx_free((char *) h->fv);
}
fv[h->fc++] = nf;
h->fv = fv;
return 0;
}

/***********************************************************/
/* Deletes field "name" from header specified by "h".      */
/* The field must exist in the header.                     */
/***********************************************************/

FUNCTION int sp_delete_field(h,name)
struct header_t *h;
char *name;
{
struct field_t **tmp_fv, *nf;
int i, new_fc;

if (h == HDRNULL)	return -1;	/* check sanity of arguments */
if (h->fc <= 0)		return -1;
if (name == CNULL)	return -1;

if (h->fc > 1) {
	tmp_fv = spx_get_field_vector(h->fc - 1);
	if (tmp_fv == FVNULL)
		return -1;
} else
	tmp_fv = FVNULL;
nf = FNULL;
for (i=0, new_fc=0; i< h->fc; i++)
	if (strcmp(name,h->fv[i]->name) == 0) {
		if (nf != FNULL) {
			spx_free((char *) tmp_fv);
			return -1;
		}
		nf = h->fv[i];
	} else {
		if ((nf == FNULL) && (i == h->fc - 1)) {
			spx_free((char *) tmp_fv);
			return -1;
		}
		tmp_fv[new_fc++] = h->fv[i];
	}
(void) spx_deallocate_field(nf);
spx_free((char *) h->fv);
h->fv = tmp_fv;
--h->fc;
return 0;
}

/***********************************************************/
/* Changes an existing field to a new type and/or value.   */
/* The field must already exist in the header.             */
/***********************************************************/

FUNCTION int sp_change_field(h,name,type,p)
struct header_t *h;
char *name, *p;
int type;
{
register int i, index, size;
struct field_t *nf;

if (h == HDRNULL)		return -1;	/* check sanity of arguments */
if (name == CNULL)		return -1;
if (p == CNULL)			return -1;
if (spx_tp(type) == '?')	return -1;

for (i=0, index = -1; i< h->fc; i++)
	if (strcmp(h->fv[i]->name,name) == 0) {
		if (index >= 0) return -1;
		index = i;
	}
if (index < 0) return -1;

switch (type) {
	case T_INTEGER:
		size = sizeof(long); break;
	case T_REAL:
		size = sizeof(double); break;
	default:
		size = strlen(p); break;
}

nf = spx_allocate_field(type,name,p,size);
if (nf == FNULL) return -1;

if (spx_deallocate_field(h->fv[index]) < 0) {
	(void) spx_deallocate_field(nf);
	return -1;
}
h->fv[index] = nf;
return 0;
}

/******************************************************************/
/* Returns a pointer to an empty header.                          */
/* Use sp_add_field() to insert fields into it.                   */
/* Use sp_print_header() to print it in readable format.          */
/* Use sp_format_header() to print it to a file in NIST SPHERE    */
/*      format.                                                   */
/******************************************************************/

FUNCTION struct header_t *sp_create_header()
{
return spx_allocate_header(0,FVNULL);
}

/*******************************************************************/
/* Returns TRUE if the specified field name is a "standard" field, */
/* FALSE otherwise.                                                */
/* Standard fields are listed in stdfield.c.                       */
/*******************************************************************/

FUNCTION int sp_is_std(name)
register char *name;
{
register char **f;

if (name == CNULL)
	return FALSE;

f = &std_fields[0];
while (*f != CNULL)
	if (strcmp(name,*f++) == 0)
		return TRUE;

return FALSE;
}
