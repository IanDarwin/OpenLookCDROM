/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* File: h_test.c */

/******************************************/
/* This program tests header functions    */
/* in an infinite loop that continually   */
/* inserts, retrieves, and deletes fields */
/* from a header.                         */
/* If there is a bug in there, we hope    */
/* that a function will eventually fail   */
/* due to a detected inconsistency or by  */
/* running out of memory.                 */
/******************************************/

#include <stdio.h>
#include "header.h"
#include "sp.h"
#include "version.h"

#define LONG1		27L
#define LONG2		-55L
#define DOUBLE1		205.17197
#define STR1		"foobar"

main(argc,argv)
int argc;
char *argv[];
{
char *prog;
struct header_t *h;
int i, n;
int f1size, f2size, f3size;
char buf[BUFSIZ];
extern char *rindex();

prog = rindex(argv[0],'/');
prog = (prog == CNULL) ? argv[0] : (prog + 1);

if (argc != 1) {
	(void) fprintf(stderr,"%s: no arguments expected\n",prog);
	exit(ERROR_EXIT_STATUS);
}

h = sp_create_header();
if (h == HDRNULL) {
	(void) fprintf(stderr,"%s: Could not create header %s\n",prog);
	exit(ERROR_EXIT_STATUS);
}

for (i=0;;i++) {

static double r = DOUBLE1;
static long l = LONG1, l2 = LONG2;

n = sp_add_field(h,"field1",T_INTEGER,(char *) &l);
if (n >= 0) n = sp_add_field(h,"field2",T_REAL,(char *) &r);
if (n >= 0) n = sp_add_field(h,"field3",T_STRING,STR1);
if (n < 0) {
	(void) fprintf(stderr,"%s: iteration %d -- error adding fields to header\n",prog,i);
	(void) sp_print_lines(h,stdout);
	exit(ERROR_EXIT_STATUS);
}

f1size = sizeof(long);
f2size = sizeof(double);
f3size = sizeof(STR1) - 1;
n = sp_get_data(h,"field1",buf,&f1size);
if (n >= 0) if (*(long *)buf != LONG1) n = -1;
if (n >= 0) {
	n = sp_get_data(h,"field2",buf,&f2size);
	if (n >= 0) if (*(double *)buf != DOUBLE1) n = -1;
}
if (n >= 0) {
	n = sp_get_data(h,"field3",buf,&f3size);
	if (n >= 0) if (strncmp(buf,STR1,sizeof(STR1)-1) != 0) n = -1;
}
if (n < 0) {
	(void) fprintf(stderr,"%s: iteration %d -- error retrieving values\n",prog,i);
	(void) sp_print_lines(h,stdout);
	exit(ERROR_EXIT_STATUS);
}

if (rand() % 2) {
	n = sp_delete_field(h,"field1");
	if (n >= 0) n = sp_delete_field(h,"field2");
	if (n >= 0) n = sp_change_field(h,"field3",T_INTEGER,(char *)&l2);
	if (n >= 0) n = sp_delete_field(h,"field3");
} else
	n = sp_clear_fields(h);
if (n < 0) {
	(void) fprintf(stderr,"%s: iteration %d -- error deleting field(s) from header\n",prog,i);
	(void) sp_print_lines(h,stdout);
	exit(ERROR_EXIT_STATUS);
}
}
}
