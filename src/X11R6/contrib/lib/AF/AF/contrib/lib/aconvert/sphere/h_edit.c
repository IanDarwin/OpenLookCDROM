/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* File: h_edit.c */

#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "header.h"
#include "sp.h"
#include "version.h"

extern int ignore_failure;
extern char *prog;

extern char *index();
extern long atol();
extern double atof();

char opspec[] = "D:S:I:R:fo:u";
char ops[]    = "SIR";

int wav_edit(f1,f2,fct,f,fop)
char *f1, *f2, *f[], fop[];
int fct;
{
struct header_t *h;
char *errmsg;
register FILE *fp1, *fp2;
int i, failed;
long dummy;

fp1 = fopen(f1,"r");
if (fp1 == FPNULL) {
	(void) fprintf(stderr,"%s: %s: Error opening for reading\n",prog,f1);
	return -1;
}

fp2 = fopen(f2,"w");
if (fp2 == FPNULL) {
	(void) fprintf(stderr,"%s: %s: Error opening %s for writing\n",
		prog,f1,f2);
	(void) fclose(fp1);
	return -1;
}

h = sp_open_header(fp1,1,&errmsg);
if (h == HDRNULL) {
	(void) fprintf(stderr,"%s: %s: Error reading header -- %s\n",
		prog,f1,errmsg);
	(void) fclose(fp1);
	(void) fclose(fp2);
	return -1;
}

failed = 0;
for (i=0; i < fct; i++) {
	char *p;
	long lbuf;
	double dbuf;
	int dummy, t, type;

	p = index(f[i],'=');
	if (p == (char *) NULL) {
		(void) fprintf(stderr,"%s: %s: Error in edit specifier %s -- \"=\" required\n",
			prog,f1,f[i]);
		(void) sp_close_header(h);
		(void) fclose(fp1);
		(void) fclose(fp2);
		return -1;
	}

	*p++ = '\0';

	switch (fop[i]) {

	  case 'S':	type = T_STRING;
			break;

	  case 'I':	type = T_INTEGER;
			lbuf = atol(p);
			p = (char *) &lbuf;
			break;

	  case 'R':	type = T_REAL;
			dbuf = atof(p);
			p = (char *) &dbuf;
			break;

	  default:	(void) fprintf(stderr,
				"%s: %s: Error in edit specification -%s\n",
				prog,f1,fop[i]);
			(void) sp_close_header(h);
			(void) fclose(fp1);
			(void) fclose(fp2);
			return -1;
	}


	if (sp_get_field(h,f[i],&t,&dummy) < 0) {
		if (sp_add_field(h,f[i],type,p) < 0) {
			(void) fprintf(stderr,"%s: %s: Error adding field %s\n",
				prog,f1,f[i]);
			failed++;
		}
		continue;
	}

	if (type != t) {
		(void) fprintf(stderr,"%s: %s: Error changing field %s -- type mismatch\n",
			prog,f1,f[i]);
		failed++;
		continue;
	}

	if (sp_change_field(h,f[i],type,p) < 0) {
		(void) fprintf(stderr,"%s: %s: Error changing field %s\n",
			prog,f1,f[i]);
		failed++;
	}
}

if (failed && ! ignore_failure) {
	(void) unlink(f2);
	(void) fclose(fp1);
	(void) fclose(fp2);
	(void) sp_close_header(h);
	return -1;
}

if ((sp_write_header(fp2,h,&dummy,&dummy) < 0) || (sp_fpcopy(fp1,fp2) < 0)) {
	(void) fprintf(stderr,"%s: %s: Error writing to %s\n",
		prog,f1,f2);
	(void) unlink(f2);
	(void) fclose(fp1);
	(void) fclose(fp2);
	(void) sp_close_header(h);
	return -1;
}

(void) fclose(fp1);
(void) fclose(fp2);

if (sp_close_header(h) < 0) {
	(void) fprintf(stderr,"%s: %s: Warning -- close_header failed\n",
		prog,f1);
	return -1;
}

return 0;
}
