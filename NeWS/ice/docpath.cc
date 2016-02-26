/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"

extern "C" {
char *			getenv(char *);
char *			strcat(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
}

extern void		ice_err(char *, int);

void
get_pathdirs()
{
	char *dp, *docpath, *docdir, *dirname;
	Pathdir *pd;
	int len, i;
	boolean colon;

	pathdirs= (Pathdir *) NULL;

	if ((dp= getenv("ICEDOCPATH")) == (char *) NULL)
		return;

	if ((docpath= new char[strlen(dp)+1]) == (char *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		return;
	}
	(void) strcpy(docpath, dp);

	for (i= 0; docpath[i] != '\0'; ) {
		for (; docpath[i] == ':'; i++);
		if (docpath[i] == '\0')
			break;
		docdir= &(docpath[i]);
		for (i++; (docpath[i] != ':') && (docpath[i] != '\0'); i++);
		if (docpath[i] == ':') {
			colon= TRUE;
			docpath[i]= '\0';
		}
		else
			colon= FALSE;

		len= strlen(docdir);
		if (docdir[strlen(docdir)-1] != '/')
			len++;
		if ((dirname= new char[len+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			delete docpath;
			return;
		}
		(void) strcpy(dirname, docdir);
		if (len != strlen(docdir))
			(void) strcat(dirname, "/");
		if ((pd= new Pathdir((Dlnk **) &pathdirs, dirname)) == (Pathdir *) NULL) {
			ice_err("Cannot create docpath entry.", NONFATAL);
			delete docpath;
			return;
		}

		if (colon)
			docpath[i]= ':';
	}

	delete docpath;
	return;
}

FILE *
doc_open(char *filenm, char *flags)
{
	FILE *fp;
	Pathdir *pd;
	char *dirnm, *fullnm;
	int len;

	if ((pathdirs == (Pathdir *) NULL) || (filenm[0] == '/'))
		return fopen(filenm, flags);

	for (pd= pathdirs; pd != (Pathdir *) NULL; pd= (Pathdir *) pd->succ()) {
		dirnm= pd->getdirname();
		len= strlen(dirnm)+strlen(filenm)+1;
		if ((fullnm= new char[len+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			return (FILE *) NULL;
		}
		(void) strcpy(fullnm, dirnm);
		(void) strcat(fullnm, filenm);
		fp= fopen(fullnm, flags);
		delete fullnm;
		if (fp != (FILE *) NULL)
			return fp;
	}
	return (FILE *) NULL;
}
