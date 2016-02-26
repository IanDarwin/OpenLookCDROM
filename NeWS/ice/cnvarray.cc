/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>

#include "ice_defines.h"
#include "ice_externs.h"

extern "C" {
char *			gconvert(double, int, int, char *);
char *			strcat(char *, char*);
char *			strcpy(char *, char*);
int			strlen(char *);
}

int
flt2str(float *farray, int flen, char **str)
/*
   Convert an array of floats to a string.
*/
{
	char *buf, smallbuf[10];
	int i;

	*str= (char *) NULL;
	if ((buf= new char[(10*flen)+1]) == (char *) NULL)
		return GROBJ_FAILURE;
	bzero(buf, (10*flen)+1);

	for (i= 0; i < flen; i++) {
		(void) gconvert((double) ((float) *(farray+i)), 5, 0, smallbuf);
		(void) strcat(buf, smallbuf);
		if (i < flen-1)
			(void) strcat(buf, " ");
	}
	if ((*str= new char[strlen(buf)+1]) == (char *) NULL) {
		delete buf;
		return GROBJ_FAILURE;
	}
	(void) strcpy(*str, buf);
	delete buf;

	return GROBJ_SUCCESS;
}

int
str2flt(char *str, float **farray, int *flen)
/*
   Convert a string to an array of floats.
*/
{
	int i;
	float f;
	char *start, *stop;

	*farray= (float *) NULL;
	*flen= 0;

	for (start= str; *start == ' '; start++);
	if (*start == '\0')
		return GROBJ_FAILURE;
	for (i= 0; TRUE; ) {
		f= (float) strtod(start, &stop);
		if (stop == start)
			return GROBJ_FAILURE;
		else if (*stop == '\0') {
			i++;
			break;
		}
		else if (*stop == ' ') {
			i++;
			for (start= stop; *start == ' '; start++);
			if (*start == '\0')
				break;
		}
		else
			return GROBJ_FAILURE;
	}

	if ((*farray= new float[i]) == (float *) NULL)
		return GROBJ_FAILURE;
	*flen= i;
	for (start= str; *start == ' '; start++);
	for (i= 0; TRUE; ) {
		*(*farray+i)= (float) strtod(start, &stop);
		if (*stop == '\0')
			break;
		else if (*stop == ' ') {
			i++;
			for (start= stop; *start == ' '; start++);
			if (*start == '\0')
				break;
		}
	}

	return GROBJ_SUCCESS;
}
