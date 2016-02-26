/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>

#include "ice_defines.h"
#include "ice_externs.h"

extern "C" {
char *			getenv(char *);
char *			strcat(char *, char *);
char *			strchr(char *, int);
int			strcmp(char *, char *);
char *			strcpy(char *, char *);
int			strlen(char *);
int			strncmp(char *, char *, int);
char *			strncpy(char *, char *, int);
}

extern void		ice_err(char *, int);

static char **psfonts= (char **) NULL;

void
get_psfonts()
{
	char *fp, *fontpath, *fontdir;
	DIR *dir;
	struct dirent *dp;
	int len, nfonts, i;
	boolean colon, unsorted;

	npsfonts= 0;
	npsfontfamilies= 0;
	psfontfamilies= (PSFontFamily *) NULL;

	if ((fp= getenv("FONTPATH")) == (char *) NULL) {
		if ((fp= getenv("OPENWINHOME")) == (char *) NULL) {
			ice_err("Cannot locate PostScript fonts.", NONFATAL);
			return;
		}
		if ((fontpath= new char[strlen(fp)+strlen("/lib/fonts")+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			return;
		}
		(void) strcpy(fontpath, fp);
		(void) strcat(fontpath, "/lib/fonts");
	}
	else {
		if ((fontpath= new char[strlen(fp)+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			return;
		}
		(void) strcpy(fontpath, fp);
	}

	/* determine total number of fonts */
	for (i= 0; fontpath[i] != '\0'; ) {
		for (; fontpath[i] == ':'; i++);
		if (fontpath[i] == '\0')
			break;
		fontdir= &(fontpath[i]);
		for (i++; (fontpath[i] != ':') && (fontpath[i] != '\0'); i++);
		if (fontpath[i] == ':') {
			colon= TRUE;
			fontpath[i]= '\0';
		}
		else
			colon= FALSE;
		if ((dir= opendir(fontdir)) == (DIR *) NULL) {
			ice_err("Cannot locate PostScript fonts.", NONFATAL);
			delete fontpath;
			return;
		}
		for (dp= readdir(dir); dp != (struct dirent *) NULL; dp= readdir(dir)) {
			if ((len= strlen(dp->d_name)) < 5)
				continue;
			if (strcmp(dp->d_name+len-4, ".f3b"))
				continue;
			npsfonts++;
		}
		closedir(dir);
		if (colon)
			fontpath[i]= ':';
	}
	if (npsfonts == 0) {
		ice_err("Cannot locate PostScript fonts.", NONFATAL);
		delete fontpath;
		return;
	}

	/* allocate space for font names */
	if ((psfonts= new char *[npsfonts]) == (char **) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		delete fontpath;
		return;
	}
	for (nfonts= 0; nfonts < npsfonts; nfonts++)
		psfonts[nfonts]= (char *) NULL;

	/* store font names */
	nfonts= 0;
	for (i= 0; fontpath[i] != '\0'; ) {
		for (; fontpath[i] == ':'; i++);
		if (fontpath[i] == '\0')
			break;
		fontdir= &(fontpath[i]);
		for (i++; (fontpath[i] != ':') && (fontpath[i] != '\0'); i++);
		if (fontpath[i] == ':') {
			colon= TRUE;
			fontpath[i]= '\0';
		}
		else
			colon= FALSE;
		if ((dir= opendir(fontdir)) == (DIR *) NULL) {
			ice_err("Cannot locate PostScript fonts.", NONFATAL);
			delete fontpath;
			npsfonts= 0;
			delete psfonts;
			psfonts= (char **) NULL;
			return;
		}
		for (dp= readdir(dir); dp != (struct dirent *) NULL; dp= readdir(dir)) {
			if ((len= strlen(dp->d_name)) < 5)
				continue;
			if (strcmp(dp->d_name+len-4, ".f3b"))
				continue;
			if ((psfonts[nfonts]= new char[len-3]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete fontpath;
				npsfonts= nfonts;
				closedir(dir);
				return;
			}
			(void) strncpy(psfonts[nfonts], dp->d_name, len-4);

			/* this line is necessary to neutralize a bizarre bug
			   which causes the preceding strncpy() to fail to write
			   a terminating null when the program is run on certain
			   machines with a particular .Xdefaults file (??!!) */
			psfonts[nfonts][len-4]= '\0';

			nfonts++;
		}
		closedir(dir);
		if (colon)
			fontpath[i]= ':';
	}

	/* sort font names */
	unsorted= TRUE;
	while (unsorted) {
		unsorted= FALSE;
		for (i= 0; i < npsfonts-1; i++) {
			if (strcmp(psfonts[i], psfonts[i+1]) > 0) {
				char *tmp= psfonts[i+1];
				psfonts[i+1]= psfonts[i];
				psfonts[i]= tmp;
				unsorted= TRUE;
			}
		}
	}

	delete fontpath;
	return;
}

void
get_psfontfamilies()
{
	int i, j;
	char family[MAX_FONTFAMILYLEN+1];
	char *c;
	int len, start, stop;
	int nfamily;

	/* determine number of families */
	for (i= 0; i < npsfonts; i++) {
		bzero(family, 256);
		if ((c= strchr(psfonts[i], (int) '-')) == (char *) NULL) {
			if ((len= strlen(psfonts[i])) > MAX_FONTFAMILYLEN)
				continue;
		}
		else {
			if ((len= c-psfonts[i]) > MAX_FONTFAMILYLEN)
				continue;
		}
		(void) strncpy(family, psfonts[i], len);
		start= i;
		if (i < npsfonts-1) {
			for (stop= start; stop+1 < npsfonts; stop++) {
				if (strncmp(family, psfonts[stop+1], len))
					break;
				if ((strlen(psfonts[stop+1]) > len) &&
				     (psfonts[stop+1][len] != '-'))
					break;
			}
		}
		else
			stop= start;
		i= stop;
		npsfontfamilies++;
	}

	/* allocate space for font families */
	if ((psfontfamilies= new PSFontFamily[npsfontfamilies]) == (PSFontFamily *) NULL) {
		ice_err("Memory allocation error.", NONFATAL);
		npsfonts= 0;
		delete psfonts;
		return;
	}

	/* process families */
	nfamily= 0;
	for (i= 0; i < npsfonts; i++) {
		if ((c= strchr(psfonts[i], (int) '-')) == (char *) NULL) {
			if ((len= strlen(psfonts[i])) > MAX_FONTFAMILYLEN)
				continue;
		}
		else {
			if ((len= c-psfonts[i]) > MAX_FONTFAMILYLEN)
				continue;
		}
		if ((psfontfamilies[nfamily].psff_name= new char[len+1]) == (char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			delete psfonts;
			for (i= 0; i < nfamily; i++) {
				for (j= 0; j < psfontfamilies[i].psff_count; j++)
					delete psfontfamilies[i].psff_fonts[j];
				delete psfontfamilies[i].psff_fonts;
			}
			delete psfontfamilies;
			npsfonts= npsfontfamilies= 0;
		}
		(void) strncpy(psfontfamilies[nfamily].psff_name, psfonts[i], len);
		psfontfamilies[nfamily].psff_name[len]= '\0';
		start= i;
		if (i < npsfonts-1) {
			for (stop= start; stop+1 < npsfonts; stop++) {
				if (strncmp(psfontfamilies[nfamily].psff_name, psfonts[stop+1], len))
					break;
				if ((strlen(psfonts[stop+1]) > len) &&
				     (psfonts[stop+1][len] != '-'))
					break;
			}
		}
		else
			stop= start;
		psfontfamilies[nfamily].psff_count= stop-start+1;
		if ((psfontfamilies[nfamily].psff_fonts= new char *[stop-start+1]) == (char **) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			delete psfonts;
			for (i= 0; i < nfamily; i++) {
				for (j= 0; j < psfontfamilies[i].psff_count; j++)
					delete psfontfamilies[i].psff_fonts[j];
				delete psfontfamilies[i].psff_fonts;
			}
			delete psfontfamilies;
			npsfonts= npsfontfamilies= 0;
		}
		for (i= start; i <= stop; i++) {
			if ((psfontfamilies[nfamily].psff_fonts[i-start]= new char[strlen(psfonts[i])+1]) == (char *) NULL) {
				ice_err("Memory allocation error.", NONFATAL);
				delete psfonts;
				for (i= 0; i < nfamily; i++) {
					for (j= 0; j < psfontfamilies[i].psff_count; j++)
						delete psfontfamilies[i].psff_fonts[j];
					delete psfontfamilies[i].psff_fonts;
				}
				delete psfontfamilies;
				npsfonts= npsfontfamilies= 0;
			}
			(void) strcpy(psfontfamilies[nfamily].psff_fonts[i-start], psfonts[i]);
		}
		i= stop;
		nfamily++;
	}
	return;
}
