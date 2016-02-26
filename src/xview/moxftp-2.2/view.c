/* * Copyright (c) 1993 Rob Kooper (kooper@cc.gatech.edu) * All rights reserved.  */
#include "defs.h"
#include "view_progs.h"
#include "proto/view.h"

#define TMP_TAR "/tmp/viewtar"

struct _file_format file_format[] = {
   { "aiff", 8, "AIFF",     0, AUDIO }, 
   { "tiff", 0, NULL,   18761, PICTURE }, 
   { "jpg",  0, NULL,   65496, PICTURE }, 
   { "gif",  0, "GIF",      0, PICTURE }, 
   { "ps",   0, "%!",       0, PS }, 
   { "au",   0, ".snd",     0, AUDIO }, 
   { "rgb",  0, NULL,     474, PICTURE }, 
   { "tar",  0, NULL,       0, TAR }
};


int NO_FORMATS = sizeof(file_format)/sizeof(struct _file_format);

#ifndef SEEK_SET
#define SEEK_SET 0
#endif


void
View(file, name)
char *file;
char *name;
{
    char *command = NULL;

    switch(file_recognition(file, name)) {
	case NONE:	unlink(file);
			break;
	case TEXT:	if (strcmp(viewers[TEXT].command, "builtin") == 0) {
			    View_The_File(file, name);
			} else {
			    command = 
				XtMalloc(2*strlen(file)+
				         strlen(viewers[TEXT].command)+14);
			    sprintf(command, "(%s %s ; rm %s)&\n",
				viewers[TEXT].command, file, file);
			    system(command);
			    XtFree(command);
			}
			break;
	case PICTURE:	if (strcmp(viewers[PICTURE].command, "none") != 0) {
			    command = 
				XtMalloc(2*strlen(file)+
				         strlen(viewers[PICTURE].command)+14);
			    sprintf(command, "(%s %s ; rm %s)&\n",
				viewers[PICTURE].command, file, file);
			    system(command);
			    XtFree(command);
			} else {
			    Set_Status("Sorry could not display file.");
			    unlink(file);
			}
			break;
	case PS:	if (strcmp(viewers[PS].command, "none") != 0) {
			    command = 
				XtMalloc(2*strlen(file)+
				         strlen(viewers[PS].command)+14);
			    sprintf(command, "(%s %s ; rm %s)&\n",
				viewers[PS].command, file, file);
			    system(command);
			    XtFree(command);
			} else {
			    Set_Status("Sorry could not display file.");
			    unlink(file);
			}
			break;
	case AUDIO:	if (strcmp(viewers[AUDIO].command, "none") != 0) {
			    command = 
				XtMalloc(2*strlen(file)+
				         strlen(viewers[AUDIO].command)+14);
			    sprintf(command, "(%s %s ; rm %s)&\n",
				viewers[AUDIO].command, file, file);
			    system(command);
			    XtFree(command);
			} else {
			    Set_Status("Sorry could play file.");
			    unlink(file);
			}
			break;
	case TAR:	if (strcmp(viewers[TAR].command, "none") != 0) {
			    command =
				XtMalloc(strlen(file)+strlen(TAR_PROG)+
					 strlen(viewers[TAR].command)+8);
			    sprintf(command, "%s %s > %s\n",
				viewers[TAR].command, file, TMP_TAR);
			    system(command);
			    unlink(file);
			    XtFree(file);
			    file = concat(NULL, TMP_TAR);
			    XtFree(command);
			    if (strcmp(TEXT_PROG, "builtin") == 0) {
				View_The_File(file, name);
			    } else {
				command = 
				    XtMalloc(2*strlen(file)+
					strlen(viewers[TEXT].command)+14);
				sprintf(command, "(%s %s ; rm %s)&\n",
				    viewers[TEXT].command, file, file);
				system(command);
				XtFree(command);
			    }
			} else {
			    Set_Status("Sorry could not untar file.");
			    unlink(file);
			}
			break;
    }
}

static int
file_recognition(file, name)
char *file;
char *name;
{
    int   n, j;
    char *command = NULL;
    FILE *fp;
    char  line[2000];
    short magic;
    int   imagic;
    
    if ((fp = fopen(file, "r")) == NULL)
	return(NONE);
    
    for (j=0; j<NO_FORMATS; j++) {
	fseek(fp, file_format[j].offset, SEEK_SET);
	if (file_format[j].magic_string != NULL) {
	    n = fread(line, sizeof(char), strlen(file_format[j].magic_string), fp);
	    line[n] = '\0';
	    if ( (n == strlen(file_format[j].magic_string)) && 
	         (strcmp(file_format[j].magic_string, line) == 0) ) {
		fclose(fp);
		sprintf(line, "%s is a %s-file.", name,
		    file_format[j].extension);
		Set_Status_Short(line);
		return(file_format[j].return_type);
	    }
	} else if (file_format[j].magic_number != 0) {
	    n = fread(&magic, sizeof(short), 1, fp);
	    imagic = (int)((unsigned int)magic);
	    if ( (n == 1) && (file_format[j].magic_number == imagic) ) {
		fclose(fp);
		sprintf(line, "%s is a %s-file.", name,
		    file_format[j].extension);
		Set_Status_Short(line);
		return(file_format[j].return_type);
	    }
	} else {
	    n = strlen(file);
	    strcpy(line, ".");
	    strcat(line, file_format[j].extension);
	    if (n > (int)strlen(line))
		if (strcasecmp(&file[n-strlen(line)], line) == 0) {
		    fclose(fp);
		    sprintf(line, "%s is a %s-file.", name,
			file_format[j].extension);
		    Set_Status_Short(line);
		    return(file_format[j].return_type);
		}
	}
    }
    
    sprintf(line, "Assuming %s is a text-file.", name);
    Set_Status_Short(line);
    fclose(fp);
    return(TEXT);
}
