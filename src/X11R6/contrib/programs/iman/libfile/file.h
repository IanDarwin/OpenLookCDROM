/*
 *
 * 	file.h
 * 	structures for directory and file management
 *
 * 	Modification :  03/05/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appears in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Bruno RIVAS not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  Bruno RIVAS makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Bruno RIVAS disclaims all warranties with regard to this software,
 * including all implied warranties of merchantability and fitness,
 * in no event shall Bruno RIVAS be liable for any special,
 * indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits,
 * whether in an action of contract, negligence or other tortious
 * action, arising out of or in connection with the use 
 * or performance of this software.
 *
 *
 *
 *      libMfile.a  version 1.0
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */



#ifndef MISC_DIR_H
#define MISC_DIR_H



typedef struct
{
  char *name;
  unsigned short mode;
  unsigned short owner;
  unsigned group;
  long size;
  short year, month, day;
  short hour, minute, seconde;

}FileInfo;




typedef struct
{
  char *name;
  int mode;
  int nivel;

}TreeInfo;


#define FILENAME_LENGTH 	35



extern int file_GetNumValidFiles(
	char *,
	char *
);


extern char **file_GetNames(
	char *,
	char *,
	int *
);


extern char **file_GetOrderedNames(
	char *,
	char *,
	int *
);


extern FileInfo **file_GetInfos(
	char *, char *,
	int *
);


extern FileInfo **file_GetOrderedInfos(
	char *, char *,
	int *
);


extern int VerifyMask(
	char *, 
	char *
);


extern int file_VerifyAccess(char *);

extern char *file_ExtractPath(char *);

extern char *file_ExtractName(char *);

extern int file_IsDirectory(char *);

extern int file_IsSpecial(char *);

extern int file_IsNormal(char *);

extern int file_FreeNames(
	char **,
	int
);

extern int file_FreeInfos(
	FileInfo **,
	int
);

extern int file_Delete(char *);

extern TreeInfo **dir_GetTree(
	char *,
	int *
);

extern int dir_FreeTree(
	TreeInfo **,
	int
);

extern char *dir_GetCurrent();

extern dir_SetCurrent(char *);

#endif



