/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: man.c,v 1.15 89/12/13 17:07:54 jim Exp $
 *
 * Copyright 1987, 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:    Chris D. Peterson, MIT Project Athena
 * Created:   August 10, 1987
 */

#if ( !defined(lint) && !defined(SABER))
  static char rcs_version[] = "$Athena: man.c,v 4.5 88/12/19 13:47:35 kit Exp $";
#endif

#include <string.h>
#include <dirent.h>
#include <malloc.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <gdd.h>
#include "olxvman_ui.h"
#include "globals.h"

#ifdef DEBUG
static char error_buf[BUFSIZ];		/* The buffer for error messages. */
#endif /* DEBUG */

static void SortList(), ReadMandescFile(), SortAndRemove(), InitManual();
static void AddToCurrentSection(), AddNewSection(), AddStandardSections();
static void ReadCurrentSection();
static int CmpEntryLabel();

#define SECT_ERROR -1

typedef struct _SectionList {
  struct _SectionList * next;	/* link to next elem in list. */
  char * label, *directory;	/* The label and directory that this 
				   section represents. */
  Bool standard;		/* Is this one of the standard sections? */
} SectionList;

/*	Function Name: Man
 *	Description: Builds a list of all manual directories and files.
 *	Arguments: none. 
 *	Returns: the number of manual sections.
 */

int
Man()
{
  SectionList *list = NULL;
  char *ptr, manpath[BUFSIZ], *path, *current_label;
  int sect, num_alloced;

/* 
 * Get the environment variable MANPATH, and if it doesn't exist then back
 * up to MANDIR.
 */

  ptr = getenv("MANPATH");
  if (ptr == NULL || streq(ptr , "") )
    ptr = MANDIR;
  strcpy(manpath, ptr);

/*
 * Get the list of manual directories in the users MANPATH that we should
 * open to look for manual pages.  The ``mandesc'' file is read here.
 */

  for ( path = manpath ; (ptr = index(path , ':')) != NULL ; path = ++ptr) { 
    *ptr = '\0';
    ReadMandescFile(&list, path);
  }
  ReadMandescFile(&list, path);

  SortList(&list);
  
  sect = 0;
  num_alloced = SECTALLOC;
  manual = (Manual *) malloc( sizeof(Manual) * num_alloced );
  InitManual( manual, list->label );
  current_label = NULL;

  while ( list != NULL ) {
    SectionList * old_list;

    if ( current_label == NULL || streq(list->label, current_label) )
      AddToCurrentSection( manual + sect, list->directory);
    else {
      if (manual[sect].nentries == 0) {	/* empty section, re-use it. */
	free(manual[sect].blabel);
	manual[sect].blabel = list->label;
      }
      else {
	if ( ++sect >= num_alloced ) {
	  num_alloced += SECTALLOC;
	  manual = (Manual *) OlRealloc ( (char *) manual,
				        (sizeof(Manual) * num_alloced));
	  if (manual == NULL) 
	    PrintError("Could not allocate memory for manual sections.");
	}
	InitManual( manual + sect, list->label );
      }
      AddToCurrentSection( manual + sect, list->directory);
    }
    /* Save label to see if it matches next entry. */
    current_label = list->label; 
    old_list = list;
    list = list->next;
    free(old_list);		/* free what you allocate. */
  }
  if (manual[sect].nentries != 0)
    sect++;			/* don't forget that last section. */
  
  SortAndRemove(manual, sect);

#ifdef notdef			/* dump info. */
  DumpManual(sect);
#endif
  
/*
 * realloc manual to be minimum space necessary.
 */

  manual = (Manual *) OlRealloc( (char *) manual, (sizeof(Manual) * sect));
  if (manual == NULL) 
    PrintError("Could not allocate memory for manual sections.");

  return(sect);		/* return the number of man sections. */
}    

/*	Function Name: SortList
 *	Description: Sorts the list of sections to search.
 *	Arguments: list - a pointer to the list to sort.
 *	Returns: a sorted list.
 *
 * This is the most complicated part of the entire operation.
 * all sections with the same label must by right next to each other,
 * but the sections that are in the standard list have to come first.
 */

static void
SortList(list)
SectionList ** list;
{
  SectionList * local;
  SectionList *head, *last, *inner, *old;
  
  if (*list == NULL)
    PrintError("No manual sections to read, exiting.");

/* 
 * First step 
 * 
 * Look for standard list items, and more them to the top of the list.
 */

  last = NULL;			/* keep Saber happy. */
  for ( local = *list ; local->next != NULL ; local = local->next) {
    if ( local->standard ) {
      if ( local == *list )	/* top element is already standard. */
	break;
      head = local;

      /* Find end of standard block */
      for ( ; (local->next != NULL) && (local->standard) 
	   ; old = local, local = local->next); 

      last->next = old->next; /* Move the block. */
      old->next = *list;
      *list = head;

      break;			/* First step accomplished. */
    }
    last = local;
  }

/*
 *  Second step
 *
 *  Move items with duplicate labels right next to each other.
 */

  local = *list;
  for ( local = *list ; local->next != NULL ; local = local->next) {
    inner = local->next;
    while ( inner != NULL) {
      if ( streq(inner->label, local->label) && (inner != local->next)) {
	last->next = inner->next; /* Move it to directly follow local. */
	inner->next = local->next;
	local->next = inner;
	inner = last;		/* just so that we keep marching down the
				   tree (this keeps us from looping). */
      }
      last = inner;
      inner = inner->next;
    }
  }
}	

/*	Function Name: ReadMandescFile
 *	Description: Reads the mandesc file, and adds more sections as 
 *                   nescessary.
 *	Arguments: path - path name if the current search directory.
 *                 section_list - pointer to the list of sections.
 *	Returns: TRUE in we should use default sections
 */
  
static void
ReadMandescFile( section_list, path )
SectionList ** section_list;
char * path;
{
  char mandesc_file[BUFSIZ];	/* full path to the mandesc file. */
  FILE * descfile;
  char string[BUFSIZ], local_file[BUFSIZ];
  Bool use_defaults = TRUE;

  sprintf(mandesc_file, "%s/%s", path, MANDESC);
  if ( (descfile = fopen(mandesc_file, "r")) != NULL) {
    while ( fgets(string, BUFSIZ, descfile) != NULL) {
      string[strlen(string)-1] = '\0';        /* Strip off the CR. */

      if ( streq(string, NO_SECTION_DEFAULTS) ) {
	use_defaults = FALSE;
	continue;
      }

      sprintf(local_file, "%s%c", MAN, string[0]);
      AddNewSection(section_list, path, local_file, (string + 1), FALSE );
    }
    fclose(descfile);
  }
  if (use_defaults)
    AddStandardSections(section_list, path);
}

#ifdef CRAY
/*	Function Name: AddStandardCraySections
 *	Description: Add sections specific to the Cray.
 *	Arguments: list - a pointer to the section list.
 *                 path - the path to these standard sections.
 *                 names - standard section names.
 *	Returns: none.
 */

AddStandardCraySections(list, path, names)
SectionList **list;
char *path, **names;
{
  char file[BUFSIZ];

  sprintf(file, "%s1bsd", SEARCHDIR);
  AddNewSection(list, path, file, names[0], TRUE);
  sprintf(file, "%s1m", SEARCHDIR);
  AddNewSection(list, path, file, names[0], TRUE);
  sprintf(file, "%s1r", SEARCHDIR);
  AddNewSection(list, path, file, names[0], TRUE);
  sprintf(file, "%s1rb", SEARCHDIR);
  AddNewSection(list, path, file, names[0], TRUE);
  
  sprintf(file, "%s3X11", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3Xt", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3bsd", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3c", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3db", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3f", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3io", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3m", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3mt", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3n", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3q", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3rpc", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3s", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3sci", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3svc", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3u", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3w", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3x", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3yp", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  sprintf(file, "%s3z", SEARCHDIR);
  AddNewSection(list, path, file, names[2], TRUE);
  
  sprintf(file, "%s4d", SEARCHDIR);
  AddNewSection(list, path, file, names[3], TRUE);
  sprintf(file, "%s4f", SEARCHDIR);
  AddNewSection(list, path, file, names[3], TRUE);
  sprintf(file, "%s4n", SEARCHDIR);
  AddNewSection(list, path, file, names[3], TRUE);
}
#endif /* CRAY */

/*	Function Name: AddStandardSections
 *	Description: Adds all the standard sections to the list for this path.
 *	Arguments: list - a pointer to the section list.
 *                 path - the path to these standard sections.
 *	Returns: none.
 */

static void
AddStandardSections(list, path)
SectionList **list;
char * path;
{
  static char * names[] = {
    "(1) User Commands",
    "(2) System Calls",
    "(3) Subroutines",
    "(4) Devices",
    "(5) File Formats",
    "(6) Games",
    "(7) Miscellaneous",
    "(8) Sys. Administration",
    "(l) Local",
    "(n) New",
    "(o) Old",
    };
  register int i;
  char file[BUFSIZ];

  for (i = 0 ; i < 8 ; i++) {
    sprintf(file, "%s%d", SEARCHDIR, i + 1);
    AddNewSection(list, path, file, names[i], TRUE);
#ifdef hpux			/* Puts in in the correct order */
    if (i == 0) {
      sprintf(file, "%s1m", SEARCHDIR);
      AddNewSection(list, path, file, "(1m) Sys. Administration", TRUE);
    }
#endif
  }
  sprintf(file, "%sl", SEARCHDIR);
  AddNewSection(list, path, file, names[i++], TRUE);
  sprintf(file, "%sn", SEARCHDIR);
  AddNewSection(list, path, file, names[i++], TRUE);
  sprintf(file, "%so", SEARCHDIR);
  AddNewSection(list, path, file, names[i], TRUE);

#ifdef CRAY			/* The Cray's sections are all screwed up. */
  AddStandardCraySections(list, path, names);
#endif
}

/*	Function Name: AddNewSection
 *	Description: Adds the new section onto the current section list.
 *	Arguments: list - pointer to the section list.
 *                 path - the path to the current manual section.
 *                 file - the file to save.
 *                 label - the current section label.
 *                 standard - one of the standard labels?
 *	Returns: none.
 */

static void
AddNewSection(list, path, file, label, standard)
SectionList **list;
char * path, * label, * file;
Bool standard;
{
  SectionList * local_list, * end;
  char full_path[BUFSIZ];

/* Allocate a new list element */

  local_list = (SectionList *) malloc(sizeof(SectionList));

  if (*list != NULL) {
    for ( end = *list ; end->next != NULL ; end = end->next );
    end->next = local_list;
  }
  else 
    *list = local_list;

  local_list->next = NULL;
  local_list->label = StrAlloc(label);
  sprintf(full_path, "%s/%s", path, file);
  local_list->directory = StrAlloc(full_path);
  local_list->standard = standard;
}  

/*	Function Name: AddToCurrentSection
 *	Description: This function gets the names of the manual page
 *                   directories, then closes the directory.
 *	Arguments:  local_manual - a pointer to a manual pages structure.
 *                  path - the path to this directory.
 *	Returns: none.
 */

static void
AddToCurrentSection(local_manual, path)
Manual * local_manual;
char * path;
{
  char temp_path[BUFSIZ];

  ReadCurrentSection(local_manual, path);
  sprintf(temp_path, "%s.%s", path, COMPRESSION_EXTENSION);
  ReadCurrentSection(local_manual, temp_path);
}

/*	Function Name: ReadCurrentSection
 *	Description: Actually does the work of adding entries to the 
 *                   new section
 *	Arguments:  local_manual - a pointer to a manual pages structure.
 *                  path - the path to this directory.
 *                  compressed - Is this a compressed directory?
 *	Returns: TRUE if any entries are found.
 */

static void
ReadCurrentSection(local_manual, path)
Manual * local_manual;
char * path;
{
  DIR * dir;

  register struct dirent *dp;
  register int nentries;
  register int nalloc;
  char full_name[BUFSIZ], *ptr;

  if((dir = opendir(path)) == NULL) {	
#ifdef DEBUG
    sprintf(error_buf,"Can't open directory %s", path);
    PrintWarning(NULL, error_buf);
#endif /* DEBUG */
    return;
  }

/*
 * Remove the compression extension from the path name.
 */

  if ( (ptr = rindex(path, '.')) != NULL) 
    if (streq(ptr + 1, COMPRESSION_EXTENSION)) 
      *ptr = '\0';
  
  nentries = local_manual->nentries;
  nalloc = local_manual->nalloc;

  while( (dp = readdir(dir)) != NULL ) {
    char * name = dp->d_name;
    if( (name[0] == '.') || (index(name, '.') == NULL) ) 
      continue;
    if( nentries >= nalloc ) {
      nalloc += ENTRYALLOC;
      local_manual->entries =(char **) OlRealloc((char *)local_manual->entries,
						 nalloc * sizeof(char *));
    }

    sprintf(full_name, "%s/%s", path, name);
/*
 * Remove the compression extension from the entry name.
 */

    if ( (ptr = rindex(full_name, '.')) != NULL) 
      if (streq(ptr + 1, COMPRESSION_EXTENSION)) 
	*ptr = '\0';

    local_manual->entries[nentries++] = StrAlloc(full_name);
  }

  local_manual->nentries = nentries;
  local_manual->nalloc = nalloc;

  closedir(dir);
}

/*	Function Name: SortAndRemove
 *	Description: This function sorts all the entry names and
 *                   then removes all the duplicate entries.
 *	Arguments: man - a pointer to the manual structure.
 *                 number - the number of manual sections.
 *	Returns: an improved manual stucure
 */

static void
SortAndRemove(man, number)
Manual *man;
int number;
{
  int i;
  char *l1, *l2;

  for ( i = 0; i < number; man++, i++) { /* sort each section */
    register int j = 0;      

#ifdef DEBUG
  printf("sorting section %d - %s\n", i, man->blabel);
#endif /* DEBUG */

    qsort(man->entries, man->nentries, sizeof( char * ), CmpEntryLabel);

#ifdef DEBUG
    printf("removing from section %d.\n", i);
#endif /* DEBUG */

    if ( (l1 = rindex(man->entries[j], '/')) == NULL)
      PrintError("Internal error while removing duplicate manual pages.");
    j++;

    while (j < (man->nentries - 1) ) {
      l2 = l1;
      if ( (l1 = rindex(man->entries[j], '/')) == NULL)
	PrintError("Internal error while removing duplicate manual pages.");
      if ( streq(l1, l2) ) {
	register int k;
	for( k = j; k < (man->nentries); k++)
	  man->entries[k - 1] = man->entries[k];
	(man->nentries)--;
      }
      else
	j++;
    }
  }
}

/*	Function Name: CmpEntryLabel - used in qsort().
 *	Description: compares to elements by using their labels.
 *	Arguments: e1, e2 - two items to compare.
 *	Returns: an integer >, < or = 0.
 */

static int 
CmpEntryLabel(e1, e2) 
char **e1, **e2;
{
  char *l1, *l2;

/*
 * What we really want to compare is the actual names of the manual pages,
 * and not the full path names.
 */

  if ( (l1 = rindex(*e1, '/')) == NULL)
    PrintError("Internal error while sorting manual pages.");
  if ( (l2 = rindex(*e2, '/')) == NULL)
    PrintError("Internal error while sorting manual pages.");
  return( strcmp(l1, l2) );
}

/*	Function Name: InitManual
 *	Description: Initializes this manual section.
 *	Arguments: l_manual - local copy of the manual structure.
 *                 label - the button label for this section.
 *	Returns: none.
 */

static void
InitManual(l_manual, label)
Manual * l_manual;
char * label;
{
  bzero( l_manual, sizeof(Manual) );	        /* clear it. */
  l_manual->blabel = label;	                /* set label. */
}
  
#if defined(DEBUG)

/*	Function Name: DumpManual
 *	Description: Debugging function that dumps the entire manual page
 *                   structure.
 *	Arguments: number - the number of sections.
 *	Returns: none.
 */

DumpManual(number)
{
  register int i,j;
  
  for ( i = 0; i < number; i++) {
    printf("label: %s\n", manual[i].blabel);
    for (j = 0; j < manual[i].nentries; j++) 
      printf("%s\n", manual[i].entries[j]);
  }
}

#endif /* DEBUG */
