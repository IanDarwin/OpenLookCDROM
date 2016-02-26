/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <portability.h>
#include <constants.h>

#ifdef HAS_DIRENT_H
#define SYSV_SYSTEM_DIR
#endif

#ifdef SYSV_SYSTEM_DIR

#include <dirent.h>		/* replaces sys/dir.h in new releases (see man opendir) */
#define DIRENTRY struct dirent
#define NAMLEN(p) strlen (p->d_name)

#else
#include <sys/dir.h>		/* this is currently the one for our CAM suns */
#define DIRENTRY struct direct
#define NAMLEN(p) p->d_namlen
extern DIR *opendir ();
extern struct direct *readdir ();

#endif

#define TRUE 1
#define FALSE 0
#define NULL 0
#define min(a, b) ((a) < (b) ? (a) : (b))

/*
 * filename_completion()
 * Completes file name FILE in directory DIR.
 * Looks at all filenames in DIR that start with FILE.
 * If there is only one and FILE matches it exactly, returns 0.
 * Returns negative error status if DIR contains no name starting with FILE.
 * Returns k>0 if the first k characters of FILE match possible files.
 * FILE returns as best possible completion.
 * Algorithm adapted from GNU EMACS (dired.c) by fjw 8/22/92
 */
filename_completion (file, dirname)
     char *file, *dirname;
{
  DIR *d;
  DIRENTRY *dp;
  int bestmatchsize;
  register int compare, matchsize;
  char *p1, *p2, *getcwd();
  int matchcount = 0;
  char *bestmatch, *name;
  struct stat st;
  int directoryp;
  char *filename_as_directory();
  char defalt_dir[200];

  /* getwd(defalt_dir);	*/	/* make this DSTOOL_DATA_DIR for dstool */
  getcwd(defalt_dir, SIZE_OF_DIR_PLUS_FNAME);
  expand_filename (dirname, defalt_dir);
  bestmatch = NULL;

  directory_filename(dirname);
  if (!(d = opendir (dirname)))	/* error opening directory;  */
      return -3;
  
  /* Don't waste time trying to complete a null string; return success */
  if ( !strlen(file) )
    {
      closedir(d); 
      return 0;
    }

  /* Loop reading blocks; try to come up with a maximum filename match */
  while (1)
    {
      DIRENTRY *dp;
      int len;
      
      dp = readdir (d);	/* move down one tree level */
      
      if (!dp) break;	/* are we at the end? break out. */
      
      len = NAMLEN (dp);
      
      /* put in call to pm INTERRUPT HERE and if TRUE
	 goto quit; */
      
      /* go to next entry if current candidate's name too short or first letters don't agree */
      if (!dp->d_ino	/* file number of entry */
	  || len < strlen(file)
	  || 0 <= scmp(dp->d_name, file, strlen(file) ))
	continue;
      
      /* we have a candidate ...*/
      if (file_name_completion_stat (dirname, dp, &st) < 0)	/* get file status */
	continue;
      
      directoryp = S_ISDIR(st.st_mode); /* is it a directory? (cf man stat) */
      
      /* process this name as a completion */
      /* Update computation of how much all possible completions match */
      
      matchcount++;
      
      if ( !bestmatch )
	{
	  /* This is first possible completion */
	  bestmatch = (char *)calloc(100, sizeof(char)); /* make this SIZE_OF_DIR_PLUS_FNAME */
	  if (directoryp)
	    filename_as_directory (bestmatch, dp->d_name); /* make it end with '/' */
	  else
	    strcpy(bestmatch, dp->d_name); 
	  bestmatchsize = strlen(bestmatch);
	}
      else
	{
	  compare = min (bestmatchsize, len);
	  p1 = bestmatch;
	  p2 = (char *) dp->d_name;
	  matchsize = scmp(p1, p2, compare);
	  if (matchsize < 0)
	    matchsize = compare;
	  /* If this dirname all matches, see if implicit following slash does too.  */
	  if (directoryp
	      && compare == matchsize
	      && bestmatchsize > matchsize
	      && p1[matchsize] == '/')
	    matchsize++;
	  bestmatchsize = min (matchsize, bestmatchsize);
	}
    }
  closedir (d);

  if ( !bestmatch )
    return -2;			/* no possible match */

  if (matchcount == 1 && bestmatchsize == strlen(file))
    {
      strncpy(file, bestmatch, bestmatchsize); 
      file[bestmatchsize] = 0;
      return 0;			/* exact match! */
    }
  strncpy(file, bestmatch, bestmatchsize); 
  file[bestmatchsize] = 0;
  return bestmatchsize;
 quit:
  if (d) closedir (d);
  return -4;
}

/*
 * separate_filename() splits a directory name into the parent and child.
 * Input: dirname = directory name
 * 	  file = pointer to allocated space
 * Upon return file contains the last entry in the dirname tree.
 * We also return the integer k such that dirname[k+1] is the last "/" in dirname.
 * Example: char file[20], dir[20]; int k;
 *          strcpy(dirname, "usr/local/lib");
 *          k = separate_filename(file, dirname);
 * ===>  k = 9 = strlen("usr/local") and file = "lib"
 * fjw 8/23/92
 */
int
separate_filename(file, dirname)
char *file, *dirname;
{
  int i=0,len=strlen(dirname);
  
  while( dirname[len-i] != '/' && i<= len)
    i++;
  if(i>=len)			/* no slashes, ie, ~joe */
    return len;
  i--;				/* went one too far */
  strncpy(file, dirname+len-i, i);
  file[i] = 0;
  return (len-i-1);
}

/* 
 * file_name_completion_stat() calls system function which returns status 
 * of file in *st_addr (see man stat) 
 * Returns 0 if info gotten; -1 on failure (and writes error to errno; cf man errno)
 * Modified from GNU EMACS (dired.c) 8/22/92 by fjw
 */
file_name_completion_stat (dirname, dp, st_addr)
     char *dirname;
     DIRENTRY *dp;
     struct stat *st_addr;
{
  int len = NAMLEN (dp);
  int old_pos = strlen(dirname);
  int i, new_pos;
  char *filename_as_directory();

  char *fullname = (char *) calloc(len + old_pos + 2, sizeof(char));

  filename_as_directory(dirname, dirname);
  new_pos = strlen(dirname);	/* could be old_pos + 1 if ending "/" appended */
  strncpy(fullname, dirname, new_pos);
  strncpy(fullname+new_pos, dp->d_name, len);
  fullname[new_pos + len] = 0;

  i = stat (fullname, st_addr); 

  dirname[old_pos] = 0;		/* make sure that dirname returns unchanged */
  cfree(fullname);
  return i;
}



/* Compare exactly LEN chars of strings at S1 and S2.
 * Return -1 if strings match,
 * else number of chars that match at the beginning. 
 * Modified from GNU EMACS (minibuf.c) 8/22/92 by fjw
 */
scmp (s1, s2, len)
     register char *s1, *s2;
     int len;
{
  register int l = len;

  while (l && *s1++ == *s2++)
    l--;

  if (l == 0)
    return -1;
  else return len - l;
}
