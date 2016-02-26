
/*  @(#)faces.h 1.3 88/12/09
 *
 *  Contains all the global definitions used by faces.
 *
 *  Copyright (c) Rich Burridge - Sun Microsystems Australia.
 *                                All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged. 
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

#include <stdio.h>
#include <sys/fcntl.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <pwd.h>
#include <strings.h>

#define  FCLOSE        (void) fclose      /* To make lint happy. */
#define  FFLUSH        (void) fflush
#define  FGETS         (void) fgets
#define  FPRINTF       (void) fprintf
#define  FSEEK         (void) fseek
#define  GETHOSTNAME   (void) gethostname
#define  GET_SUN_ICON  (void) get_sun_icon
#define  IOCTL         (void) ioctl
#define  PUTC          (void) putc
#define  SELECT        (void) select
#define  SSCANF        (void) sscanf
#define  SPRINTF       (void) sprintf
#define  STRCAT        (void) strcat
#define  STRCPY        (void) strcpy
#define  STRNCAT       (void) strncat
#define  STRNCPY       (void) strncpy
#define  UNLINK        (void) unlink

/* The types of display. */
enum disp_type { BOTH, ICON, WINDOW } ;

/* Field extraction options. */
enum field_type { HOSTNAME, TIMESTAMP, USERNAME } ;

/* Different types of possible face images. */
enum icon_type { NOMAIL, NOPAPER, NOPRINT, ORDINARY } ;

/* Text justification within face display. */
enum just_type { LEFT, RIGHT } ;

/* Different types of file monitoring performed by this program. */
enum mon_type { MONALL, MONNEW, MONPRINTER } ;

/* Different graphics systems appropriate one set in gtype. */
enum gr_type { SUNVIEW, NEWS } ;

/* Determine order for face type lookup. */
#define  NEWSTYPE        0
#define  SUNTYPE         1
#define  BLITTYPE        2

/* NeWS return event values. */
#define  DIED            100  /* Faces has been zapped. */
#define  PAINTED         101  /* Canvas/Icon needs repainting. */

#define  BLITHEIGHT      48   /* Maximum number of lines in a blit icon. */
#define  EQUAL(str,val)  !strncmp(str,val,strlen(val))
#define  ICONHEIGHT      64   /* Height of individual face icons. */
#define  ICONWIDTH       64   /* Width of individual face icons. */
#define  INC             argc-- ; argv++ ;
#define  MAXLINE         200  /* Maximum length for character strings. */
#define  MAXTYPES        3    /* Maximum number of different face types. */
#define  NO_PER_ROW      10   /* Number of faces per row. */

char *getenv(), *malloc(), *sprintf() ;

struct machinfo                 /* Machine/community record. */
  {
    char *machine ;             /* Machine name. */
    char *community ;           /* Community it belongs to. */
    struct machinfo *next ;     /* Pointer to next record. */
  } ;

struct comminfo                 /* Community alias/username records. */
  {
    char *community ;           /* Community name. */
    struct peopinfo *people ;   /* Chain of alias/usernames. */
    struct comminfo *next ;     /* Pointer to next record. */
  } ;

struct peopinfo                 /* Username/alias record. */
  {
    char *alias ;               /* Alias for this user. */
    char *username ;            /* Real username. */
    struct peopinfo *next ;     /* Pointer to next record. */
  } ;

struct psinfo                   /* News.ps animation records. */
  {
    char *name ;                /* Full pathname of news.ps file. */
    int row ;                   /* Row number where animation will occur. */
    int column ;                /* Column number where animation will occur. */
    struct psinfo *next ;       /* Pointer to next record. */
  } ;

struct recinfo                  /* Mail/print information record. */
  {
    char *community ;           /* Community name for this person. */
    char *iconname ;            /* Name of iconfile for this person. */
    char *username ;            /* User name for this person. */
    int total ;                 /* Total number of messages/print jobs. */
    int size ;                  /* Total size in bytes of print job. */
    char ts[6] ;                /* Latest timestamp for this user. */
    struct recinfo *next ;      /* Pointer to next record. */
  } ;
