/*      Copyright (c) 1991 UNIX System Laboratories, Inc        */
/*	Copyright (c) 1988 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ident	"@(#)dump:common/dump.h	1.2"

#ifdef __STDC__
#include <stdlib.h>
#include <locale.h>
#endif

#include <libelf.h>
#include <link.h>
#include <sys/types.h>
#ifdef M68K
#include <sys/elf_68K.h>
#else
# ifdef M88K
# include <sys/elf_88K.h>
# else
# include <sys/elf_M32.h>
# include <sys/elf_386.h>
# endif
#endif
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>

#define DATESIZE 60

struct file_list {
	char *name;
	struct file_list *prev;
};

struct resolv_info {
    struct file_list *classheader_list, *getclassinfo_list;
};

typedef struct scntab {
	char             *scn_name;
	Elf32_Shdr       *p_shdr;
	Elf_Scn          *p_sd;
} SCNTAB;

#ifdef __STDC__
#define VOID_P void *
#else
#define VOID_P char *
#endif

#define UCHAR_P unsigned char *

#define FAILURE 0
#define SUCCESS 1
