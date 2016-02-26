/*
 * Copyright (c) 1993 Rob Kooper (kooper@cc.gatech.edu)
 * All rights reserved.
 */

/*
 * This file will contain all recognized file formats.
 */

/*
 * What programs should I use to view the files.
 * These should be changed to point to a correct program!
 */
#if defined(OLD)
#define PICTURE_PROG	"xv"
#define TAR_PROG	"tar -tvf"
#define TEXT_PROG	"builtin"
#define PS_PROG		"xpsview"
#define AUDIO_PROG	"sfplay"
#else
#define PICTURE_PROG	"xv"
#define TAR_PROG	"tar -tvf"
#define TEXT_PROG	"builtin"
#define PS_PROG		"ghostview"
#define AUDIO_PROG	"showaudio"
#endif

/*
 * What numbers can I return.
 */
#define NONE		0
#define TEXT		1
#define PICTURE		2
#define PS		3
#define AUDIO		4
#define TAR		5

/*
 * Following defines a structure with all recognized formats. This structure
 * has the following fields:
 *  extension	    - This is the usual extension.
 *  offset	    - offset for the magic number.
 *  magic string    - The first couple of bytes could either form a string
 *  magic number    -  or they could form a magic number.
 *  return number   - What program should be used to view.
 */
struct _file_format {
    char *extension;
    long  offset;
    char *magic_string;
    int   magic_number;
    int   return_type;
};

struct _viewers {
    int	    type;
    char    *command;
};


extern struct _viewers viewers[];
/*
 * And we need a prototype offcourse.
 */
void view();
