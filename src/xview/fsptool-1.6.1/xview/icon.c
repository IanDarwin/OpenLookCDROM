/********************************************************************************/
/* icon.c --									*/
/*										*/
/* (c)1993 A.J.Doherty								*/
/********************************************************************************/

#include "fsptool.h"
#include "icon.h"
#include "frame.h"

#include <X11/Xlib.h>

/********************************************************************************/

#include "../icons/Size64/fsptool64"
#include "../icons/Size64/fsptool64_1"
#include "../icons/Size64/fsptool64_2"
#include "../icons/Size64/fsptool64_3"
#include "../icons/Size64/fsptool64_4"
#include "../icons/Size64/fsptool64_5"
#include "../icons/Size64/fsptool64_6"
#include "../icons/Size64/fsptool64_7"
#include "../icons/Size64/fsptool64_8"
#include "../icons/Size64/fsptool64_9"

#include "../icons/compressed"
#include "../icons/c_file"
#include "../icons/h_file"
#include "../icons/directory"
#include "../icons/link"
#include "../icons/tarfile"
#include "../icons/textfile"
#include "../icons/unknown"
#include "../icons/graphic"
#include "../icons/postscript"
#include "../icons/anim"
#include "../icons/audio"

#include "../icons/drop_site_busy"
#include "../icons/drop_site_idle"

/********************************************************************************/

void load_file_glyphs();

Server_image make_64by64(char[]);
Server_image make_48by48(char[]);
Server_image load_server_image(char[]);

/********************************************************************************/

extern Base_frame baseframe;

/********************************************************************************/

Icon            fsptoolicon;

Server_image    fsptool_glyph, drop_busy_glyph, drop_idle_glyph;

static Server_image fileglyphs[NO_FILETYPES+1], busy_glyph[9];

/********************************************************************************/

void load_icons()

/* this routine loads all icons, file type glyph				*/

{
set_frame_icon();

fsptool_glyph  = make_64by64(fsptool64_bits);
busy_glyph[0]  = make_64by64(fsptool64_1_bits);
busy_glyph[1]  = make_64by64(fsptool64_2_bits);
busy_glyph[2]  = make_64by64(fsptool64_3_bits);
busy_glyph[3]  = make_64by64(fsptool64_4_bits);
busy_glyph[4]  = make_64by64(fsptool64_5_bits);
busy_glyph[5]  = make_64by64(fsptool64_6_bits);
busy_glyph[6]  = make_64by64(fsptool64_7_bits);
busy_glyph[7]  = make_64by64(fsptool64_8_bits);
busy_glyph[8]  = make_64by64(fsptool64_9_bits);

drop_busy_glyph = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
				XV_WIDTH,		15,
				XV_HEIGHT,		20,
				SERVER_IMAGE_X_BITS,	drop_site_busy_bits,
				NULL);

drop_idle_glyph = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
				XV_WIDTH,		15,
				XV_HEIGHT,		20,
				SERVER_IMAGE_X_BITS,	drop_site_idle_bits,
				NULL);

load_file_glyphs();
}

/********************************************************************************/

Server_image make_32by32 ( char bits[] )

{
return((Server_image) xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH,		32,
		XV_HEIGHT,		32,
		SERVER_IMAGE_X_BITS,	bits,
		NULL));
}

/********************************************************************************/

Server_image make_64by64 ( char bits[] )

{
return((Server_image) xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH,		64,
		XV_HEIGHT,		64,
		SERVER_IMAGE_X_BITS,	bits,
		NULL));
}

/********************************************************************************/

void set_frame_icon()

/* create the application icon, according to size hints received from the	*/
/* Window Manager - if any.							*/

{ Rect  	image_rect, label_rect;
  Server_image	image;
  int		count;
  XIconSize    *size_hints = XAllocIconSize();

  Display      *dpy  = (Display*) xv_get(xv_default_server,XV_DISPLAY);
  Window	root = (Window) xv_get(xv_get(xv_default_screen,XV_ROOT),XV_XID);


if (XGetIconSizes(dpy,root,&size_hints,&count) == 0) {
    }
else {
    }

rect_construct(&image_rect,0,0,64,48);
rect_construct(&label_rect,0,48,64,64);

image = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
        XV_WIDTH,               64,
        XV_HEIGHT,              64,
        SERVER_IMAGE_X_BITS,	fsptool64_bits,
        NULL);

fsptoolicon = (Icon) xv_create(XV_NULL, ICON,
	XV_WIDTH,		64,
	XV_HEIGHT,		64,
	ICON_TRANSPARENT,	TRUE,
	ICON_LABEL_RECT,	&label_rect,
        ICON_IMAGE,             image,
	ICON_MASK_IMAGE,	image,
	ICON_IMAGE_RECT,	&image_rect,
	ICON_LABEL,		"FSPtool",
	XV_X,			64,
	XV_Y,			64,
        NULL);

XFree((caddr_t) size_hints);
}

/********************************************************************************/

void load_file_glyphs()

{
fileglyphs[0]		= load_server_image(unknown_bits);
fileglyphs[DIRECTORY]	= load_server_image(directory_bits);
fileglyphs[FILE_LINK]	= load_server_image(link_bits);
fileglyphs[TAR_FILE]	= load_server_image(tarfile_bits);
fileglyphs[Z_FILE]	= load_server_image(compressed_bits);
fileglyphs[z_FILE]	= fileglyphs[Z_FILE];
fileglyphs[ZIP_FILE]	= fileglyphs[Z_FILE];
fileglyphs[TEXT_FILE]	= load_server_image(textfile_bits);
fileglyphs[H_FILE]	= load_server_image(h_file_bits);
fileglyphs[C_FILE]	= load_server_image(c_file_bits);
fileglyphs[GIF_FILE]	= load_server_image(graphic_bits);
fileglyphs[PBM_FILE]	= fileglyphs[GIF_FILE];
fileglyphs[X11_FILE]	= fileglyphs[GIF_FILE];
fileglyphs[RAS_FILE]	= fileglyphs[GIF_FILE];
fileglyphs[PS_FILE]	= load_server_image(postscript_bits);
fileglyphs[JPEG_FILE]	= fileglyphs[GIF_FILE];
fileglyphs[TIFF_FILE]	= fileglyphs[GIF_FILE];
fileglyphs[MPG_FILE]	= load_server_image(anim_bits);
fileglyphs[GL_FILE]	= fileglyphs[MPG_FILE];
fileglyphs[RLE_FILE]	= fileglyphs[GIF_FILE];
fileglyphs[AU_FILE]	= load_server_image(audio_bits);
fileglyphs[UNKNOWN]	= fileglyphs[0];
}

/********************************************************************************/

Server_image load_server_image ( char bits[] )

{
return((Server_image) xv_create(XV_NULL, SERVER_IMAGE,
				XV_WIDTH, 		16,
				XV_HEIGHT,		16,
				SERVER_IMAGE_X_BITS,	bits,
				NULL));
}

/********************************************************************************/

Server_image return_file_glyph ( int filetype )

/* this function returns the appropriate file glyph for the specified filetype	*/

{
return(fileglyphs[filetype]);
}

/********************************************************************************/

void set_icon ( int active, int total_done, int total_size )

/* this fn sets the fsptool icon, if active is FALSE then icon is set to normal	*/
/* fsptool images, if active is UPLOAD or DOWNLOAD then the appropriate image	*/
/* is used from the animation sequence.	Sets label as percentage completed	*/

{ static int count = 0, percent;
  static char buffer[10];

    Icon tool_icon = (Icon) xv_get(baseframe.frame, FRAME_ICON);
  
       float total = (float) total_size,
	     done  = (float) total_done;

percent = (int) ((done/total)*100.0);

sprintf(buffer," %02d%%",(percent>100 ? 100:percent));

switch (active)
    {
    case UPLOAD:
	count++;

	if (count == 9)
	    count = 0;

	break;

    case DOWNLOAD:
	count--;

	if (count < 0)
	    count = 8;

	break;

    default:
	xv_set(tool_icon,
		ICON_IMAGE,	 fsptool_glyph,
		ICON_MASK_IMAGE, fsptool_glyph,
		ICON_LABEL,	 "FSPtool",
		NULL);

	count = 0;
	return;
    }

xv_set(tool_icon,
	ICON_IMAGE,	 busy_glyph[count],
	ICON_MASK_IMAGE, busy_glyph[count],
	ICON_LABEL,	 buffer,
	NULL);
}

/********************************************************************************/
