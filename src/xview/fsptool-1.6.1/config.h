/********************************************************************************/
/*
 config.h (03/10/93) --

 These are compiled in defaults. Can be over-ridden by resource database.
 
 This file contains any stuff which you may need to change to get fsptool to make
 on your system. Basically the names of the fget, fls, fcat and fver fsp commands
 on your local system. If the paths to all the commands shown here are present in
 the variable path then you can just specify the names and they will work. If not
 then specify the full pathname starting at root eg: "/bin/ls" instead of "ls".

 The default actions to take when encountering certain file types are also given
 below, you can change these if you like but ensure that they are going to work,
 eg: the given executable file is available to the person using the program at run
 time.

 The majority of these are simply supplied defaults which the program will use if
 the ~/.fsptooldefaults file does not exist in the user's home directory, or if
 alternatives have not been set for the resource in question. For example if the
 user has the resource "Fsptool.Exec.Gif: xli" in the database file then this
 overrides the compiled in default from this file. Bearing in mind that the
 user is unlikely to have a defaults file initally, it's a good idea for all of
 the below to be set to something that will work.

 NOTE: All of the commands which the FSPtool application runs are run using
 exevcp, so that the user's path will be searched for the appropriate program. If
 you don't think the user will have the appropriate dirs in their path, or you
 want them to run a specific copy use an absolute pathname in the define
 statements below.

*/
/********************************************************************************/

#ifndef _FSPtool_CONFIG_H_
#define _FSPtool_CONFIG_H_ 1

/********************************************************************************/
/* Names and full pathnames for FSP cmds change the pathnames as needed.	*/

#define FSP_VER_CMD		"fver"
#define FSP_GET_CMD		"fgetcmd"
#define FSP_LS_CMD		"flscmd"
#define FSP_PUT_CMD		"fput"

/********************************************************************************/
/* Names and full pathnames for decompression utilities -- should cope with	*/
/* current compressors. Changes as needed. A recent version of gzip ie: 1.2.4	*/
/* is prefered as this copes with both .Z , .z/.gz and certain versions of .zip	*/
/* files.									*/

#define UNIX_UNCOMPRESS_CMD	"uncompress"			/* .Z files	*/
#define UNIX_GZIP_CMD		"gzip"				/* .gz/.z files	*/

/* if you don't have gzip then change this to HAS_GZIP	FALSE */

#define HAS_GZIP		TRUE

/********************************************************************************/
/* File actions for specified filetypes, these are the default actions, they	*/
/* are overridden if user specified options exist in the ~/.fsptooldefaults	*/
/* resource file. File actions can be changed using File Actions menu item.	*/

#define ACTION_DEFAULT	"textedit %f"
#define ACTION_TEXT	"textedit %f"
#define ACTION_H	"textedit %f"
#define ACTION_C	"textedit %f"
#define ACTION_GIF	"xv %f"
#define ACTION_PBM	"xv %f"
#define ACTION_X11	"iconedit %f"
#define ACTION_RAS	"xv %f"
#define ACTION_PS	"pageview %f"
#define ACTION_JPEG	"xv %f"
#define ACTION_TIFF	"xv %f"
#define ACTION_MPG	"xmpg %f"
#define ACTION_GL	"xviewgl %f"
#define ACTION_RLE	"xv %f"
#define ACTION_AU	"audiotool %f"

/*********************************************************************************/
#endif

