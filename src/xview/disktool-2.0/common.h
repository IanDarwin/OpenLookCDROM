/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Questions, bug reports, modifications, etc can be sent to the following
address:

	sfinn@astro.ge.com

ALL RIGHTS ARE RESERVED, EXCEPT FOR THE FOLLOWING:

ANYONE CAN MODIFY `disktool' PROVIDED THEY CONTACT THE AUTHOR FIRST AND
REDISTRIBUTE THE MODIFIED VERSION FREE OF CHARGE AND DO NOT OMIT THIS
HEADER FROM THE DISTRIBUTION.

THERE IS NO WARRANTY FOR `disktool'.  IT IS PROVIDED AS IS.  I HAVE NO 
RESPONSIBILITY FOR ANY PROBLEMS OR DAMAGES CAUSED BY USE OF `disktool'.

++++++++++++++++ Defines +++++++++++++++++++++++++++++++++++++++++++++++++++*/
#define max(a,b)   (((a) > (b)) ? (a) : (b))
#define FALSE 0
#define TRUE 1
#define MAX_GAUGES 64
#define WHITE 0
#define RED   1
#define GREEN 2
#define BLACK 3
#define NUM_COLORS 4
#define DT_LABEL "Disktool 2.0"
#define OKCOLOR GREEN         /* Change this to any of the above colors     */
#define CRITCOL RED           /* Change this to any of the above colors     */
#define MINSPACING 50         /* Min. spacing value allowed before autocalc */
#define MIN_POLL_INT 5        /* Min. polling interval (seconds)            */
#define MAX_POLL_INT 3600     /* Max. polling interval (seconds)            */
/*++++++++++++++ Includes ++++++++++++++++++++++++++++++++++++++++++++++++++*/
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/vfs.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/notice.h>
#include <xview/notify.h>
#include <xview/svrimage.h>
#include <xview/icon.h>
#include <xview/rect.h>
#include <xview/cms.h>
#include <X11/Xos.h>
#ifndef MAXPATHLEN
#include <sys/param.h>
#endif /* MAXPATHLEN */
#ifndef MAXHOSTNAMELEN
#include <netdb.h>
#endif /* MAXHOSTNAMELEN */
#include "dt_ok.icon"
#include "dt_crit.icon"
#include "dt_mask.icon"
/*++++++++++++++ Global Variables ++++++++++++++++++++++++++++++++++++++++++*/
Frame       frame;
Frame       prop_frame;
Icon        icon;
Panel       panel;
Cms         cms;
Panel_item  gauge[MAX_GAUGES];
Panel_item  part_item[MAX_GAUGES];
Panel_item  kbyte_item[MAX_GAUGES];
Panel_item  delta_item[MAX_GAUGES];
Panel_item  thresh_item;
Panel_item  units_item;
Panel_item  inc_item;
Panel_item  disk_item;
Panel_item  cmd_item;
Panel_item  pop_check;
Panel_item  orient_item;
Panel_item  roc_item;
Panel_item  spacing_item;
Panel_item  rep_item;
Server_image  image_ok;
Server_image  image_crit;
Server_image  image_mask;
struct itimerval  timer;
int         debug;
int         delta;
int         used;
int         fscnt;
int         curr_item;
int         critcnt[MAX_GAUGES];
int         divisor[MAX_GAUGES];
long        thresh[MAX_GAUGES];
char        poll_str[24];
/*+++++++++++++++++++++++++ End of common.h ++++++++++++++++++++++++++++++++*/
