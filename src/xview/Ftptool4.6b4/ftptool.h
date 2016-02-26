/*
 * Ftptool - written by Mike Sullivan
 *
 * The more I add, the nastier it gets. I used to _like_ this code,
 * and I usually _hate_ using globals.
 */

#ifdef hpux
#define ident title
#endif

#pragma ident   "@(#)ftptool.h 1.8     93/08/19"

#include <stdio.h>
#include <errno.h>
#include <math.h>

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xresource.h>

#ifndef LINT
#include <unistd.h>
#endif

#include <sys/param.h>
#include <sys/stat.h>

#include <termios.h>
#include <signal.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>

#ifdef hpux
#undef SYSV
#endif

#if defined(SYSV) || defined(SYSV386)
#include <sys/statvfs.h>
#else
#ifdef ultrix
#include <sys/mount.h>
#include <nfs/nfs_clnt.h>
#include <nfs/vfs.h>
#else
#ifdef AIXV3
#include <sys/statfs.h>
#else
#include <sys/vfs.h>
#endif
#endif
#endif

#include <arpa/ftp.h>
#include <arpa/telnet.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <malloc.h>
#include <dirent.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/notice.h>
#include <xview/textsw.h>
#include <xview/font.h>
#include <xview/canvas.h>
#include <xview/xv_xrect.h>
#include <xview/cursor.h>
#include <xview/defaults.h>
#include <pwd.h>
#include <grp.h>
#ifdef notdef
#include <setjmp.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#if !defined(XVIEW2) && !defined(XVIEW3)
#	ifdef NOTICE
#		define XVIEW3
#	else
#		define XVIEW2
#	endif
#endif

#if defined(XVIEW2)
#undef XVIEW3
#endif

#if defined(XVIEW3)
#undef XVIEW2
#endif

#ifndef USE_PROTOTYPES
extern time_t time();
#endif

#include "config.h"

#ifdef XVIEW3
#include <xview/dragdrop.h>
#endif

#ifdef USE_PROTOTYPES
#include <stdarg.h>
#else
#include <varargs.h>
#endif

struct file_property_window; /* for prototypes */

#include "batch_struct.h"
#include "dircache_struct.h"
#include "dirlist_struct.h"
#include "host_list_struct.h"
#include "schedule_struct.h"
#include "view_file_struct.h"

#include "batch.h"
#include "button_funcs.h"
#include "change_dir.h"
#include "create_main.h"
#include "create_other.h"
#include "dircache.h"
#include "dirlist.h"
#include "dofuncs.h"
#include "dnd.h"
#include "event.h"
#include "ftp_var.h"
#include "host_list.h"
#include "main.h"
#include "menu_funcs.h"
#include "misc.h"
#include "patchlevel.h"
#include "readdir.h"
#include "schedule.h"
#include "tar_view.h"
#include "transfer.h"
#include "view_file.h"

extern char *getwd();

#ifndef EXTERN
#define	EXTERN extern
#endif

/*
 * Global object definitions.
 */

EXTERN Icon	frame_icon;
EXTERN Display *dpy;
EXTERN int	display_width;
EXTERN int	display_height;

#define	MAXGEOMETRYSIZE 20

EXTERN struct dircache local_dircache;
EXTERN struct dircache remote_dircache;
EXTERN int dircache_size;
EXTERN char *which_remote_file;
EXTERN mode_t which_remote_mode;
EXTERN size_t which_remote_size;

EXTERN struct schedule schedule_list;

struct base_window {
	Frame	frame;
	Panel	panel;
	Panel_item file;
	Panel_item view;
	Panel_item props;
	Panel_item connect;
	Panel_item abort;
	Panel_item quit;	/* for non-OPEN LOOK window managers */
	Panel_item directory;
	Panel_item list;
#ifdef XVIEW3
	Xv_drop_site	drop_site;
	Xv_drag_drop	dnd;
	Selection_requestor	sel;
#endif
	char	geometry[MAXGEOMETRYSIZE];
};

EXTERN struct base_window base_window;

struct host_window {
	Frame	frame;
	Panel	panel;
	Panel_item	new;
	Panel_item	anonymous;
	Panel_item	hosts;
	Panel_item	host_list_ops;
	struct {
		Panel	panel;
		Panel_item	list;
		Panel_item	host;
		Panel_item	login;
		Panel_item	password;
		Panel_item	account;
		/* for non-OPEN LOOK window managers */
		Panel_item	dismiss;
		Panel_item  connect;
		Panel_item	plus;
	} basic;
	struct {
		Panel	panel;
		Panel_item	alias;
		Panel_item	last_visited;
		Panel_item	comment;
		Panel_item	os_type;
		Panel_item	dir_parse;
		Panel_item	proxy;
		Panel_item	transfer_mode;
		Panel_item	remote_auto_cd;
		Panel_item	local_auto_cd;
		Panel_item	minus;
		int		visible;
	} advanced;
	int		visible;
	char	geometry[MAXGEOMETRYSIZE];
};

EXTERN struct host_window host_window;

struct local_window {
	Frame	frame;
	Panel	panel;
	Panel_item	directory;
	Panel_item	space;
	Panel_item	list;
	Panel_item	dismiss;	/* for non-OPEN LOOK window managers */
#ifdef XVIEW3
	Xv_drop_site	drop_site;
	Xv_drag_drop	dnd;
	Selection_requestor	sel;
#endif
	int		visible;
	char	geometry[MAXGEOMETRYSIZE];
};

EXTERN struct local_window local_window;

struct status_window {
	Frame	frame;
	Panel	panel;
	Panel_item message;
	Panel_item size;
#ifdef notdef
	Panel_item file_gauge;
#endif
	Panel_item total_gauge;
	Panel_item dismiss;	/* for non-OPEN LOOK window managers */
	int		visible;
	char	geometry[MAXGEOMETRYSIZE];
};

EXTERN struct status_window status_window;

struct session_window {
	Frame	frame;
	Panel	panel;
	Textsw	log;
	char	geometry[MAXGEOMETRYSIZE];
};

EXTERN struct session_window session_window;

struct tool_property_window {
	Frame	frame;
	Panel	panel;
	Textsw	log;
	Panel_item category;
	struct {
		Panel panel;
		Panel_item options;
		Panel_item unique;
		Panel_item automatic;
		Panel_item openlook;
		Panel_item anonftp;
		Panel_item confirm;
		Panel_item ignore_case;
		Panel_item apply;
	} ftptool;
	struct {
		Panel panel;
		Panel_item cache;
		Panel_item cache_inf;
		Panel_item remote_sort;
		Panel_item remote_sortdir;
		Panel_item remote_dotfiles;
		Panel_item remote_group;
		Panel_item local_sort;
		Panel_item local_sortdir;
		Panel_item local_group;
		Panel_item local_dotfiles;
		Panel_item apply;
	} directory_lists;
	struct {
		Panel panel;
		Panel_item viewer;
		Panel_item compressor;
		Panel_item list;
		Panel_item add;
		Panel_item delete;
		Panel_item change;
		Panel_item extension;
		Panel_item magic;
		Panel_item program;
		Panel_item type;
		Panel_item apply;
	} viewers;
	Panel_button_item apply;
};

EXTERN struct tool_property_window tool_property_window;

struct file_property_window	{
	Frame	frame;
	Panel	panel;
	Panel_item filename;
	Panel_item owner;
	Panel_item group;
	Panel_item modtime;
	Panel_item size;
	Panel_item type;
	Panel_item perms_message;
	Panel_item user_perms;
	Panel_item group_perms;
	Panel_item other_perms;
	/* for non-OPEN LOOK window managers */
	Panel_item dismiss;
};

EXTERN struct file_property_window local_file_properties;
EXTERN struct file_property_window remote_file_properties;

struct about_window {
	Frame	frame;
	Panel	panel;
	Textsw	message;
	Panel_item	bottom_panel;
	Panel_item	mail;
	/* for non-OPEN LOOK window managers */
	Panel_item	dismiss;
	char	geometry[MAXGEOMETRYSIZE];
};

EXTERN struct about_window about_window;

struct feedback_window {
	Frame	frame;
	Panel	panel;
	Panel_item	which;
	Panel_item	other;
	Textsw	feedback;
	Panel_item	bottom_panel;
	Panel_item	send;
	Panel_item	cancel;
};

EXTERN struct feedback_window feedback_window;

struct schedule_window {
	Frame	frame;
	Panel	panel;
	Panel_item	current_time;
	Panel_item	process;
	Panel_item	hosts;
	Panel_item	options;
	Panel_item	direction;
	Panel_item	menu_name;
	Panel_item	hour;
	Panel_item	minute;
	Panel_item	month;
	Panel_item	day;
	Panel_item	year;
	Panel_item	repeat;
	Panel_item	repeat_minutes;
	Panel_item	repeat_message;
	Panel_item	send_list;
	Panel_item	receive_list;
	/* for non-OPEN LOOK window managers */
	Panel_item	dismiss;
	Frame	lsframe;
	Panel_item	filename;
	Panel_item	lsbutton;
	int		visible;
	char	geometry[MAXGEOMETRYSIZE];
};

EXTERN struct schedule_window schedule_window;

EXTERN Frame	tar_frame;
EXTERN Panel_item	tar_text;
EXTERN Panel_item	tar_button;
/* for non-OPEN LOOK window managers */
EXTERN Panel_item	tar_quit_button;

EXTERN Xv_font	list_font;
EXTERN Xv_font	bold_list_font;

EXTERN Xv_Cursor	busy_cursor;
EXTERN Xv_Cursor	normal_cursor;

EXTERN char *program_name;
EXTERN char *header_name;
EXTERN char *list_label;

#define	MAXLINE 1024

EXTERN char	response_line[MAXLINE];

EXTERN char	scratch[MAXPATHLEN + MAXNAMLEN + 10];
EXTERN char	myhostname[MAXHOSTNAMELEN + 1];

EXTERN int connected;
EXTERN int which_up_cmd;
EXTERN char icon_label[9];
EXTERN int dowhat;
EXTERN int batch_mode;

EXTERN int local_list_nfiles;
EXTERN int local_list_ndirs;
EXTERN int local_list_nothers;
EXTERN int remote_list_nfiles;
EXTERN int remote_list_ndirs;
EXTERN int remote_list_nothers;

EXTERN int		timedout;
EXTERN int		list_changed;
EXTERN int		extensions_changed;
EXTERN int		timestamped;
EXTERN int		nhostlist_items;
EXTERN int		nsenditems;
EXTERN int		nreceiveitems;
EXTERN int		current_year;
EXTERN int		current_month;
EXTERN int		is_dst;

/* Properties */
EXTERN int		logging;
EXTERN int		keepalive;
EXTERN int		ignore_case;
EXTERN int		unique_local_names;
EXTERN int		unique_remote_names;
EXTERN int		remote_showdotfiles;
EXTERN int		local_showdotfiles;
EXTERN int		confirmdeletes;
EXTERN int		confirmoverwrites;
EXTERN int		remote_sort_mode;
EXTERN int		remote_sort_direction;
EXTERN int		group_remote_files;
EXTERN int		local_sort_mode;
EXTERN int		local_sort_direction;
EXTERN int		group_local_files;
EXTERN char		*default_viewer;
EXTERN char		*default_compressor;
EXTERN char		*archive_viewer;
EXTERN char		*postscript_viewer;

EXTERN double	click_timeout;
EXTERN int		drag_threshold;
EXTERN int nbusyicons;
EXTERN Server_image busy_glyphs[];
EXTERN Server_image busy_glyphs[];

EXTERN Server_image directory_glyph;
EXTERN Server_image file_glyph;
EXTERN Server_image dotdot_glyph;
EXTERN Server_image link_glyph;
EXTERN Server_image unknown_glyph;

EXTERN unsigned short icon_array[];

EXTERN Server_image ftptool_glyph;

EXTERN struct hostlist *hostlist_head;

EXTERN struct extension_info *extension_list;

EXTERN FILE	*commandfp;
EXTERN FILE	*responsefp;

EXTERN char	*anonftp_password;
EXTERN char	*login_name;

EXTERN int	ftp_port;
EXTERN int	ftp_passthru_port;
EXTERN int	try_proxy;
EXTERN int	auto_connect;

EXTERN int	abort_transfer;

EXTERN int openlook_mode;
EXTERN int show_status;

EXTERN int non_unix;

EXTERN char *netrc_filename;

EXTERN char *other_dir_pattern;
EXTERN char *unix_dir_pattern;

EXTERN int	remote_os_type;

/*
 * FTP externs
 */
EXTERN struct	sockaddr_in hisctladdr;
EXTERN struct	sockaddr_in data_addr;
EXTERN struct sockaddr_in myctladdr;
EXTERN int	data;
EXTERN int	abrtflag;
EXTERN int	verbose;
EXTERN int	debug;
EXTERN int	code;
EXTERN int	cpend;
EXTERN int	curtype;
EXTERN int	crflag;
EXTERN int	runique;
EXTERN int	sendport;
