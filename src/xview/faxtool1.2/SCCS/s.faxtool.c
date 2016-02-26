h11053
s 00072/00002/01095
d D 1.4 93/06/27 23:54:22 corson 4 3
c [twc 6/27/93] Updated for V1.2.
c 
c 	Added support for FAXSERVER environment variable.  Fax Host
c 	field on Properties popup now defaults to the value of
c 	FAXSERVER.  Send button processing now sets the sendfax -h
c 	option to the value stored in Fax Host.
c 
c 	Fixed bug - Special characters not escaped before calling faxcover.
c 	Modified validate_fax to use new function escape_string to
c 	escape '"', "<", ">", and "`".
c 
c 	Fixed bug - Clear button does not reactivate "Add To Directory"
c 	button.
c 
c 	Fixed bug - "Add To Directory" button does not set textfields
c 	on directory popup.
c 
c 	Fixed bug - Adding duplicate Fax Directory entry caused core
c 	dump the next time the scrolling list was selected.
c 
c 
e
s 00018/00005/01079
d D 1.3 93/06/14 11:40:04 corson 3 2
c [twc 6/14/93] 
c Modified list formatting of the receive queue to accomodate FlexFax2.2.
c 
c Changed format of receive queue list and added sort by date received.
c 
c Fixed bug: strings not terminated after strncpy.
e
s 00775/00019/00309
d D 1.2 93/05/29 18:16:50 corson 2 1
c [twc 5/29/93]
c 
c Added a Fax Directory popup for maintaining and selecting
c Names and Fax Numbers.
c 
c Added a Properties popup for specifing Fax Host, Spool
c Directory, and filters for Viewing, Printing, and Previewing faxes.
c 
c Added fax id (filename) to Receive Queue display.
c 
c Added capability to View, Print and Delete received faxes.
c 
c All panels with scrolling lists are now resizeable and the
c scrolling lists expand/contract accordingly.  Similarly,
c FaxTool now lays out properly when invoked using the -scale
c Xview command line option.
c 
c Misc. bug fixes.
e
s 00328/00000/00000
d D 1.1 93/05/02 22:52:55 corson 1 0
c [twc 5/2/93] Initial Release (Alpha 1.0)
e
u
U
f e 0
t
T
I 1
/*
 * Copyright (c) 1993 Thomas W. Corson
 * Copyright (c) 1993 VetMark Systems, Inc. d.b.a Information Dynamics
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Thomas W. Corson, VetMark Systems, or Information Dynamics may not be used 
 * in any advertising or publicity relating to the software without the 
 * specific, prior written permission of Thomas W. Corson and VetMark Systems.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 * 
 * IN NO EVENT SHALL THOMAS W. CORSON OR VETMARK SYSTEMS, INC. BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#include <stdio.h>
I 2
#include <string.h>
E 2
#include <errno.h>
D 2
#include <signal.h>
E 2
#include <unistd.h>
D 2
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/resource.h>
E 2
I 2
#include <dirent.h>
#include <sys/dirent.h>
E 2
#include <sys/ioctl.h>
D 2
#include <sys/param.h>
#include <sys/types.h>
E 2
#include <xview/xview.h>
#include <xview/panel.h>
D 2
#include <xview/textsw.h>
E 2
#include <xview/xv_xrect.h>
#include <xview/notice.h>
D 2
#include <xview/notify.h>
E 2
#include <xview/font.h>
I 2
#include <xview/frame.h>
#include <xview/notify.h>
E 2
#include <group.h>
#include <gfm.h>
#include <pwd.h>
#include "faxtool_ui.h"
#include "faxtool.h"

D 2
extern	faxtool_window1_objects	*Faxtool_window1;
extern	gfm_popup_objects	*gfm_popup;
extern	Xv_font			fw_font;
E 2
I 2
extern	faxtool_window1_objects		*Faxtool_window1;
extern	faxtool_prop_popup_objects	*Faxtool_prop_popup;
extern	faxtool_directory_popup_objects	*Faxtool_directory_popup;
extern	gfm_popup_objects		*gfm_popup;
extern	Xv_font				fw_font;
extern	char				*args[];
D 4
extern	defaults 			def;
E 4
I 4
extern	Defaults 			def;
E 4
E 2

char			buf[BUFSIZ];
int			pipe_io[2];
Notify_client		client1 = (Notify_client) 10;

I 2
static boolean
multi_list_group(Panel_item	list,
	    	 Group		top_group);

I 4
static void
escape_string(char	*dest,
	      char	*source,
	      int	length);

E 4
E 2
int
add_file_to_send(gfm_popup_objects	*ip,
		 char			*directory,
		 char			*file)
{
D 2
	char	path[PATHLEN];
E 2
I 2
	char	path[PATHLEN + 1];
E 2
	int	row = (int) xv_get(Faxtool_window1->file_list,
				   PANEL_LIST_NROWS);

	strcpy(path, directory);
	strcat(path, "/");
	strcat(path, file);
	
	xv_set(Faxtool_window1->file_list, 
		PANEL_LIST_INSERT, row, 
		PANEL_LIST_STRING, row,	path, 
		NULL);

	return(GFM_OK);
}

int
D 4
validate_fax(faxtool_window1_objects *ip)
E 4
I 4
validate_fax(faxtool_window1_objects 	*ip,
	     Cover			*cover)
E 4
{
	Xv_notice	notice;
	int		status;

	if ((int) xv_get(ip->cover, PANEL_VALUE)
	 && (strcmp((char *) xv_get(ip->to, PANEL_VALUE), "") == 0)
	 && (strcmp((char *) xv_get(ip->to, PANEL_VALUE), "") == 0))
	{
		notice = xv_create(ip->send_controls, NOTICE,
				   NOTICE_LOCK_SCREEN, FALSE,
				   NOTICE_BLOCK_THREAD, TRUE,
				   NOTICE_MESSAGE_STRINGS, 
					"You must specify either Name or Company to use a cover page.", 
					"Press Continue to Correct.", 
					NULL,
				   NOTICE_BUTTON, "Continue", 101,
				   XV_SHOW, TRUE,
				   NULL);

		xv_destroy(notice);

		return(FALSE);
	}

D 2
	else if (!(int) xv_get(ip->cover, PANEL_VALUE)
	 && !(int) xv_get(ip->file_list, PANEL_LIST_NROWS))
E 2
I 2
	else if (!(int) xv_get(ip->file_list, PANEL_LIST_NROWS))
E 2
	{
		notice = xv_create(ip->send_controls, NOTICE,
				   NOTICE_LOCK_SCREEN, FALSE,
				   NOTICE_BLOCK_THREAD, TRUE,
				   NOTICE_MESSAGE_STRINGS, 
D 2
					"You must specify either a cover page, or files, or both.", 
E 2
I 2
					"You must specify at least one file to send.", 
E 2
					"Press Continue to Correct.", 
					NULL,
				   NOTICE_BUTTON, "Continue", 101,
				   NOTICE_STATUS, &status,
				   XV_SHOW, TRUE,
				   NULL);

		xv_destroy(notice);

		return(FALSE);
	}

	else
	{
I 4

		escape_string(cover->fax_number, 
			      (char *) xv_get(ip->fax, PANEL_VALUE), 
			      NUMLEN);

		escape_string(cover->name, 
			      (char *) xv_get(ip->to, PANEL_VALUE), 
			      TXTLEN);

		escape_string(cover->company, 
			      (char *) xv_get(ip->company, PANEL_VALUE), 
			      TXTLEN);

		escape_string(cover->re, 
			      (char *) xv_get(ip->re, PANEL_VALUE), 
			      TXTLEN);

		escape_string(cover->comments, 
			      (char *) xv_get(ip->comments, PANEL_VALUE), 
			      COMLEN);

E 4
		return(TRUE);
	}
}

I 4
static void
escape_string(char	*dest,
	      char	*source,
	      int	length)
{
	static char	meta[] = {'<', '>', '"', '`'};
	char		*pos,
			*here = source;
	size_t		cnt;

	strcpy(dest, "");

	while ((pos = strpbrk(here, meta)) != (char *) NULL)
	{
		cnt = pos - here;

		strncat(dest, here, cnt);

		here += cnt + 1;
		dest += cnt;

		(*dest++) = '\\';
		(*dest++) = (*pos);
		(*dest) = '\0';

	}

	strncat(dest, here, length);
}

E 4
void
do_command(char 	*argv[],
	   Notify_value	(*notify_fn) (Notify_client, register int))
{
	int 	i, pid;
#ifdef TRACE
	char	**ptr = argv;

	fprintf(stderr, "execvp:");
	
	while ((*ptr))
	{
		fprintf(stderr, " %s", (*ptr++));
	}

	fprintf(stderr, "\n");
#endif
	pipe(&pipe_io);

	switch (pid = fork())
	{
	case -1:

		close(pipe_io[0]);
		close(pipe_io[1]);

		perror("fork failed");

		exit(-1);

	case 0:		/* Child 	*/

		dup2(pipe_io[1], 1);
		dup2(pipe_io[1], 2);

		for (i = getdtablesize(); i > 2; i--)
		{
			close(i);
		}

		for (i = 0; i < NSIG; i++)
		{
			signal(i, SIG_DFL);
		}

		execvp(*argv, argv);

		perror("execvp");

		exit(-1);

	default:	/* Parent	*/

		close(pipe_io[1]);

		break;

	}

	notify_set_input_func(client1, notify_fn, pipe_io[0]);
	notify_set_wait3_func(client1, sig_child_notify, pid);

	xv_set(Faxtool_window1->window1, FRAME_BUSY, TRUE, NULL);
}

Notify_value
read_pipe_to_footer(Notify_client	client,
	  	    register int	fd)
{
	int	bytes, i;

	if (ioctl(fd, FIONREAD, &bytes) == 0)
	{
		while (bytes > 0)
		{
			if ((i = read(fd, buf, sizeof(buf))) > 0)
			{
#ifdef TRACE
				fprintf(stderr, 
					"Reading %d bytes from pipe\n", i);
				write(1, buf, i);
#endif
				bytes =- i;
			}

			buf[i] = '\0';

			xv_set(Faxtool_window1->window1, 
				FRAME_LEFT_FOOTER, buf, 
				NULL);
		}

		notify_set_input_func(client, NOTIFY_FUNC_NULL, fd);

	}

	return(NOTIFY_DONE);
}

Notify_value
read_pipe_to_list(Notify_client	client,
	  	  register int	fd)
{
	int	bytes, i, pos, len;
D 2
	char	line[TXTLEN],
E 2
I 2
	char	line[TXTLEN + 1],
E 2
		*newline;

	if (ioctl(fd, FIONREAD, &bytes) == 0)
	{
		while (bytes > 0)
		{
			if ((i = read(fd, buf, sizeof(buf))) > 0)
			{
#ifdef TRACE
				fprintf(stderr, 
					"Reading %d bytes from pipe\n", i);
				write(1, buf, i);
#endif
				bytes =- i;
			}

			buf[i] = '\0';

			i = pos = 0;

			while ((newline = strchr(&buf[pos], '\n'))
			 != (char *) NULL)
			{
				len = newline - &buf[pos] + 1;
				strncpy(line, &buf[pos], len);
				line[len - 1] = '\0';

				switch(i)
				{
				case 0:

					xv_set(Faxtool_window1->window1, 
						FRAME_LEFT_FOOTER, line, 
						NULL);

					break;

				case 1:
I 2
				case 2:
E 2

					break;

				default:

					xv_set(Faxtool_window1->jobs_list,
D 2
						PANEL_LIST_INSERT, i - 2, 
						PANEL_LIST_FONT, i - 2, fw_font,
						PANEL_LIST_STRING, i - 2, line, 
E 2
I 2
						PANEL_LIST_INSERT, i - 3, 
						PANEL_LIST_FONT, i - 3, fw_font,
						PANEL_LIST_STRING, i - 3, line, 
E 2
						NULL);

					break;

				}

				i++;
				pos += len;
			}
		}

		notify_set_input_func(client, NOTIFY_FUNC_NULL, fd);

	}

	return(NOTIFY_DONE);
}

Notify_value
I 2
read_pipe_to_recv_list(Notify_client	client,
	  	       register int	fd)
{
	int	bytes, i, pos, len;
	char	line[TXTLEN + 1],
		filename[NUMLEN + 1],
		sender[NUMLEN + 1],
		pages[NUMLEN + 1],
		quality[NUMLEN + 1],
		page_size[NUMLEN + 1],
		received[NUMLEN + 1],
		*newline,
		*c_pos;

	if (ioctl(fd, FIONREAD, &bytes) == 0)
	{
		while (bytes > 0)
		{
			if ((i = read(fd, buf, sizeof(buf))) > 0)
			{
#ifdef TRACE
				fprintf(stderr, 
					"Reading %d bytes from pipe\n", i);
				write(1, buf, i);
#endif
				bytes =- i;
			}

			buf[i] = '\0';

			i = pos = 0;

			while ((newline = strchr(&buf[pos], '\n'))
			 != (char *) NULL)
			{
				len = newline - &buf[pos] + 1;
				strncpy(line, &buf[pos], len);
				line[len - 1] = '\0';

				switch(i)
				{
				case 0:

					line[strlen(line) - 1] = '\0';
					strcpy(filename, strrchr(line, '/') + 1);

					break;

				case 1:

					if (c_pos = strchr(line, ':'))
D 3
						strcpy(sender, c_pos + 2);
E 3
I 3
					{
						strncpy(sender, c_pos + 2, 14);
						sender[14] = '\0';
					}

E 3
					else
						strcpy(sender, "");

					break;

				case 2:

					if (c_pos = strchr(line, ':'))
						strcpy(pages, c_pos + 2);
					else
						strcpy(pages, "");

					break;

				case 3:

					if (c_pos = strchr(line, ':'))
						strcpy(quality, c_pos + 2);
					else
						strcpy(quality, "");

					break;

				case 4:

					if (c_pos = strchr(line, ':'))
D 3
						strcpy(page_size, c_pos + 2);
E 3
I 3
					{
						strncpy(page_size, c_pos + 2, 21);
						page_size[21] = '\0';
					}

E 3
					else
						strcpy(page_size, "");

					break;

				case 5:

					if (c_pos = strchr(line, ':'))
I 3
					{
E 3
						strncpy(received, c_pos + 2, 16);
I 3
						received[16] = '\0';
					}

E 3
					else
						strcpy(received, "");

					break;

				default:

					break;

				}

				i++;
				pos += len;
			}
		}

		sprintf(line, 
D 3
			"%9s   %14s   %16s   %5s %12s  %s", 
			filename, sender, received, pages, page_size, quality);
E 3
I 3
			"%16s   %9s   %14s   %5s   %21s  %s", 
			received, filename, sender, pages, page_size, quality);
E 3

		i = (int) xv_get(Faxtool_window1->jobs_list, PANEL_LIST_NROWS);

		xv_set(Faxtool_window1->jobs_list,
			PANEL_LIST_INSERT, i, 
			PANEL_LIST_FONT, i, fw_font,
			PANEL_LIST_STRING, i, line, 
I 3
			PANEL_LIST_SORT, PANEL_REVERSE, 
E 3
			NULL);

		notify_set_input_func(client, NOTIFY_FUNC_NULL, fd);

	}

	return(NOTIFY_DONE);
}

Notify_value
E 2
sig_child_notify(Notify_client	client,
		 int		pid,
		 union wait	*status,
		 struct rusage	*rusage)
{
	if (WIFEXITED(status->w_status))
	{
#ifdef TRACE
		fprintf(stderr, "Child %d exited with status %d\n", 
			pid, status->w_retcode);
#endif
/*		notify_set_input_func(client, NOTIFY_FUNC_NULL,
					(client == client1) ? pipe_io[0] : 0);
*/
		xv_set(Faxtool_window1->window1, FRAME_BUSY, FALSE, NULL);

		if (status->w_retcode)
		{
			xv_set(Faxtool_window1->window1, 
				FRAME_LEFT_FOOTER, "Command Failed!", 
				NULL);
		}

		return(NOTIFY_DONE);
	}

	return(NOTIFY_IGNORED);
}

I 2
void
load_recv_queue(faxtool_window1_objects	*ip)
{
	DIR		*dirp;
	struct dirent	*entry;
/*	static int	flags = O_RDONLY; */
	char		path[PATHLEN + 1],
			recv_queue[PATHLEN + 1],
			pgm[PATHLEN + 1],
			msg[TXTLEN + 1];
	int		file_desc;

	strcpy(recv_queue, def.spool_dir);
	strcat(recv_queue, "/recvq");

	if ((dirp = opendir(recv_queue)) == (DIR *) NULL)
	{
		sprintf(msg, "Cannot read %s", def.spool_dir);

		xv_set(Faxtool_window1->window1, 
			FRAME_LEFT_FOOTER, msg, 
			NULL);

		perror("load_recv_queue");

		return;
	}
	
	xv_set(ip->window1, 
		FRAME_LEFT_FOOTER, "Updating Status...", 
		NULL);


	xv_set(ip->jobs_list,
		PANEL_LIST_TITLE, 
D 3
		"Filename            Sender   Time Received      Pages         Size  Quality", 
E 3
I 3
		"Time Received      Filename            Sender   Pages                    Size  Quality", 
E 3
		NULL);

	sprintf(pgm, "%s/bin/faxinfo", def.spool_dir);
	args[0] = pgm;

	while ((entry = readdir(dirp)) != (struct dirent *) NULL)
	{
		if (strstr(entry->d_name, "fax") != entry->d_name)
			continue;

		sprintf(path, "%s/%s", recv_queue, entry->d_name);
		args[1] = path;

		args[2] = (char *) 0;
		do_command(args, read_pipe_to_recv_list);

/*
		if ((file_desc = open(path, flags)) == -1)
		{
			sprintf(msg, "Cannot open %s", path);

			xv_set(Faxtool_window1->window1, 
				FRAME_LEFT_FOOTER, msg, 
				NULL);

			perror("load_recv_queue");

			return;
		}

		else
		{
			close(file_desc);
		}
*/
	}

	xv_set(ip->window1, FRAME_LEFT_FOOTER, "", NULL);
	
	closedir(dirp);
}

void
layout_objects(Frame frame,
	       Panel controls,
	       Xv_opaque target_objects,
	       Xv_opaque all_objects)

{
	int	xv_x = 20,
		xv_y = 20,
	       	new_width,
		new_height,
	       	orig_width,
		orig_height;

	group_layout(target_objects);

	new_width = (2 * xv_x) + (int) xv_get(all_objects, XV_WIDTH);

	if ((orig_width = (int) xv_get(controls, XV_WIDTH)) != new_width)
	{
#ifdef TRACE
		fprintf(stderr, "Changing panel width from %d to %d\n", 
			orig_width, new_width);
#endif
		xv_set(controls, XV_WIDTH, new_width, NULL);
	}

	new_height = (2 * xv_y) + (int) xv_get(all_objects, XV_HEIGHT);

	if ((orig_height = (int) xv_get(controls, XV_HEIGHT)) != new_height)
	{
#ifdef TRACE
		fprintf(stderr, "Changing panel height from %d to %d\n", 
			orig_height, new_height);
#endif
		xv_set(controls, XV_HEIGHT, new_height, NULL);
	}

#ifdef TRACE
	fprintf(stderr, "Window size before window_fit = %d x %d\n", 
		(int) xv_get(frame, XV_WIDTH), 
		(int) xv_get(frame, XV_HEIGHT));
	fprintf(stderr, "Changing dimensions: width by %d; height by %d\n", 
		new_width - orig_width, 
		new_height - orig_height);
#endif

	window_fit(frame); 

#ifdef TRACE
	fprintf(stderr, "Window size after window_fit = %d x %d\n", 
		(int) xv_get(frame, XV_WIDTH), 
		(int) xv_get(frame, XV_HEIGHT));
#endif

	xv_set(frame, XV_KEY_DATA, LAST_WIDTH, (int) xv_get(frame, XV_WIDTH), 
	       NULL);

	xv_set(frame, XV_KEY_DATA, LAST_HEIGHT, (int) xv_get(frame, XV_HEIGHT),
	       NULL);

	group_anchor(all_objects);
}

Group
which_group(Panel_item	list,
	    Group	top_group)
{
	Xv_opaque	*members;
	Group		current_group = top_group,
			member_group[MAX_GROUPS_PER_PANEL];
	int		ngroups = 0,
			i;

	members = (Xv_opaque *) xv_get(current_group, GROUP_MEMBERS);

	for ( ; (* members); members++)
	{
		switch ((Panel_item_type) xv_get((* members), PANEL_ITEM_CLASS))
		{

		case 0:

			member_group[ngroups++] = (* members);

			break;

		case PANEL_LIST_ITEM:

			if ((* members) == list)
				return(current_group);
		}
	}

	for (i = 0; i < ngroups; i++)
	{
		if (current_group = which_group(list, member_group[i]))
			return(current_group);
	}

	return((Group) 0);
}

boolean
scale_list(Panel owner,
	   Panel_item list,
	   SCALE_TYPE type)
{
	int	new_width = scale_list_width(owner, list, type),
		new_height = scale_list_height(owner, list, type);

	if (((int) xv_get(list, PANEL_LIST_WIDTH) != new_width)
	 || ((int) xv_get(list, PANEL_LIST_DISPLAY_ROWS) != new_height))
	{
		xv_set(list,
		       PANEL_LIST_WIDTH, new_width,
		       PANEL_LIST_DISPLAY_ROWS, new_height,
		       NULL);

		return(TRUE);
	}

	return(FALSE);
}

int
scale_list_height(Panel owner,
		  Panel_item list,
		  SCALE_TYPE type)
{
	float           ratio,
	                orig;

	int             cur_height = xv_get(owner, XV_HEIGHT),
	                result,
	                incr;

	if (type == SCALE_RELATIVE)
	{
		orig = (float) xv_get(owner, XV_KEY_DATA, ORIG_HEIGHT);

		if (!orig)
			ratio = 1;

		else
			ratio = (float) cur_height / orig;

		result = (int) ((float) xv_get(list, XV_KEY_DATA, ORIG_HEIGHT) * ratio);
	}

	else if (type == SCALE_ABSOLUTE)
	{
		incr = (int) (((float) cur_height
			  - (float) xv_get(owner, XV_KEY_DATA, LAST_HEIGHT))
			      / (float) xv_get(list, PANEL_LIST_ROW_HEIGHT));

		result = (int) xv_get(list, XV_KEY_DATA, LAST_HEIGHT) + incr;
	}

	else
	{
/*		sprintf(wms_err_msg, dgettext("wms_stubs_labels", "scale_list_height: Type = %d"), type);
		wms_error(E_ILL_OP, wms_err_msg);
*/
		result = (int) xv_get(list, PANEL_LIST_DISPLAY_ROWS);
	}

#ifdef TRACE
	fprintf(stderr, "scale_list_height: Scaling Height from %d (%d) to %d\n", 
		(int) xv_get(list, PANEL_LIST_DISPLAY_ROWS), 
		(int) xv_get(list, XV_KEY_DATA, LAST_HEIGHT), 
		result);
#endif

	xv_set(owner, XV_KEY_DATA,
	       LAST_HEIGHT, cur_height,
	       NULL);

	xv_set(list, XV_KEY_DATA,
	       LAST_HEIGHT, result,
	       NULL);

	if (result <= 0)
		result = 1;

	return (result);
}

int
scale_list_width(Panel owner,
		 Panel_item list,
		 SCALE_TYPE type)
{
	float           ratio,
	                orig;

	int             cur_width = xv_get(owner, XV_WIDTH),
	                result,
	                incr;

	if (type == SCALE_RELATIVE)
	{
		orig = (float) xv_get(owner, XV_KEY_DATA, ORIG_WIDTH);

		if (!orig)
			ratio = 1;

		else
			ratio = (float) cur_width / orig;

		result = (int) ((float) xv_get(list, XV_KEY_DATA, ORIG_WIDTH) * ratio);
	}

	else if (type == SCALE_ABSOLUTE)
	{
		incr = cur_width - (int) xv_get(owner, XV_KEY_DATA, LAST_WIDTH);

		result = (int) xv_get(list, XV_KEY_DATA, LAST_WIDTH) + incr;
	}

	else
	{
/*		sprintf(wms_err_msg, dgettext("wms_stubs_labels", "scale_list_width: Type = %d"), type);
		wms_error(E_ILL_OP, wms_err_msg);
*/
		result = (int) xv_get(list, PANEL_LIST_WIDTH);
	}

#ifdef TRACE
	fprintf(stderr, "scale_list_width: Scaling Width from %d (%d) to %d\n", 
		(int) xv_get(list, PANEL_LIST_WIDTH), 
		(int) xv_get(list, XV_KEY_DATA, LAST_WIDTH), 
		result);
#endif

	xv_set(owner, XV_KEY_DATA,
	       LAST_WIDTH, cur_width,
	       NULL);

	xv_set(list, XV_KEY_DATA,
	       LAST_WIDTH, result,
	       NULL);

	if (result <= 0)
		result = 50;

	return (result);
}

void
panel_resize(Panel	panel,
	     Group	top_group)
{
	Frame			frame = (Frame) xv_get(panel, XV_OWNER);
	int			save_width,
				new_width,
				save_height,
				new_height,
				nlists = 0,
				i;
	Panel_item		item,
				list[MAX_LISTS_PER_PANEL];
	boolean			horiz_lay = FALSE;
	static boolean		ignore = FALSE;
	static Xv_opaque	last_panel = 0;
	Group			group;
	
	if (!ignore || (panel != last_panel))
	{

		PANEL_EACH_ITEM(panel, item)
	
		switch ((Panel_item_type) xv_get(item, PANEL_ITEM_CLASS))
		{
	
		case PANEL_LIST_ITEM:
	
			if (nlists < MAX_LISTS_PER_PANEL)
			{
				list[nlists++] = item;
				group = which_group(item, top_group);

				if (group
				 && (xv_get(group, GROUP_TYPE) == GROUP_ROW)
				 && multi_list_group(item, group))
				{
					horiz_lay = TRUE;
				}
			}

			break;
		}
	
		PANEL_END_EACH
	
		group_layout(top_group);
		panel_paint(panel, PANEL_NO_CLEAR);
	
		new_width = (int) xv_get(frame, XV_WIDTH);
		save_width = (int) xv_get(frame, XV_KEY_DATA, LAST_WIDTH);
		if (horiz_lay)
		{

#ifdef TRACE
			fprintf(stderr, "Lists Are Arranged Horizontally\n");
#endif

			save_width += ((nlists -1 ) * (new_width - save_width) 
					/ nlists);
		}
	
		new_height = (int) xv_get(frame, XV_HEIGHT);
		save_height = (int) xv_get(frame, XV_KEY_DATA, LAST_HEIGHT);
		if (!horiz_lay && nlists)
		{

#ifdef TRACE
			fprintf(stderr, "Lists Are Arranged Vertically\n");
#endif

			save_height += ((nlists -1 ) * (new_height - save_height) 
					/ nlists);
		}
	
		xv_set(frame, XV_KEY_DATA, LAST_HEIGHT, save_height, NULL);
		xv_set(frame, XV_KEY_DATA, LAST_WIDTH, save_width, NULL);
	
		for (i = 0; i < nlists; i++)
		{
			if (scale_list(frame, list[i], SCALE_ABSOLUTE))
			{
				group_layout(which_group(list[i], top_group)); 
	
				if (i != (nlists - 1))
				{
					xv_set(frame, XV_KEY_DATA, LAST_WIDTH, save_width, 
					       NULL);
					xv_set(frame, XV_KEY_DATA, LAST_HEIGHT, save_height,
					       NULL);
				}
			}
		}

		ignore = TRUE;
	}

	else
	{
		ignore = FALSE;
	}

	last_panel = panel;

	group_layout(top_group);
	panel_paint(panel, PANEL_NO_CLEAR);
}

static boolean
multi_list_group(Panel_item	list,
	    	 Group		top_group)
{
	Xv_opaque	*members;

	members = (Xv_opaque *) xv_get(top_group, GROUP_MEMBERS);

	for ( ; (* members); members++)
	{
		switch ((Panel_item_type) xv_get((* members), PANEL_ITEM_CLASS))
		{

		case PANEL_LIST_ITEM:

			if ((* members) != list)
				return(TRUE);
		}
	}

	return(FALSE);
}

void
load_defaults(faxtool_prop_popup_objects	*ip,
	      FILE				*fd)
{
	char	item[NUMLEN + 1],
		value[TXTLEN + 1],
		buf[COMLEN + 1];

	fseek(fd, 0L, SEEK_SET);

	while((fgets(buf, COMLEN, fd) != (char *) NULL) || !feof(fd))
	{
		sscanf(buf, "%s %[^:]", item, value);

		if (value[strlen(value) - 1] == '\n')
		{
			value[strlen(value) - 1] = '\0';
		}
	
		if (strcmp(item, "FaxTool.FaxServer:") == 0)
		{
			xv_set(ip->fax_host, PANEL_VALUE, value, NULL);
			strncpy(def.server, value, NUMLEN);
		}
	
		else if (strcmp(item, "FaxTool.SpoolDir:") == 0)
		{
			xv_set(ip->fax_dir, PANEL_VALUE, value, NULL);
			strncpy(def.spool_dir, value, PATHLEN);
		}
	
		else if (strcmp(item, "FaxTool.ViewFilter:") == 0)
		{
			xv_set(ip->view_cmd, PANEL_VALUE, value, NULL);
			strncpy(def.view_cmd, value, TXTLEN);
		}
	
		else if (strcmp(item, "FaxTool.PrintFilter:") == 0)
		{
			xv_set(ip->print_cmd, PANEL_VALUE, value, NULL);
			strncpy(def.print_cmd, value, TXTLEN);
		}
		
		else if (strcmp(item, "FaxTool.PreviewFilter:") == 0)
		{
			xv_set(ip->preview_cmd, PANEL_VALUE, value, NULL);
			strncpy(def.preview_cmd, value, TXTLEN);
		}
	}
I 4

E 4
}

void
store_defaults(faxtool_prop_popup_objects	*ip,
	       FILE				*fd)
{
I 4
	char	*host;

	if (strcmp((char *) xv_get(ip->fax_host, PANEL_VALUE), "") == 0)
	{
		if ((host = getenv("FAXSERVER")) != (char *) NULL)
		{
			xv_set(ip->fax_host, 
				PANEL_VALUE, host, 
				NULL);
		}
	}

E 4
	fprintf(fd, "FaxTool.FaxServer:\t%s\n", 
		(char *) xv_get(ip->fax_host, PANEL_VALUE));

	fprintf(fd, "FaxTool.SpoolDir:\t%s\n", 
		(char *) xv_get(ip->fax_dir, PANEL_VALUE));

	fprintf(fd, "FaxTool.ViewFilter:\t%s\n", 
		(char *) xv_get(ip->view_cmd, PANEL_VALUE));

	fprintf(fd, "FaxTool.PrintFilter:\t%s\n", 
		(char *) xv_get(ip->print_cmd, PANEL_VALUE));

	fprintf(fd, "FaxTool.PreviewFilter:\t%s\n", 
		(char *) xv_get(ip->preview_cmd, PANEL_VALUE));
}

int
load_directory(Panel_item	list,
	       FILE		*fd)
{
	char	buf[COMLEN + 1],
		name[NUMLEN + 1],
		company[NUMLEN + 1],
		fax[NUMLEN + 1];
	int	nrows = 0;

	fseek(fd, 0L, SEEK_SET);

	while((fgets(buf, COMLEN, fd) != (char *) NULL) || !feof(fd))
	{
		if (strstr(buf, "FaxTool.") == (char *) NULL)
		{
			if (buf[strlen(buf) - 1] == '\n')
			{
				buf[strlen(buf) - 1] = '\0';
			}

			sscanf(buf, "%[^|]|%[^|]|%[^|]", name, company, fax);

			sprintf(buf, "%-25s%-25s%-25s", name, company, fax);

			xv_set(list, 
				PANEL_LIST_INSERT, nrows,
				PANEL_LIST_STRING, nrows++, buf,
				NULL);
		}
	}

	return(nrows);
}

int
store_directory(Panel_item	list,
	        FILE		*fd)
{
	char	*buf,
		name[NUMLEN + 1],
		company[NUMLEN + 1],
		fax[NUMLEN + 1];
	int	nrows = (int) xv_get(list, PANEL_LIST_NROWS),
		i;

	for (i = 0; i < nrows; i++)
	{
		buf = (char *) xv_get(list, PANEL_LIST_STRING, i);

		sscanf(buf, "%25c%25c%25c", name, company, fax);
		name[NUMLEN] = '\0';
		company[NUMLEN] = '\0';
		fax[NUMLEN] = '\0';

		fprintf(fd, "%s|%s|%s\n", name, company, fax);

	}

	return(nrows);
}

void
save_rc(void)
{
	FILE		*rc;
	char		rc_file[PATHLEN + 1];
	struct passwd	*pw = getpwuid(getuid());

	strncpy(rc_file, pw->pw_dir, PATHLEN);
	strncat(rc_file, "/.faxtoolrc", PATHLEN);

	if ((rc = fopen(rc_file, "w")) == (FILE *) NULL)
	{
		perror("faxtool: fopen: ");
		exit(-1);
	}

	store_defaults(Faxtool_prop_popup, rc);
	store_directory(Faxtool_directory_popup->directory_list, rc);

	fclose(rc);
}

void
trim_string(char	*string)
{
	int	i;

	for (i = strlen(string) - 1; i > 0; i--)
	{
		if ((string[i] != ' ') && (string[i] != '\n'))
		{
			string[i + 1] = '\0';
			return;
		}
	}
}
E 2
E 1
